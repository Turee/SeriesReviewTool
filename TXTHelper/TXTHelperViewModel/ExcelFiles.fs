namespace TXTHelper
open System
open ExcelPackageF
open FSharpx
open FSharpx.Collections

module ExcelFiles=
    type ExcelFileVM=
        {
            filePath : string
            file : System.IO.FileInfo
            seriesName : string
        
            seriesXsYs : (double*double) array
        }

        member this.DisplayString=
            sprintf "%s - %s" (this.file.Name) (this.seriesName)

        member this.TextFileName =
             sprintf "%s_%s.txt"  this.file.Name this.seriesName |> fun x -> x.Replace(" ","_")
        member this.DiscardedFileName =
            sprintf "%s_%s.txt.discarded"  this.file.Name this.seriesName  |> fun x -> x.Replace(" ","_")
    

    let parseFileXlsx (file:IO.FileInfo)=
            //let fileWithPath = System.IO.Path.Combine(path,file.Name)

            let wsheet = ExcelPackageF.Excel.getWorksheetByIndex 1 file.FullName

            //Timeseries y values
            let xdata =
                seq {
                    for row in (Excel.getColumn 1 wsheet) |> Seq.skip 1 |> Seq.takeWhile (String.IsNullOrEmpty >> not) do
                        yield (Double.parse(row.Replace(",",".")))

                }
                |> Seq.map (Option.getOrElse Double.NaN)
                |> Array.ofSeq

            //Column indexes that contain data and their header values
            let colsWithDataAndHeaders = 
                Excel.getRow 1 wsheet
                |> Seq.skip 1
                |> Seq.takeWhile (fun x -> x <> null && x <> "")
                |> Seq.mapi (fun i header -> (i + 2,header))
                |> List.ofSeq

            let yDatas =
                seq {
                    for (coli,header) in colsWithDataAndHeaders do
                        let ydata = 
                            Excel.getColumn coli wsheet
                            |> Seq.skip 1
                            |> Seq.takeWhile (String.IsNullOrEmpty >> not)
                            |> Seq.map (fun v ->
                                Double.parse (v.Replace(",","."))
                            )
                            |> Seq.map (Option.getOrElse Double.NaN)
                        yield(header,ydata |> Array.ofSeq)
                }
            seq {
                for (header,ydata) in yDatas do
                    yield
                        {
                            ExcelFileVM.filePath = file.FullName
                            ExcelFileVM.file = file
                            seriesName = header
                            seriesXsYs = Array.zip xdata ydata
                         }
            }

    let toFrequencyPlane (xsYs:(float*float)[]) =
        if Array.length xsYs < 2 then
            Array.empty
        else
            let xs = Seq.map fst xsYs
            let ys = Seq.map snd xsYs
        
            let interpolation = new MathNet.Numerics.Interpolation.Algorithms.LinearSplineInterpolation(xs |> Seq.sort |> Seq.toArray,ys |> Seq.toArray)

            //ensure even sample rate
            let tmin = xsYs.[0] |> fst
            let tmax = xsYs.[xsYs.Length-1] |> fst

            let deltat = tmax - tmin
            let delta = deltat/(xsYs.Length |> double)
            let xsys= 
                seq {
                    for x in tmin..delta..tmax do 
                        yield (x,interpolation.Interpolate(x))
                }

            
            let complexs = Seq.map (fun (_,y) -> new Numerics.Complex(y,0.0)) xsys |> Array.ofSeq
        
            let deltaF = 1.0/deltat
            MathNet.Numerics.IntegralTransforms.Transform.FourierForward(complexs)

            let toBpm i = i*60.0
            let pm = Seq.mapi (fun i (x:Numerics.Complex) -> (deltaF*(double i) |> toBpm ,x.Magnitude)) complexs |> Array.ofSeq
            pm 
            |> fun (x:(float*float)[]) -> x.[0..(x.Length/2+1)]
            |> Array.filter (fun (x,_) -> 0.0 < x  && x < 300.0)
            |> Array.sortBy fst

    let parseFileXls (file:IO.FileInfo) =
        let workBook = ExcelLibrary.SpreadSheet.Workbook.Load(file.FullName)
        let wsheet = workBook.Worksheets.[1]

        let datas=
            wsheet.Cells.Rows
            |> Seq.map (fun row ->
                [0..row.Value.LastColIndex] 
                |> Seq.map (fun i -> row.Value.GetCell(i).StringValue.Replace(',','.') |> Double.parse) 
                |> Seq.map (Option.getOrElse Double.NaN)  
                |> Array.ofSeq     
            )
            |> Array.ofSeq
        let scale = 1.0/1000.0
        let xdata = Seq.map (fun (row:float[]) -> row.[0] |> (*) scale ) datas 
        let yDatas = 
            [1..(datas.[0].Length-1)]
            |> Seq.map (fun i -> 
                datas
                |> Seq.map (fun (row:float[]) -> row.[i] |> (*) scale)  
            ) 
        
        yDatas
        |> Seq.mapi (fun i ydata ->
            let header = sprintf "ROI_%i" i
            {
                ExcelFileVM.filePath = file.FullName
                ExcelFileVM.file = file
                seriesName = header
                seriesXsYs = Seq.zip xdata ydata |> Array.ofSeq
            }
        )
        