namespace TXTHelper

open FSharpx
open FSharpx.Collections
open System
open Microsoft.WindowsAPICodePack.Dialogs;
open ExcelPackageF
open System.Windows.Input
open OxyPlot.Annotations
open ExcelFiles



type TXTFileVM =
    {
        fileName : string
        isDiscarded : bool
    }

    member this.DisplayString = this.fileName
    member this.Background = 
        let color = 
            if this.isDiscarded then
                System.Windows.Media.Color.FromArgb(127uy,255uy,0uy,0uy)
            else
                System.Windows.Media.Color.FromArgb(127uy,0uy,255uy,0uy)
        new System.Windows.Media.SolidColorBrush(color)

    static member CreateAndSave (path:string) (vm:ExcelFileVM) (discard:bool) =

        let fileName = 
            if discard then
                vm.DiscardedFileName
            else
                vm.TextFileName

        let withpath = 
                System.IO.Path.Combine(path,fileName)

        use file = new System.IO.StreamWriter(withpath)
        let str = 
            vm.seriesXsYs
            |> Seq.map (fun (x,y) ->
                sprintf "%s\t%s" (string (x*1000.0)) (string y)
            )
            |> fun lines -> System.String.Join(System.Environment.NewLine,lines)
        file.Write(str)
        file.Flush()

        
        {
            fileName = fileName
            isDiscarded = discard
        }


type TXTHelperViewModel() as this=
    inherit ObservableObject()

    let inPath = ref ""
    let outPath = ref ""

    let exelFiles = ref List.empty
    let textFiles = ref List.empty
    let currentPlotModel = ref None
    let currentPlotModelHz = ref None
    let selectedExcelVM : ExcelFileVM option ref = ref None

    let updateSeriesCounts()=
        this.NotifyPropertyChanged "SeriesToReviewText"
        this.NotifyPropertyChanged "ReviewedSeriesText"

    let setPlotModelNull() = currentPlotModel := None ; this.NotifyPropertyChanged "CurrentPlotModel"
    let setHzPlotModelNull() = currentPlotModelHz := None ; this.NotifyPropertyChanged "CurrentPlotModelHz"

    let updatePlotModelHz (freqData:(double*double) array) (bpm:double)  =
        
        let plotModelHz = new OxyPlot.PlotModel()
        let series = new OxyPlot.Series.LineSeries()

        if Seq.isEmpty freqData |> not then

            plotModelHz.Series.Add series
            currentPlotModelHz := Some <| plotModelHz

            series.Color <- OxyPlot.OxyColor.FromArgb(200uy,0uy,0uy,0uy)
            freqData
            |> Seq.iter (fun (x,y) ->
                let point = new OxyPlot.DataPoint(x,y)
                series.Points.Add point
            )
            let bpmAnnotation = new OxyPlot.Annotations.LineAnnotation()
            bpmAnnotation.Type <- OxyPlot.Annotations.LineAnnotationType.Vertical
            bpmAnnotation.X <- bpm
            bpmAnnotation.Color <- OxyPlot.OxyColor.FromRgb(250uy,0uy,0uy)
            //bpmAnnotation.TextLinePosition <- 0.5
            bpmAnnotation.Text <- (sprintf "%s BPM" (string <| Math.Round(bpm,0)))
            bpmAnnotation.FontWeight <- OxyPlot.FontWeights.Bold
            bpmAnnotation.FontSize <- 18.0
            bpmAnnotation.TextOrientation <- OxyPlot.Annotations.AnnotationTextOrientation.Vertical

            plotModelHz.Annotations.Add bpmAnnotation
            plotModelHz.InvalidatePlot(true)
        else
            setHzPlotModelNull()

    let insertPulseAnnotations (bpm:float) (fileVm:ExcelFileVM)=
        match !currentPlotModel with
        |Some (plotmodel:OxyPlot.PlotModel) ->

            let annotations = Analysis.getPulseAnnotations bpm fileVm.seriesXsYs 
            
            for ann in annotations do
                let pulseAnnotation = new OxyPlot.Annotations.LineAnnotation()
                pulseAnnotation.Type <- OxyPlot.Annotations.LineAnnotationType.Vertical
                pulseAnnotation.X <- ann
                pulseAnnotation.Color <- OxyPlot.OxyColor.FromArgb(127uy,250uy,0uy,0uy)
                plotmodel.Annotations.Add(pulseAnnotation)
            plotmodel.InvalidatePlot(true)
        |None -> ()

    let updateFrequencyChartAndAnnotations (xmin:float) (xmax:float) (fileVm:ExcelFileVM)=
        let dataFiltered = (fileVm.seriesXsYs |> Array.filter (fun (x,y) ->  xmin <= x && x <= xmax) ) 
        let freqData = Analysis.toFrequencyPlane dataFiltered
        if Seq.isEmpty freqData |> not then
            let bpm = Seq.maxBy snd freqData |> fst
            Option.iter (fun (pm:OxyPlot.PlotModel) -> pm.Annotations.Clear()) !currentPlotModel
            updatePlotModelHz freqData bpm
            insertPulseAnnotations bpm fileVm

            this.NotifyPropertyChanged "CurrentPlotModelHz"
            this.NotifyPropertyChanged "CurrentPlotModel"
        else
            ()

    let selectRangeHandler (model:OxyPlot.PlotModel) (file:ExcelFileVM) =
        
        let sxmax = (Seq.maxBy fst file.seriesXsYs) |> fst
        let sxmin = Seq.minBy fst file.seriesXsYs |> fst
        
        let addRectangleAnnotation()=
            let ra = new RectangleAnnotation()
            ra.MinimumX <-  0.0
            ra.MaximumX <-  0.0
            ra.Fill <- OxyPlot.OxyColor.FromArgb(90uy,0x66uy,0xCCuy,0xCCuy)
            model.Annotations.Add(ra)
            ra.FontSize <- 16.0
            ra.FontWeight <- OxyPlot.FontWeights.Bold
            model.InvalidatePlot(true)
            ra
        let rectangleAnnotation = ref <| addRectangleAnnotation()  
        
        let startx = ref Double.NaN


        model.MouseDown.Add (fun (ea) ->    
            if (ea.ChangedButton = OxyPlot.OxyMouseButton.Left) then
                if Seq.exists (fun ann -> ann = (!rectangleAnnotation :> OxyPlot.Annotations.Annotation)) model.Annotations |> not then
                    rectangleAnnotation := addRectangleAnnotation()
                let r = !rectangleAnnotation
                startx := r.InverseTransform(ea.Position).X
                r.MinimumX <- !startx
                r.MaximumX <- !startx
                model.InvalidatePlot(true)
                ea.Handled <- true
            else ()

        )

        model.MouseMove.Add (fun ea ->
            match !startx with
            |sx when not <| Double.IsNaN sx ->
                let r = !rectangleAnnotation
                let x = r.InverseTransform(ea.Position).X;
                r.MinimumX <- Math.Min(x,sx)
                r.MaximumX <- Math.Max(x,sx)
                (!rectangleAnnotation).Text <- (sprintf "FFT Window [%fs .. %fs]" r.MinimumX r.MaximumX)
                
                model.InvalidatePlot(true)
                ea.Handled <- true
                
            |_ -> ()
        )
        model.MouseUp.Add (fun ea ->
            let range = !rectangleAnnotation
            startx := Double.NaN
            range.Text <- ""
            if range.MaximumX - range.MinimumX = 0.0 then
                updateFrequencyChartAndAnnotations sxmin sxmax file
            else
                updateFrequencyChartAndAnnotations range.MinimumX range.MaximumX file
                
        )

    let filterExelFiles()=
        exelFiles :=
            !exelFiles
            |> List.filter (fun (ef:ExcelFileVM) ->
                Seq.exists (fun tf -> tf.fileName = ef.TextFileName || tf.fileName = ef.DiscardedFileName) !textFiles |> not
            )
        this.NotifyPropertyChanged "ExcelFiles"
        updateSeriesCounts()

   

    let updatePlotModel(fileVm:ExcelFileVM)=
        
        if Seq.isEmpty fileVm.seriesXsYs then
            setPlotModelNull()
        else
            let plotModel = 
                new OxyPlot.PlotModel()
            selectRangeHandler (plotModel) fileVm

            let series = new OxyPlot.Series.LineSeries(fileVm.DisplayString)
            series.Color <- OxyPlot.OxyColor.FromArgb(200uy,0uy,0uy,0uy)

            fileVm.seriesXsYs
            |> Seq.sortBy fst
            |> Seq.iter (fun (x,y) ->
                let point = new OxyPlot.DataPoint(x,y)
                series.Points.Add point
            )
            
            plotModel.Series.Add series
            currentPlotModel := Some <| plotModel
            this.NotifyPropertyChanged "CurrentPlotModel"
            updateFrequencyChartAndAnnotations (Seq.minBy fst fileVm.seriesXsYs |> fst) (Seq.maxBy fst fileVm.seriesXsYs |> fst) fileVm 



    let loadExcelFiles (path:string)=
        inPath := path
        this.NotifyPropertyChanged "RefreshExcelDirectoryCommand"
        let di = new System.IO.DirectoryInfo(path)
        let files = Array.append (di.GetFiles("*.xlsx")) (di.GetFiles("*.xls"))

        files
        |> Seq.map (fun file -> 
                try 
                    match file.Extension with
                    |".xls" ->
                        parseFileXls file |> List.ofSeq
                    |".xlsx" ->
                        parseFileXlsx file  |> List.ofSeq
                    |_ -> List.empty
                with
                    |e ->
                        System.Windows.MessageBox.Show(sprintf "Excel file: %s was invalid!! \n Message: %s" (file.Name) e.Message) |> ignore
                        List.empty
        )
        |> List.concat
        |> fun files ->
            exelFiles := files

        this.NotifyPropertyChanged "InPath"
        filterExelFiles()


    let loadTXTFiles (path:string)=
        outPath := path
        this.NotifyPropertyChanged "RefreshTextDirectoryCommand"
    
        let di = new System.IO.DirectoryInfo(path)
        let files = Seq.concat [di.GetFiles("*.txt");di.GetFiles("*.discarded")]
    
        files
        |> Seq.sortBy (fun fi -> -fi.CreationTime.Ticks)
        |> Seq.map (fun f ->
            {
                TXTFileVM.fileName = f.Name
                isDiscarded = f.Name.EndsWith("discarded")
            }
        )
        |> Seq.toList
        |> fun x ->
            textFiles := x
        
        this.NotifyPropertyChanged "OutPath"
        this.NotifyPropertyChanged "TextFiles"
        filterExelFiles()
        updateSeriesCounts()

    let selectDirectoryCommand desc (onDirectorySelected : string -> unit) =
        let action =
            fun _ ->
                let dialog = new Ookii.Dialogs.Wpf.VistaFolderBrowserDialog()
                dialog.ShowNewFolderButton <- true
                let result = dialog.ShowDialog()
                dialog.Description <- desc
                if result.HasValue && result.Value then
                    onDirectorySelected dialog.SelectedPath
                    
        let canexecute _ = true

        Helpers.createCommand action canexecute

        
    member this.CurrentPlotModel
        with get() = 
            match !currentPlotModel with |Some pm -> pm :> obj |_ -> null

    member this.CurrentPlotModelHz
        with get() = 
            match !currentPlotModelHz with |Some pm -> pm :> obj |_ -> null

    member this.ExcelFiles
        with get()= !exelFiles

    member this.TextFiles
        with get()= !textFiles

    member this.SelectedExcelFile
        with get()= !selectedExcelVM |> Option.getOrElseWith null (fun x -> x :> obj)
        and set (v:obj) = 
            match v with
            | :? ExcelFileVM as vm ->
                selectedExcelVM := Some vm
                updatePlotModel(vm)
            |_ -> 
                selectedExcelVM := None
                setPlotModelNull()
            this.NotifyPropertyChanged "AcceptSeriesCommand"
            this.NotifyPropertyChanged "DiscardSeriesCommand"

    member this.SelectedFileIndex 
        with get()=
            match !selectedExcelVM with
            |Some ef -> 
                List.tryFindIndex (fun f -> f = ef) !exelFiles |> Option.getOrElse 0
            |None -> 0

    member this.InPath 
        with get() = !inPath

    member this.OutPath
        with get()= !outPath



    member this.SelectExcelDirectoryCommand
        with get()= 
            selectDirectoryCommand "Select Excel file directory" loadExcelFiles
            

    member this.SelecteTextDirectoryCommand
        with get()= 
            selectDirectoryCommand "Select text file directory" loadTXTFiles
            

    member this.AcceptSeriesCommand
        with get()=

            let canExecute _ = selectedExcelVM.Value.IsSome
            let action _ =
                match !selectedExcelVM with
                |Some ef ->
                    exelFiles := !exelFiles |> List.filter (fun f -> f <> ef)
                    this.NotifyPropertyChanged "ExcelFiles"
                    let txtFile = TXTFileVM.CreateAndSave outPath.Value ef false
                    textFiles := txtFile::(!textFiles |> List.filter (fun f -> f <> txtFile)) 
                    this.NotifyPropertyChanged "TextFiles"
                    
                    selectedExcelVM :=
                        match !exelFiles with
                        |h::t -> Some h 
                        |_ -> None
                    this.NotifyPropertyChanged "SelectedFileIndex"
                    updateSeriesCounts()
                |_ -> ()
            Helpers.createCommand action canExecute


    member this.DiscardSeriesCommand
        with get()=
            let canExecute _ = selectedExcelVM.Value.IsSome
            let action _ =
                match !selectedExcelVM with
                |Some ef ->
                    exelFiles := !exelFiles |> List.filter (fun f -> f <> ef)
                    this.NotifyPropertyChanged "ExcelFiles"
                    let txtFile = TXTFileVM.CreateAndSave outPath.Value ef true
                    textFiles := txtFile::(!textFiles |> List.filter (fun f -> f <> txtFile)) 
                    this.NotifyPropertyChanged "TextFiles"
                    
                    selectedExcelVM :=
                        match !exelFiles with
                        |h::t -> Some h 
                        |_ -> None
                    this.NotifyPropertyChanged "SelectedFileIndex"
                    updateSeriesCounts()
                |None -> ()
            Helpers.createCommand action canExecute


    member this.SeriesToReviewText
        with get()=
            sprintf "%i Series To Review" (!exelFiles |> Seq.length)

    member this.ReviewedSeriesText
        with get()=
            sprintf "%i Accepted / %i Discarded" (!textFiles |> Seq.filter (fun tf -> tf.isDiscarded |> not) |> Seq.length) (!textFiles |> Seq.filter (fun tf -> tf.isDiscarded) |> Seq.length)
    
    member this.RefreshTextDirectoryCommand
        with get()=
            Helpers.createCommand
                (fun _ ->
                    if (!inPath <> "") then
                         loadExcelFiles (!inPath)
                    loadTXTFiles (!outPath)
                )
                (fun _ ->
                    !outPath <> ""
                )

    member this.RefreshExcelDirectoryCommand
        with get()=
            Helpers.createCommand
                (fun _ ->
                    loadExcelFiles (!inPath)
                )
                (fun _ ->
                    !inPath <> ""
                )
    //RefreshTextDirectoryCommand
    //RefreshExcelDirectoryCommand

//         Helpers
//            let canExecute = selectedExcelVM.Value.IsSome && outPath <>
//
//            let action = 
//                fun _ ->
//                    

