namespace TXTHelper
open System
open FSharpx
open FSharpx.Collections
open System.IO

type TextFileInfo =
    {
        averageX : double
        name : string
        xsYs : (double*double)[]
    }

type ToolsViewModel() as this=
    inherit ObservableObject()

    let multiplierValue = ref 1000.0

    let textFileInfos = ref List.empty

    let parseLines (lines:string[]) =
        lines
        |> Array.choose (fun line -> 
            match line.Split('\t') |> List.ofSeq with
            |[x;y] ->
                try
                    let x = Double.Parse(x,Globalization.CultureInfo.InvariantCulture)
                    let y = Double.Parse(y,Globalization.CultureInfo.InvariantCulture)
                    Some (x,y)
                with
                    |e -> None
            |_ -> None
        )
        |> fun parsed -> if Array.isEmpty parsed then None else Some parsed

    let filterLessThan = ref 1000.0
    let selectedPath = ref ""

    let updateFileList(path)=
        let dirInfo = new DirectoryInfo(path)

        let txtInfos =
            Seq.append (dirInfo.EnumerateFiles("*.DISCARDED",SearchOption.AllDirectories)) (dirInfo.EnumerateFiles("*.txt",SearchOption.AllDirectories))
            |> Array.ofSeq
            |> Array.map (fun path -> path.FullName,File.ReadAllLines path.FullName)
            |> Array.choose (fun (path,lines) -> parseLines lines  |> Option.bind (fun r -> Some <| (path,r)) )
            |> Array.map (fun (name,xsys) ->
                {
                    name = name
                    averageX = xsys |> Seq.averageBy fst
                    xsYs = xsys
                }
            )
            |> Seq.filter (fun info -> info.averageX < !filterLessThan)
            |> List.ofSeq
        textFileInfos := txtInfos
        this.NotifyPropertyChanged "TxtFileInfos"



    member this.FilterLessThan
        with get()= !filterLessThan
        and set(v) = 
            filterLessThan := v
            if !selectedPath <> "" then
                updateFileList(!selectedPath)

    member this.MultiplierValue
        with get () = !multiplierValue
        and set(v) = multiplierValue := v

    member this.TxtFileInfos= !textFileInfos

    member this.SelectTxtFileFolderCommand=
        
        Helpers.createCommand (fun _ -> 
                let dialog = new Ookii.Dialogs.Wpf.VistaFolderBrowserDialog()

                dialog.Description <- "Select txt file folder"
                if dialog.ShowDialog() = Nullable.create true then
            
                    let path = dialog.SelectedPath
                    selectedPath := path
                    updateFileList(path)
                    
                else
                    ()
            )
            (fun _ -> true)

    member this.ExecuteMultiplyCommand =
        Helpers.createCommand (fun _ ->
                
                !textFileInfos
                |> List.iter (fun textFileInfo ->
                        let content = 
                            textFileInfo.xsYs
                            |> Analysis.interpolateEvenly
                            |> Seq.map (fun (x,y) ->
                                let x = x*(!multiplierValue)
                                sprintf "%f\t%f" x y
                            )
                        File.WriteAllLines(textFileInfo.name,content)
                    )

                updateFileList(!selectedPath)
            )
            (fun _ ->  true)