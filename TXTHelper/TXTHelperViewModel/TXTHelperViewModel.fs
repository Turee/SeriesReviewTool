namespace TXTHelper

open FSharpx
open FSharpx.Collections
open System
open Microsoft.WindowsAPICodePack.Dialogs;
open ExcelPackageF
open System.Windows.Input
open ExcelFiles

module Helpers=
    let createCommand action canExecute=
        let event1 = Event<_, _>()
        {
            new ICommand with
                member this.CanExecute(obj) = canExecute(obj)
                member this.Execute(obj) = action(obj)
                member this.add_CanExecuteChanged(handler) = event1.Publish.AddHandler(handler)
                member this.remove_CanExecuteChanged(handler) = event1.Publish.AddHandler(handler)
        }


    



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
            Seq.zip vm.seriesXData vm.seriesYData
            |> Seq.map (fun (x,y) ->
                sprintf "%s\t%s" (string x) (string y)
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
    let selectedExcelVM : ExcelFileVM option ref = ref None

    let updateSeriesCounts()=
        this.NotifyPropertyChanged "SeriesToReviewText"
        this.NotifyPropertyChanged "ReviewedSeriesText"

    let filterExelFiles()=
        exelFiles :=
            !exelFiles
            |> List.filter (fun (ef:ExcelFileVM) ->
                Seq.exists (fun tf -> tf.fileName = ef.TextFileName || tf.fileName = ef.DiscardedFileName) !textFiles |> not
            )
        this.NotifyPropertyChanged "ExcelFiles"
        updateSeriesCounts()

    let updatePlotModel(fileVm:ExcelFileVM)=
        
        let plotModel = 
            new OxyPlot.PlotModel()

        let series = new OxyPlot.Series.LineSeries(fileVm.DisplayString)
        
        Seq.zip fileVm.seriesXData fileVm.seriesYData
        |> Seq.iter (fun (x,y) ->
            let point = new OxyPlot.DataPoint(x,y)
            series.Points.Add point
        )
            
        plotModel.Series.Add series
        currentPlotModel := Some <| plotModel
        this.NotifyPropertyChanged "CurrentPlotModel"
        
    let setPlotModelNull() = currentPlotModel := None ; this.NotifyPropertyChanged "CurrentPlotModel"



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
                        parseFileXls file
                    |".xlsx" ->
                        parseFileXlsx file 
                    |_ -> Seq.empty
                with
                    |e ->
                        System.Windows.MessageBox.Show(sprintf "Excel file: %s was invalid!! \n Message: %s" (file.Name) e.Message) |> ignore
                        Seq.empty
        )
        |> Seq.concat
        |> fun files ->
            exelFiles := Seq.toList files

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

