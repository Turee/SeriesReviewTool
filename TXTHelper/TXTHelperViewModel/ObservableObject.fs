namespace TXTHelper
open System
open System.ComponentModel
open Microsoft.FSharp.Quotations.Patterns

type ObservableObject() =
    let propertyChanged = Event<_,_>()
    let getPropertyName = function
        | PropertyGet(_, p, _) -> p.Name
        | _ -> invalidOp "Invalid expression argument: expecting property getter"

    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member this.PropertyChanged = propertyChanged.Publish

    member this.NotifyPropertyChanged name =
        propertyChanged.Trigger(this, PropertyChangedEventArgs(name))
    member this.NotifyPropertyChanged expr =
        expr |> getPropertyName |> this.NotifyPropertyChanged

