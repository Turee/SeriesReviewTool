namespace TXTHelper
open System
open FSharpx
open FSharpx.Collections

module NeuralNetwork=


            // Get 5 max peaks amplitude and their width, sorted by frequency

    let rec getPeakWidths (peaks:int) (xsYs:(float*float)[]) (freqAmpW:(float*float*float) list) =
        if peaks =  0 || Array.length xsYs < 3 then
            freqAmpW
        else
            let peakIndex = 
                if Array.isEmpty xsYs then None 
                else 
                    Array.maxBy snd xsYs 
                    |> snd 
                    |> fun max -> Array.tryFindIndex (fun (_,y) -> y = max) xsYs
            match peakIndex with
            |Some i ->
                let v = xsYs.[i] |> snd
                let getNextRising theSeq = 
                    theSeq
                    |> Seq.map (fun i -> (xsYs.[i],i) )
                    |> Seq.pairwise
                    |> Seq.map (fun (((x1,y1),i1),((_,y2),i2)) -> 
                        let deltay = y2 - y1
                        (x1,deltay,i2)
                        )
                    |> Seq.tryFind (fun (_,dy,_) -> dy > 0.0)
                    |> Option.bind (fun (x1,_,i2) -> (x1,i2) |> Some)
                    
                let fRight,iRight =
                    (getNextRising <| seq {i..(Array.length xsYs - 1)} )
                    |> Option.getOrElse ((xsYs.[Array.length xsYs - 1] |> fst,Array.length xsYs - 1) )
                let fLeft,iLeft =
                    (getNextRising <| seq {i..0} )
                    |> Option.getOrElse (fst xsYs.[0],0)
                    
                let peakWidth = fRight - fLeft
                let freq = xsYs.[i] |> fst
                let amplitude = xsYs.[i] |> snd

                let ensureMinLength s = if Seq.length s = 1 then Seq.empty else s
                let newXsYs = 
                    Seq.append (seq {0..iLeft} |> ensureMinLength) (seq {iRight..Array.length xsYs - 1} |> ensureMinLength) |> Seq.map (fun i -> xsYs.[i]) |> Array.ofSeq

                getPeakWidths (peaks-1) newXsYs ((freq,amplitude,peakWidth)::freqAmpW)
                
            |_ -> freqAmpW
    
    type NeuralNetworkInput=
        {
            input : float[]
        }
        static member fromFFTSeries (xsYs:(float*float)[])=
            

            let widths = getPeakWidths 5 xsYs List.empty |> List.rev
            //[frequency,amplitude,width]

//            let getPeaksFAW (peakCount:int) (freqAmpW:(float*float*float)[]) =
//                if peakCount = 0 then
//                    freqAmpW
//                else
            Seq.map (fun (a,b,c) -> seq [a;b;c]) widths
            |> Seq.concat
            |> Array.ofSeq

    let adsf (input)=
        let activationFunction = new AForge.Neuro.BipolarSigmoidFunction()
        let an = new AForge.Neuro.ActivationNetwork(activationFunction,15,[|100;200;1|])
        an.Randomize()
        let learning =  new AForge.Neuro.Learning.BackPropagationLearning(an)
        ()

