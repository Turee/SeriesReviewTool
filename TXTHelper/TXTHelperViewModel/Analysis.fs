namespace TXTHelper
open System
open FSharpx
open FSharpx.Collections

module Analysis=
    



    let interpolateEvenly (xsYs:(float*float)[] )=
        let xs = Seq.map fst xsYs
        let ys = Seq.map snd xsYs
        
        let interpolation = new MathNet.Numerics.Interpolation.Algorithms.LinearSplineInterpolation(xs |> Seq.sort |> Seq.toArray,ys |> Seq.toArray)

        //ensure even sample rate
        let tmin = xsYs.[0] |> fst
        let tmax = xsYs.[xsYs.Length-1] |> fst

        let deltat = tmax - tmin
        let delta = deltat/(xsYs.Length |> double)
        
        seq {
            for x in tmin..delta..tmax do 
                yield (x,interpolation.Interpolate(x))
        }
        
    let hzToBpm i = i*60.0

    //In: (fst = t(s), snd = y value)
    //Out: (fst = f(bpm), snd = magnitude)
    let toFrequencyPlane (xsYs:(float*float)[]) =
        if Array.length xsYs < 2 then
            Array.empty
        else
           
            let tmin = xsYs.[0] |> fst
            let tmax = xsYs.[xsYs.Length-1] |> fst

            let xsys = interpolateEvenly xsYs

            let complexs = Seq.map (fun (_,y) -> new Numerics.Complex(y,0.0)) xsys |> Array.ofSeq
            let deltat = tmax - tmin
            let deltaF = 1.0/deltat
            MathNet.Numerics.IntegralTransforms.Transform.FourierForward(complexs)

            
            let pm = Seq.mapi (fun i (x:Numerics.Complex) -> (deltaF*(double i) |> hzToBpm ,x.Magnitude)) complexs |> Array.ofSeq
            pm 
            |> fun (x:(float*float)[]) -> x.[1..(x.Length/2+1)] //Remove DC and other half
            |> Array.filter (fun (x,_) -> 0.0 < x  && x < 300.0) //Include values only between 0.0-300 BPM
            |> Array.sortBy fst
    
    let getPulseAnnotations (bpm:float) (xsYs:(float*float)[])=
        if xsYs.Length < 2 then
            Array.empty
        else
            let dtSample = 
                ((fst (xsYs.[xsYs.Length - 1])) - (fst xsYs.[0]))
                / (xsYs.Length |> double)
            let dtPulse =  1.0/(bpm/60.0)

            let samplesPerPulse =  dtPulse / dtSample

            let toIndex (x:float) = (x |> int) % (xsYs.Length - 1)

            seq {
                for i in 0.0..1.0..samplesPerPulse do
                    yield
                        seq {
                            for j in 0.0..samplesPerPulse..(xsYs.Length-1 |> double) do
                                yield j+i
                        }
            }
            |> Seq.maxBy (fun xs -> 
                Seq.sumBy (fun (x:float) -> 
                    xsYs.[toIndex x] |> snd) xs
            )
            |> Seq.map (fun x -> xsYs.[toIndex x] |> fst)
            |> Array.ofSeq
                    

        
    