namespace SSPLib
module Filter =
    type metric = StateSpace.situation -> double
    type filter = StateSpace.situation -> bool
    let calcMetric (sit:StateSpace.situation) = 
        let _,maxEngagement = Array.maxBy (fun (_,t)-> t) sit.currentSAT
        Array.fold 
            ( fun (m:double) (_,i2)-> m + ( (double) i2 / (double) maxEngagement ) ) 
            0.0
            sit.currentSAT
    let filterMostEngaging (minEngagement:double) (sit:StateSpace.situation) =
        let metric = calcMetric sit
        let metricRounded = System.Math.Round (metric, 2) 
        metricRounded >= minEngagement
    let filterLimitedNumOfMostEngaging lim n minEngagement sit =
        if !n < lim then
            if filterMostEngaging minEngagement sit then
                n:=!n+1
                true
            else
                false
        else
            false
