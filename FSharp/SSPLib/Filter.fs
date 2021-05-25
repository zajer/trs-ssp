namespace SSPLib

module Filter =
    type filter = SS.situation -> bool
    let filterMostEngaging (minEngagement:double) (sit:SS.situation) =
        let _,maxEngagement = Array.maxBy (fun (_,t)-> t) sit.current_state
        let metric = Array.fold 
                            ( fun (m:double) (_,i2)-> m + ( (double) i2 / (double) maxEngagement ) ) 
                            0.0
                            sit.current_state 
        metric >= minEngagement
    let filterLimitedNumOfMostEngaging lim n minEngagement sit =
        if !n < lim then
            if filterMostEngaging minEngagement sit then
                n:=!n+1
                true
            else
                false
        else
            false
        