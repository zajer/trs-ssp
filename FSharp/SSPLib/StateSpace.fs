namespace SSPLib

module StateSpace =
    type walk = State.transFun list
    type situation =  { currentSAT:State.sat; currentWalk:walk }
    
    let advanceSituation situation (transFun:State.transFun) =
        let newSAT = transFun.func situation.currentSAT 
        let newWalk = transFun :: situation.currentWalk
        { currentSAT=newSAT; currentWalk=newWalk }
    let initSituation sat = 
        { currentSAT=sat; currentWalk=[] }
    let (filteringFun:situation -> situation option) = fun sit -> if State.isNegligible sit.currentSAT then None else Some sit
        
    