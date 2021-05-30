

open SSPApp.Config

let saveResults resultWalks importedTransFuns outputFilePrefix =
    let walksAsArray = Array.ofSeq resultWalks
    printfn "Saving result to files with prefix: %s" outputFilePrefix
    Array.Parallel.iteri 
        (
            fun i w -> 
                    let tf = SSPLib.Frontend.exportWalk w importedTransFuns
                    SSPLib.Data.exportTransFuns tf ( outputFilePrefix+"_"+i.ToString()+".csv")
        )
        walksAsArray
let printSATs numOfAgents (resultWalks:seq<SSPLib.StateSpace.walk>) =
    let numOfResults = Seq.length resultWalks
    let walksAsSATs = Seq.map 
                        (fun w -> 
                            List.fold 
                                (fun satList (tf:SSPLib.State.transFun) ->  
                                    let newSat = (tf.func (List.head satList))
                                    newSat :: satList
                                ) 
                                [(Array.init numOfAgents (fun i -> (i+1,0)))]
                                (w |> List.rev)
                        ) 
                        resultWalks
    Seq.iteri (fun i walkAsSATs -> 
                            let walkInProperOrder = List.rev walkAsSATs
                            let walkAsString = List.map (fun sat -> SSPLib.State.toStirng sat) walkInProperOrder |> String.concat "->\n"
                            printfn "Result %d: Consecutive SATs:\n%s" (numOfResults-i) walkAsString
            )
            walksAsSATs
let performFindFirstRoutine (conf:config) (opd:operationalData) saveResult= 
    let situationsMatrix,_,is_found = 
        match conf.resultStrategy with
        | All -> SSPLib.Frontend.searchForSituationInState opd.initialSituationMatrix opd.transitionMatrix opd.destinationStateIndex conf.numOfSteps
        | First -> 
                    let transformer = fun x -> x
                    SSPLib.Frontend.searchForSituationInStateLimited opd.initialSituationMatrix opd.transitionMatrix opd.destinationStateIndex conf.numOfSteps transformer 1
        (*| Bests (filterFun,n) ->
                                let filter = Seq.filter filterFun 
                                SSPLib.Frontend.searchForSituationInStateLimited opd.initialSituationMatrix opd.transitionMatrix opd.destinationStateIndex conf.numOfSteps filter n*)
        | Bests (metric,n) ->
                                let filter2 = Seq.sortByDescending metric
                                let filter seq = 
                                            let sorted = Seq.sortByDescending metric seq
                                            Seq.truncate n sorted
                                SSPLib.Frontend.searchForSituationInStateLimited opd.initialSituationMatrix opd.transitionMatrix opd.destinationStateIndex conf.numOfSteps filter n
    if is_found then
        printfn "%s" ("Walk to the desired state with id="+ opd.destinationStateIndex.ToString()+" found")
        let resultWalks = SSPLib.Frontend.getWalkFromSituationMatrix situationsMatrix opd.destinationStateIndex
        printfn "Found %d unique walks" (Seq.length resultWalks)
        if conf.printSATs then
            printSATs conf.numOfAgents resultWalks
        if saveResult then
            saveResults resultWalks opd.allImportedTransFuncs conf.outputFilePrefix
    else
        printfn "%s" ("Walk to the desired state has not been found!")
let performSearchUntilRoutine (conf:config) (opd:operationalData) saveResult =
    let resultWalks,is_found = 
        match conf.resultStrategy with
        | All -> SSPLib.Frontend.searchForWalksLeadingToState opd.initialSituationMatrix opd.transitionMatrix opd.destinationStateIndex conf.numOfSteps conf.forceNoIdling
        | First -> 
                    let filter = fun x -> x
                    SSPLib.Frontend.searchForWalksLeadingToStateLimited 
                        opd.initialSituationMatrix 
                        opd.transitionMatrix 
                        opd.destinationStateIndex 
                        conf.numOfSteps 
                        filter 
                        1 
                        conf.forceNoIdling
        (*| Bests (filterFun,n) ->
                                let filter = Seq.filter filterFun
                                SSPLib.Frontend.searchForWalksLeadingToStateLimited 
                                    opd.initialSituationMatrix 
                                    opd.transitionMatrix 
                                    opd.destinationStateIndex 
                                    conf.numOfSteps 
                                    filter
                                    n
                                    conf.forceNoIdling;*)
        | Bests (metric,n) -> 
                                        let filter seq = 
                                            let sorted = Seq.sortByDescending metric seq
                                            Seq.truncate n sorted
                                        SSPLib.Frontend.searchForWalksLeadingToStateLimited 
                                            opd.initialSituationMatrix 
                                            opd.transitionMatrix 
                                            opd.destinationStateIndex 
                                            conf.numOfSteps 
                                            filter 
                                            n
                                            conf.forceNoIdling
        //| SSPLib.Frontend.ComputeAll -> SSPLib.Frontend.searchForWalksLeadingToState opd.initialSituationMatrix opd.transitionMatrix opd.destinationStateIndex conf.numOfSteps conf.resultStrategy conf.forceNoIdling
        //| SSPLib.Frontend.ComputeLimited limit-> SSPLib.Frontend.searchForWalksLeadingToStateLimited opd.initialSituationMatrix opd.transitionMatrix opd.destinationStateIndex conf.numOfSteps conf.resultStrategy limit conf.forceNoIdling
    if is_found then
        printfn "%s" ("Walk to the desired state with id="+ opd.destinationStateIndex.ToString()+" found")
        printfn "Found %d unique walks" (Seq.length resultWalks)
        if conf.printSATs then
            printSATs conf.numOfAgents resultWalks
        if saveResult then
            saveResults resultWalks opd.allImportedTransFuncs conf.outputFilePrefix
    else
        printfn "%s" ("Walk to the desired state has not been found!")
let config2OperationalData conf =
    let importedTransFuns = SSPLib.Data.importTransFuns conf.transitionFunctionsFile
    let destStates = SSPLib.Data.importDestStates conf.destinationStatesFile
    let destStateIdx = SSPLib.Frontend.chooseDestinationStateIdx conf.destinationStrategy destStates
    let initSituation = Array.init conf.numOfAgents (fun i -> i+1,0) |> SSPLib.StateSpace.initSituation 
    let initSituationMatrix,transitionMatrix = SSPLib.Frontend.makeSystem importedTransFuns initSituation 0 (SSPLib.State.numberOfStates ())
    {
        allImportedTransFuncs=importedTransFuns;
        transitionMatrix=transitionMatrix;
        initialSituationMatrix=initSituationMatrix
        destinationStateIndex=destStateIdx
    }
[<EntryPoint>]
let main argv =
    if Array.length argv <> requiredNumberOfArgs then
        printHelp ()
    else
        let mode,file = parseInputArgs argv
        match mode with
        | GenerateWalks ->
            let config = parseConfigFromJson file
            SSPLib.State.setValues true config.numOfAgents config.numOfStates
            let opd = config2OperationalData config
            match config.task with
            | FirstFound -> performFindFirstRoutine config opd true
            | SearchUntil -> performSearchUntilRoutine config opd true
            | FirstFoundCount -> performFindFirstRoutine config opd false
            | SearchUntilCount -> performSearchUntilRoutine config opd false
        | Example -> 
            let exampleConfig = ConfigProvider.GetSample ()
            System.IO.File.WriteAllText(file+".json", sampleConfigJson)
    0 // return an integer exit code