
open SSPLib
open SSPApp.Config

let saveResults resultWalks importedTransFuns outputFilePrefix =
    let walksAsArray = Array.ofSeq resultWalks
    printfn "Saving result to files with prefix: %s" outputFilePrefix
    Array.Parallel.iteri 
        (
            fun i w -> 
                    let tf = Frontend.export_walk w importedTransFuns
                    Frontend.export_trans_funs tf ( outputFilePrefix+"_"+i.ToString()+".csv")
        )
        walksAsArray
let performFindFirstRoutine (conf:config) (opd:operationalData) saveResult= 
    let situationsMatrix,_,is_found = 
        match conf.computationStrategy with
        | Frontend.ComputeAll -> Frontend.search_for_situation_in_state opd.initialSituationMatrix opd.transitionMatrix opd.destinationStateIndex conf.numOfSteps
        | Frontend.ComputeLimited limit -> Frontend.search_for_situation_in_state_limited opd.initialSituationMatrix opd.transitionMatrix opd.destinationStateIndex conf.numOfSteps limit
    if is_found then
        printfn "%s" ("Walk to the desired state with id="+ opd.destinationStateIndex.ToString()+" found")
        let resultWalks = Frontend.walk_from_situation_matrix conf.outputType situationsMatrix opd.destinationStateIndex
        printfn "Found %d unique walks" (Seq.length resultWalks)
        if saveResult then
            saveResults resultWalks opd.allImportedTransFuncs conf.outputFilePrefix
    else
        printfn "%s" ("Walk to the desired state has not been found!")
let performSearchUntilRoutine (conf:config) (opd:operationalData) saveResult =
    let resultWalks,is_found = 
        match conf.computationStrategy with
        | Frontend.ComputeAll -> Frontend.search_for_walks_leading_to_state opd.initialSituationMatrix opd.transitionMatrix opd.destinationStateIndex conf.numOfSteps conf.outputType
        | Frontend.ComputeLimited limit-> Frontend.search_for_walks_leading_to_state_limited opd.initialSituationMatrix opd.transitionMatrix opd.destinationStateIndex conf.numOfSteps conf.outputType limit
    if is_found then
        printfn "%s" ("Walk to the desired state with id="+ opd.destinationStateIndex.ToString()+" found")
        printfn "Found %d unique walks" (Seq.length resultWalks)
        if saveResult then
            saveResults resultWalks opd.allImportedTransFuncs conf.outputFilePrefix
    else
        printfn "%s" ("Walk to the desired state has not been found!")
let config2OperationalData conf =
    let importedTransFuns = Frontend.import_trans_funs conf.transitionFunctionsFile
    let destStates = Frontend.import_dest_states conf.destinationStatesFile
    let destStateIdx = Frontend.destination_state_idx Frontend.FirstFound destStates
    let initSituation = Array.init conf.numOfAgents (fun i -> i+1,0) |> SS.init_situation 
    let initSituationMatrix,transitionMatrix = Frontend.make_ssp_system importedTransFuns initSituation 0 (S.num_of_states ())
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
            S._number_of_agents := config.numOfAgents
            S._number_of_states := config.numOfStates
            let opd = config2OperationalData config
            match config.taskType with
            | FirstFound -> performFindFirstRoutine config opd true
            |  SearchUntil -> performSearchUntilRoutine config opd true
            | FirstFoundCount -> performFindFirstRoutine config opd false
            | SearchUntilCount -> performSearchUntilRoutine config opd false
        | Example -> 
            let exampleConfig = ConfigProvider.GetSample ()
            System.IO.File.WriteAllText(file+".json", sampleConfigJson)
    0 // return an integer exit code