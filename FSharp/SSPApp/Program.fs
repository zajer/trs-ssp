open System
open SSPLib
let printHelp () =
        let msg = "SSPApp <numOfAgents> <numOfStates> <transitionsFile>.csv <destStatesFile>.csv <maxNumOfSteps> <outputFile> <resultStrategy:= first | all> <task:=firstFound | searchUntil>"
        printfn "%s" msg
type taskType = FirstFound | SearchUntil | FirstFoundCount | SearchUntilCount 
type config = {
        numOfAgents:int;
        numOfStates:int;
        transitionFunctionsFile:string;
        destinationStatesFile:string;
        numOfSteps:int;
        outputFilePrefix:string;
        outputType: Frontend.result_strategy 
        taskType: taskType
    }
type operationalData = {
        initialSituationMatrix : SSP.situations_in_state array;
        transitionMatrix: SquareMatrix.t<SSP.courses_between_situations>;
        allImportedTransFuncs:seq<trans_fun_raw>;
        destinationStateIndex:int
    }
let requiredNumberOfArgs = 8
let parseConfig argv =
    let noa = Array.item 0 argv |> int
    let nosta = Array.item 1 argv |> int
    let transFile = Array.item 2 argv
    let dstFile = Array.item 3 argv
    let nostep = Array.item 4 argv |> int
    let outFile = Array.item 5 argv
    let resType = 
        match Array.item 6 argv with
        | "first" -> Frontend.First
        | "all" -> Frontend.All
        | "best" -> raise (new NotImplementedException ())
        | s -> raise ( invalidArg "resultStrategy" ("Result resolve strategy "+s+" is undefined"))
    let task = 
        match Array.item 7 argv with
        | "ff" -> FirstFound
        | "su" -> SearchUntil
        | "ffc" -> FirstFoundCount
        | "suc" -> SearchUntilCount
        | s -> raise ( invalidArg "task" ("Task type "+s+" is undefined"))
    {
        numOfAgents=noa;
        numOfStates=nosta;
        transitionFunctionsFile=transFile;
        destinationStatesFile=dstFile;
        numOfSteps=nostep;
        outputFilePrefix=outFile;
        outputType=resType;
        taskType=task
    }
let saveResults resultWalks destStateIdx importedTransFuns outputFilePrefix =
    let walksAsArray = Array.ofList resultWalks
    Array.Parallel.iteri 
        (
            fun i w -> 
                    let tf = Frontend.export_walk w importedTransFuns
                    Frontend.export_trans_funs tf ( outputFilePrefix+"_"+i.ToString()+".csv")
        )
        walksAsArray
let performFindFirstRoutine (conf:config) (opd:operationalData) saveResult= 
    let situationsMatrix,_,is_found = Frontend.search_for_situation_in_state opd.initialSituationMatrix opd.transitionMatrix opd.destinationStateIndex conf.numOfSteps
    if is_found then
        printfn "%s" ("Desired state with id="+ opd.destinationStateIndex.ToString()+" found")
        let resultWalks = Frontend.walk_from_situation_matrix conf.outputType situationsMatrix opd.destinationStateIndex
        printfn "Found %d unique walks" (Seq.length resultWalks)
        if saveResult then
            saveResults (List.ofSeq resultWalks) opd.destinationStateIndex opd.allImportedTransFuncs conf.outputFilePrefix
    else
        printfn "%s" ("Desired state has not been reached!")
let performSearchUntilRoutine (conf:config) (opd:operationalData) saveResult =
    let resultWalks,is_found = Frontend.search_for_walks_leading_to_state opd.initialSituationMatrix opd.transitionMatrix opd.destinationStateIndex conf.numOfSteps
    if is_found then
        printfn "%s" ("Desired state with id="+ opd.destinationStateIndex.ToString()+" found")
        printfn "Found %d unique walks" (Seq.length resultWalks)
        if saveResult then
            saveResults (List.ofSeq resultWalks) opd.destinationStateIndex opd.allImportedTransFuncs conf.outputFilePrefix
    else
        printfn "%s" ("Desired state has not been reached!")
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
        let config = parseConfig argv
        S._number_of_agents := config.numOfAgents
        S._number_of_states := config.numOfStates
        let opd = config2OperationalData config
        match config.taskType with
        | FirstFound -> performFindFirstRoutine config opd true
        |  SearchUntil -> performSearchUntilRoutine config opd true
        | FirstFoundCount -> performFindFirstRoutine config opd false
        | SearchUntilCount -> performSearchUntilRoutine config opd false
    0 // return an integer exit code