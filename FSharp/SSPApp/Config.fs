namespace SSPApp

open System
open SSPLib
open FSharp.Data

module Config =
    let printHelp () =
            let msg = "SSPApp <mode: exmp | gen> <config file>"
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
            computationStrategy: Frontend.computation_strategy
        }
    type operationalData = {
            initialSituationMatrix : SSP.situations_in_state array;
            transitionMatrix: SquareMatrix.t<SSP.courses_between_situations>;
            allImportedTransFuncs:seq<trans_fun_raw>;
            destinationStateIndex:int
        }
    [<Literal>]
    let sampleConfigJson = """{
      "numberOfAgents": 777,
      "numberOfStates": 100000,
      "transitionFunctionsFile": "trans_funs.csv",
      "destinationStatesFile": "dest_states.csv",
      "numberOfSteps": 40,
      "outputFilePrefix": "result",
      "outputType": "all",
      "taskType": "search until",
      "computationStrategy": "limit:3"
    }
        """
    type ConfigProvider = JsonProvider<sampleConfigJson>
    let private _parseResultStrategy resultStrategy = 
        match resultStrategy with
        | "first" -> Frontend.First
        | "all" -> Frontend.All
        | "best" -> 
            let limit = 2
            let minEngagement = (double) 1.0
            let n = ref 0
            let filter = Filter.filterLimitedNumOfMostEngaging limit n minEngagement
            Frontend.Bests filter
        | s -> raise ( invalidArg "resultStrategy" ("Result resolve strategy "+s+" is undefined"))
    let private _parseTask task = 
        match task with
            | "ff" | "first found" -> FirstFound
            | "su" | "search until" -> SearchUntil
            | "ffc" | "first found count" -> FirstFoundCount
            | "suc" | "search until count" -> SearchUntilCount
            | s -> raise ( invalidArg "task" ("Task type "+s+" is undefined"))
    let private _parseComputationStrategy strategy = 
        match strategy with
            | "all" -> Frontend.ComputeAll
            | "limited" -> Frontend.ComputeLimited 1000
            | s -> raise ( invalidArg "task" ("Computation strategy "+s+" is undefined"))
    let parseConfigFromJson (jsonFile:string) =
        let config = ConfigProvider.Load(jsonFile)
        {
            numOfAgents=config.NumberOfAgents;
            numOfStates=config.NumberOfStates;
            transitionFunctionsFile=config.TransitionFunctionsFile;
            destinationStatesFile=config.DestinationStatesFile;
            numOfSteps=config.NumberOfSteps;
            outputFilePrefix=config.OutputFilePrefix;
            outputType=_parseResultStrategy config.OutputType;
            taskType= _parseTask config.TaskType;
            computationStrategy= _parseComputationStrategy config.ComputationStrategy
        }
    let requiredNumberOfArgs = 2
    type mode = Example | GenerateWalks 
    let parseInputArgs argv =
        let mode = match Array.item 0 argv with
                    | "gen" -> GenerateWalks
                    | "exmp" -> Example
                    | s -> raise (invalidArg "mode" ("Mode "+s+" is undefined"))
        let file = Array.item 1 argv
        mode,file
