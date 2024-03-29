namespace SSPApp

open SSPLib
open FSharp.Data
open System.Text.RegularExpressions

module Config =
    let printHelp () =
            let msg = "SSPApp <mode: exmp | gen> <config file>"
            printfn "%s" msg
    type taskType = FirstFound | SearchUntil | FirstFoundCount | SearchUntilCount
    type resultStrategy = All | First of int (*| Bests of (Filter.filter*int) *) | Bests of (Filter.metric*int)
    type config = {
            numOfAgents:int;
            numOfStates:int;
            transitionFunctionsFile:string;
            destinationStatesFile:string;
            numOfSteps:int;
            outputFilePrefix:string;
            task: taskType;
            resultStrategy: resultStrategy;
            destinationStrategy: Frontend.destinationStrategy;
            forceNoIdling: bool
            printSATs:bool
        }
    type operationalData = {
            initialSituationMatrix : StateSpacePolicy.situationsInState array;
            transitionMatrix: SquareMatrix.matrix<StateSpacePolicy.coursesBetweenSituations>;
            allImportedTransFuncs:seq<transFunRaw>;
            destinationStateIndex:int
        }
    [<Literal>]
    let SAMPLE_CONFIG_JSON = """{
      "numberOfAgents": 777,
      "numberOfStates": 100000,
      "transitionFunctionsFile": "trans_funs.csv",
      "destinationStatesFile": "dest_states.csv",
      "numberOfSteps": 40,
      "outputFilePrefix": "result",
      "task": "search until",
      "resultStrategy": "best:2",
      "destinationStrategy": "first found",
      "forceNoIdling":true,
      "printSATs":true
    }"""
    type ConfigProvider = JsonProvider<SAMPLE_CONFIG_JSON>
    let private _parseResultStrategy resultStrategy = 
        let (|FirstMatched|AllMatched|BestMatched|) input =
            let fr = Regex("first:[0-9]+")
            let ar = Regex("all")
            //let br = Regex("best:[0-9]+:[0-9]+\\.[0-9]+")
            let br = Regex("best:[0-9]+")
            if fr.IsMatch(input) then
                let numOfResults = Regex("[0-9]+").Match(input)
                FirstMatched numOfResults.Value
            else if ar.IsMatch(input) then
                AllMatched
            (*else if br.IsMatch(input) then
                let numOfResults = Regex("[0-9]+").Match(input)
                let minEngagement = Regex("[0-9]+\\.[0-9]+").Match(input)
                BestMatched (numOfResults.Value,minEngagement.Value)*)
            else if br.IsMatch(input) then
                let numOfResults = Regex("[0-9]+").Match(input)
                BestMatched numOfResults.Value
            else
                raise ( invalidArg "resultStrategy" ("Result resolve strategy "+input+" is undefined"))
        match resultStrategy with
        | FirstMatched limitOfResultsStr -> 
            let limitOfResults = int limitOfResultsStr
            First limitOfResults
        | AllMatched -> All
        (*| BestMatched (limitOfResultsStr,minEngagementStr) -> 
            let limitOfResults = int limitOfResultsStr
            let minEngagement = double minEngagementStr
            //let currentNumOfResults = ref 0
            let filter = Filter.filterMostEngaging minEngagement
            Bests (filter,limitOfResults)*)
        | BestMatched limitOfResultsStr ->
            let limitOfResults = int limitOfResultsStr
            Bests (Filter.calcMetric,limitOfResults)
    let private _parseTask task = 
        match task with
            | "ff" | "first found" -> FirstFound
            | "su" | "search until" -> SearchUntil
            | "ffc" | "first found count" -> FirstFoundCount
            | "suc" | "search until count" -> SearchUntilCount
            | s -> raise ( invalidArg "task" ("Task type \""+s+"\" is undefined"))
    let private _parseDestinationStrategy strategy =
        match strategy with
            | "ff" | "first found" -> Frontend.FirstFound
            | "r" | "random" -> Frontend.Random
            | s -> raise ( invalidArg "destinationStrategy" ("Destination strategy type \""+s+"\" is undefined")) 
    let parseConfigFromJson (jsonFile:string) =
        let config = ConfigProvider.Load(jsonFile)
        {
            numOfAgents=config.NumberOfAgents;
            numOfStates=config.NumberOfStates;
            transitionFunctionsFile=config.TransitionFunctionsFile;
            destinationStatesFile=config.DestinationStatesFile;
            numOfSteps=config.NumberOfSteps;
            outputFilePrefix=config.OutputFilePrefix;
            resultStrategy=_parseResultStrategy config.ResultStrategy;
            task= _parseTask config.Task;
            destinationStrategy=_parseDestinationStrategy config.DestinationStrategy;
            forceNoIdling=config.ForceNoIdling;
            printSATs=config.PrintSaTs
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
