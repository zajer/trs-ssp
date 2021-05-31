namespace SSPApp.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.ParseConfigFromJsonTest1 () =
        let configFile = "excfg1.json"
        let config = SSPApp.Config.parseConfigFromJson configFile
        Assert.AreEqual(3,config.numOfAgents)
        Assert.AreEqual(100000,config.numOfStates)
        Assert.AreEqual("trans_funs.csv",config.transitionFunctionsFile)
        Assert.AreEqual("dest_states.csv",config.destinationStatesFile)
        Assert.AreEqual(40,config.numOfSteps)
        Assert.AreEqual("result",config.outputFilePrefix)
        Assert.AreEqual(SSPApp.Config.SearchUntil,config.task)
        (*let resultsToFilter = Seq.ofList [
                                            [|(1,0);(2,2);(3,0)|] |> SSPLib.StateSpace.initSituation //1
                                            [|(1,1);(2,1);(3,1)|] |> SSPLib.StateSpace.initSituation ; //3
                                            [|(1,4);(2,2);(3,0)|] |> SSPLib.StateSpace.initSituation; //1.5
                                            [|(1,2);(2,2);(3,0)|] |> SSPLib.StateSpace.initSituation ; //2
                                            [|(1,2);(2,40);(3,40)|] |> SSPLib.StateSpace.initSituation; //2.05
                                     ]
        let expectedFilter = SSPLib.Filter.filterLimitedNumOfMostEngaging 2 (ref 0) 2.01
        let expectedResultsFiltered = Seq.filter expectedFilter resultsToFilter*)
        
        let isMatchedCorrectly = ref true
        let _,n = match config.resultStrategy with
                        | SSPApp.Config.Bests (m,n) -> m,n
                        | _ -> isMatchedCorrectly := false; (fun _ -> 0.0),-1
        if not !isMatchedCorrectly then
            Assert.Fail "Output type should be equal to SSPLib.Frontend.Bests"
        //let actualResultsFiltered = Seq.filter metric resultsToFilter
        Assert.AreEqual(2,n)
        //Assert.AreEqual(expectedResultsFiltered |> List.ofSeq, actualResultsFiltered |> List.ofSeq)
        Assert.AreEqual(SSPLib.Frontend.FirstFound,config.destinationStrategy)
    [<TestMethod>]
    member this.ParseConfigFromJsonTest2 () =
        let configFile = "excfg2.json"
        let config = SSPApp.Config.parseConfigFromJson configFile
        Assert.AreEqual(3,config.numOfAgents)
        Assert.AreEqual(777,config.numOfStates)
        Assert.AreEqual("abc1xyz.csv",config.transitionFunctionsFile)
        Assert.AreEqual("xyz2abc.csv",config.destinationStatesFile)
        Assert.AreEqual(123,config.numOfSteps)
        Assert.AreEqual("result2",config.outputFilePrefix)
        Assert.AreEqual(SSPApp.Config.SearchUntilCount,config.task)

        let isMatchedCorrectly = ref true
        let filter = match config.resultStrategy with
                        | SSPApp.Config.All -> ()
                        | _ -> isMatchedCorrectly := false
        if not !isMatchedCorrectly then
            Assert.Fail "Output type should be equal to SSPLib.Frontend.All"
        
        Assert.AreEqual(SSPLib.Frontend.FirstFound,config.destinationStrategy)
    [<TestMethod>]
    member this.ParseConfigFromJsonTest3 () =
        let configFile = "excfg3.json"
        let config = SSPApp.Config.parseConfigFromJson configFile
        Assert.AreEqual(777,config.numOfAgents)
        Assert.AreEqual(123,config.numOfStates)
        Assert.AreEqual("a.csv",config.transitionFunctionsFile)
        Assert.AreEqual("x.csv",config.destinationStatesFile)
        Assert.AreEqual(321,config.numOfSteps)
        Assert.AreEqual("r3",config.outputFilePrefix)
        Assert.AreEqual(SSPApp.Config.FirstFound,config.task)

        let isMatchedCorrectly = ref true
        let n = match config.resultStrategy with
                        | SSPApp.Config.First n -> n
                        | _ -> isMatchedCorrectly := false; -1
        if not !isMatchedCorrectly then
            Assert.Fail "Output type should be equal to SSPLib.Frontend.First"
        Assert.AreEqual(3,n)
        Assert.AreEqual(SSPLib.Frontend.Random,config.destinationStrategy)
