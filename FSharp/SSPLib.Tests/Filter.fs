namespace SSPLib.Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
[<TestClass>]
type FilterTestClass () =

    [<TestMethod>]
    member this.FilterMostEngagingTest1 () =
        let sat = [|(1,2);(2,2);(3,2)|]
        let situation = SSPLib.StateSpace.initSituation sat
        let minEngagement = 3.0
        let result = SSPLib.Filter.filterMostEngaging minEngagement situation
        Assert.IsTrue(result);
    [<TestMethod>]
    member this.FilterMostEngagingTest2 () =
        let sat = [|(1,2);(2,2);(3,2)|]
        let situation = SSPLib.StateSpace.initSituation sat
        let minEngagement = 2.0
        let result = SSPLib.Filter.filterMostEngaging minEngagement situation
        Assert.IsTrue(result);
    [<TestMethod>]
    member this.FilterMostEngagingTest3 () =
        let sat = [|(2,6);(1,0);(3,0)|]
        let situation = SSPLib.StateSpace.initSituation sat
        let minEngagement = 1.1
        let result = SSPLib.Filter.filterMostEngaging minEngagement situation
        Assert.IsFalse(result);
    [<TestMethod>]
    member this.FilterMostEngagingTest4 () =
        let sat = [|(2,0);(1,5);(3,0)|]
        let situation = SSPLib.StateSpace.initSituation sat
        let minEngagement = 1.0
        let result = SSPLib.Filter.filterMostEngaging minEngagement situation
        Assert.IsTrue(result);
    [<TestMethod>]
    member this.FilterMostEngagingTest5 () =
        let sat = [|(2,2);(1,0);(3,4)|]
        let situation = SSPLib.StateSpace.initSituation sat
        let minEngagement = 1.6
        let result = SSPLib.Filter.filterMostEngaging minEngagement situation
        Assert.IsFalse(result);
    [<TestMethod>]
    member this.FilterMostEngagingTest6 () =
        let sat = [|(2,2);(1,0);(3,4)|]
        let situation = SSPLib.StateSpace.initSituation sat
        let minEngagement = 1.5
        let result = SSPLib.Filter.filterMostEngaging minEngagement situation 
        Assert.IsTrue(result);
    [<TestMethod>]
    member this.FilterMostEngagingTest7 () =
        let sat = [|(2,3);(1,0);(3,3)|]
        let situation = SSPLib.StateSpace.initSituation sat
        let minEngagement = 2.1
        let result = SSPLib.Filter.filterMostEngaging minEngagement situation
        Assert.IsFalse(result);
    [<TestMethod>]
    member this.FilterMostEngagingTest8 () =
        let sat = [|(2,3);(1,0);(3,3)|]
        let situation = SSPLib.StateSpace.initSituation sat
        let minEngagement = 2.0
        let result = SSPLib.Filter.filterMostEngaging minEngagement situation
        Assert.IsTrue(result);
    [<TestMethod>]
    member this.FilterMostEngagingTest9 () =
        let sat = [|(2,3);(1,2);(3,1)|]
        let situation = SSPLib.StateSpace.initSituation sat
        let minEngagement = 2.1
        let result = SSPLib.Filter.filterMostEngaging minEngagement situation
        Assert.IsFalse(result);
    [<TestMethod>]
    member this.FilterMostEngagingTest10 () =
        let sat = [|(2,3);(1,2);(3,1)|]
        let situation = SSPLib.StateSpace.initSituation sat
        let minEngagement = 2.0
        let result = SSPLib.Filter.filterMostEngaging minEngagement situation
        Assert.IsTrue(result);

    [<TestMethod>]
    member this.FilterLimitedNumOfMostEngagingTest1 () =
        let sat = [|(1,3);(2,0);(3,0)|]
        let situation = SSPLib.StateSpace.initSituation sat
        let minEngagement = 1.0
        let maxNumOfPositiveResults = 2
        let filter = SSPLib.Filter.filterLimitedNumOfMostEngaging maxNumOfPositiveResults (ref 0)
        let result1 = filter minEngagement situation
        let result2 = filter minEngagement situation
        let result3 = filter minEngagement situation
        Assert.IsTrue(result1);
        Assert.IsTrue(result2);
        Assert.IsFalse(result3);
    [<TestMethod>]
    member this.FilterLimitedNumOfMostEngagingTest2 () =
        let sat1 = [|(1,3);(2,0);(3,3)|]
        let sat2 = [|(1,3);(2,2);(3,1)|]
        let sat3 = [|(1,1);(2,1);(3,1)|]
        let sat4 = [|(1,3);(2,3);(3,3)|]
        let situation1 = SSPLib.StateSpace.initSituation sat1
        let situation2 = SSPLib.StateSpace.initSituation sat2
        let situation3 = SSPLib.StateSpace.initSituation sat3
        let situation4 = SSPLib.StateSpace.initSituation sat4
        let minEngagement = 3.0
        let maxNumOfPositiveResults = 1
        let filter = SSPLib.Filter.filterLimitedNumOfMostEngaging maxNumOfPositiveResults (ref 0)
        let result1 = filter minEngagement situation1
        let result2 = filter minEngagement situation2
        let result3 = filter minEngagement situation3
        let result4 = filter minEngagement situation4
        Assert.IsFalse(result1);
        Assert.IsFalse(result2);
        Assert.IsTrue(result3);
        Assert.IsFalse(result4);
