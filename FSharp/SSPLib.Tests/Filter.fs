namespace SSPLib.Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
[<TestClass>]
type FilterTestClass () =

    [<TestMethod>]
    member this.FilterMostEngagingTest1 () =
        let sat = SSPLib.State.ofArray [|(1,2);(2,2);(3,2)|]
        let situation = SSPLib.StateSpace.initSituation sat
        let minEngagement = 3.0
        let result = SSPLib.Filter.filterMostEngaging minEngagement situation
        Assert.IsTrue(result);
    [<TestMethod>]
    member this.FilterMostEngagingTest2 () =
        let sat = SSPLib.State.ofArray [|(1,2);(2,2);(3,2)|]
        let situation = SSPLib.StateSpace.initSituation sat
        let minEngagement = 2.0
        let result = SSPLib.Filter.filterMostEngaging minEngagement situation
        Assert.IsTrue(result);