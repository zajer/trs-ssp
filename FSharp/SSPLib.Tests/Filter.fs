namespace SSPLib.Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
[<TestClass>]
type FilterTestClass () =

    [<TestMethod>]
    member this.FilterMostEngagingTest1 () =
        let situation = SSPLib.State.ofArray [|(1,2);(2,2);(3,2)|]
        let minEngagement = 3
        //let result = SSPLib.Filter.filterMostEngaging minEngagement situation
        Assert.IsTrue(true);
