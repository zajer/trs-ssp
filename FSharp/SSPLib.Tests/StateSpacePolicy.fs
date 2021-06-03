namespace SSPLib.Tests
open SSPLib.StateSpacePolicy
open Microsoft.VisualStudio.TestTools.UnitTesting
[<TestClass>]
type StateSpacePolicyTestClass () =
    let _transFun permutation timeShiftsMap sat = 
        Array.mapi 
            (fun i (aid,t) ->  
                match Map.tryFind (i+1) timeShiftsMap with
                | None -> aid,t
                | Some ts -> aid,t + ts
            )
            sat
        |> Array.permute permutation
    let _mapToIndexFun map key =
        (Map.find (key+1) map) - 1
    let _areWalksEqual (expected:SSPLib.StateSpace.walk) (actual:SSPLib.StateSpace.walk) = 
        List.iter2 (fun (tf1:SSPLib.State.transFun) (tf2:SSPLib.State.transFun) -> Assert.AreEqual ( tf1.transitionIdx, tf2.transitionIdx) ) expected actual
    let _areSituationsEqual (expected:SSPLib.StateSpace.situation) (actual:SSPLib.StateSpace.situation) =
        CollectionAssert.AreEqual (expected.currentSAT, actual.currentSAT);
        _areWalksEqual expected.currentWalk actual.currentWalk
    let _areListOfSituationsEqual expected actual = 
        List.iter2 ( fun esis asis -> _areSituationsEqual esis asis) expected actual
    let _extractSituations situationsInState = 
        match situationsInState with 
        | NotReachable -> []
        | Situations s -> List.ofSeq s
    [<TestMethod>]
    member this.ConvoluteTest1 () =
        let sat = [|(1,0);(2,0)|]
        let stateTransFunc = {
            SSPLib.State.func=_transFun (Map.ofList [(1,2);(2,1)] |> _mapToIndexFun) (Map.ofList [2,7;1,3]);
            SSPLib.State.transitionIdx=3
            }
        let sitution = SSPLib.StateSpace.initSituation sat
        let situationInState = initSituationInState sitution
        let coursesBetweenSituations = Courses  [stateTransFunc]
        let result = convolute situationInState coursesBetweenSituations 
        let expectedResult = {SSPLib.StateSpace.currentSAT=[|(2,7);(1,3)|];SSPLib.StateSpace.currentWalk=[stateTransFunc]} 
        let resultAsList,isMatchedCorrectly = match result with
                                                | Situations s -> s,true
                                                | NotReachable -> [],false
        if not isMatchedCorrectly then
            Assert.Fail ()
        Assert.AreEqual (1, List.length resultAsList)
        _areSituationsEqual expectedResult (List.head resultAsList)
        ()
    [<TestMethod>]
    member this.ConvoluteTest2 () =
        let sat = [|(1,15);(2,1)|]
        let stateTransFunc1 = {
            SSPLib.State.func=_transFun (Map.ofList [(1,2);(2,1)] |> _mapToIndexFun) (Map.ofList [2,7;1,3]);
            SSPLib.State.transitionIdx=3
            }
        let stateTransFunc2 = {
            SSPLib.State.func=_transFun (Map.ofList [(1,1);(2,2)] |> _mapToIndexFun) (Map.ofList [1,4;2,5]);
            SSPLib.State.transitionIdx=7
            }
        let sitution = SSPLib.StateSpace.initSituation sat
        let situationInState = initSituationInState sitution
        let coursesBetweenSituations = Courses [stateTransFunc1;stateTransFunc2]
        let result = convolute situationInState coursesBetweenSituations 
        let expectedResult = [
            {SSPLib.StateSpace.currentSAT=[|(2,8);(1,18)|];SSPLib.StateSpace.currentWalk=[stateTransFunc1]};
            {SSPLib.StateSpace.currentSAT=[|(1,19);(2,6)|];SSPLib.StateSpace.currentWalk=[stateTransFunc2]};
        ]
        let resultAsList,isMatchedCorrectly = match result with
                                                | Situations s -> s,true
                                                | NotReachable -> [],false
        if not isMatchedCorrectly then
            Assert.Fail ()
        Assert.AreEqual (2, List.length resultAsList)
        _areListOfSituationsEqual expectedResult resultAsList
        ()
    [<TestMethod>]
    member this.ConvoluteTest3 () =
        let sat1 = [|(1,1);(2,3)|]
        let sat2 = [|(2,1);(1,3)|]
        let stateTransFunc1 = {
            SSPLib.State.func=_transFun (Map.ofList [(1,2);(2,1)] |> _mapToIndexFun) (Map.ofList [2,7;1,3]);
            SSPLib.State.transitionIdx=3
            }
        let situation1 = SSPLib.StateSpace.initSituation sat1
        let situation2 = SSPLib.StateSpace.initSituation sat2
        let situationInState = Situations [situation1;situation2]
        let coursesBetweenSituations = Courses [stateTransFunc1]
        let result = convolute situationInState coursesBetweenSituations 
        let expectedResult = [
            {SSPLib.StateSpace.currentSAT=[|(2,10);(1,4)|];SSPLib.StateSpace.currentWalk=[stateTransFunc1]};
            {SSPLib.StateSpace.currentSAT=[|(1,10);(2,4)|];SSPLib.StateSpace.currentWalk=[stateTransFunc1]};
        ]
        let resultAsList,isMatchedCorrectly = match result with
                                                | Situations s -> s,true
                                                | NotReachable -> [],false
        if not isMatchedCorrectly then
            Assert.Fail ()
        Assert.AreEqual (2, List.length resultAsList)
        _areListOfSituationsEqual expectedResult resultAsList
        ()
    [<TestMethod>]
    member this.ConvoluteTest4 () =
        let sat1 = [|(1,1);(2,3)|]
        let sat2 = [|(2,7);(1,5)|]
        let stateTransFunc1 = {
            SSPLib.State.func=_transFun (Map.ofList [(1,2);(2,1)] |> _mapToIndexFun) (Map.ofList [2,7;1,3]);
            SSPLib.State.transitionIdx=3
            }
        let stateTransFunc2 = {
            SSPLib.State.func=_transFun (Map.ofList [(1,1);(2,2)] |> _mapToIndexFun) (Map.ofList [1,4;2,5]);
            SSPLib.State.transitionIdx=7
            }
        let situation1 = SSPLib.StateSpace.initSituation sat1
        let situation2 = SSPLib.StateSpace.initSituation sat2
        let situationInState = Situations [situation1;situation2]
        let coursesBetweenSituations = Courses [stateTransFunc1;stateTransFunc2]
        let result = convolute situationInState coursesBetweenSituations 
        let expectedResult = [
            {SSPLib.StateSpace.currentSAT=[|(2,10);(1,4)|];SSPLib.StateSpace.currentWalk=[stateTransFunc1]};
            {SSPLib.StateSpace.currentSAT=[|(1,5);(2,8)|];SSPLib.StateSpace.currentWalk=[stateTransFunc2]};
            {SSPLib.StateSpace.currentSAT=[|(1,12);(2,10)|];SSPLib.StateSpace.currentWalk=[stateTransFunc1]};
            {SSPLib.StateSpace.currentSAT=[|(2,11);(1,10)|];SSPLib.StateSpace.currentWalk=[stateTransFunc2]};
        ]
        let resultAsList,isMatchedCorrectly = match result with
                                                | Situations s -> List.ofSeq s,true
                                                | NotReachable -> [],false
        if not isMatchedCorrectly then
            Assert.Fail ()
        Assert.AreEqual (4, List.length resultAsList)
        _areListOfSituationsEqual expectedResult resultAsList
        ()
    [<TestMethod>]
    member this.ConvoluteTest5 () =
        let sat = [|(1,1);(2,0)|]
        let stateTransFunc = {
            SSPLib.State.func=
                (fun ar -> 
                    let a1,t1 = Array.item 0 ar
                    let a2,t2 = Array.item 1 ar
                    if t1 = t2 then
                        [|(a2,t2+7);(a1,t1+3)|]
                    else
                        [|(-1,-1);(-1,-1)|]
                );
            SSPLib.State.transitionIdx=3
            }
        let sitution = SSPLib.StateSpace.initSituation sat
        let situationInState = initSituationInState sitution
        let coursesBetweenSituations = Courses [stateTransFunc]
        let result = convolute situationInState coursesBetweenSituations 
        let _,isMatchedCorrectly = match result with
                                                | Situations s -> s,false
                                                | NotReachable -> [],true
        if not isMatchedCorrectly then
            Assert.Fail ()
        ()
    [<TestMethod>]
    member this.MultiplyTest1 () =
        let sat1 = [|(1,0);(2,0)|]
        let stateTransFunc1 = {
            SSPLib.State.func=_transFun (Map.ofList [(1,2);(2,1)] |> _mapToIndexFun) (Map.ofList [2,7;1,3]);
            SSPLib.State.transitionIdx=3
            }
        let stateTransFunc2 = {
            SSPLib.State.func=_transFun (Map.ofList [(1,1);(2,2)] |> _mapToIndexFun) (Map.ofList [1,4;2,5]);
            SSPLib.State.transitionIdx=7
            }
        let situation1 = SSPLib.StateSpace.initSituation sat1
        let situationInState = Situations [situation1]
        let coursesBetweenStates0_1 = Courses [stateTransFunc1]
        let coursesBetweenStates0_2 = Courses [stateTransFunc2]
        let sitMatrix = initSituationMatrix situationInState 0 3
        let transMatrix = SSPLib.SquareMatrix.init (fun _ _ -> NoTransitions) 3;
        SSPLib.SquareMatrix.update transMatrix 0 1 coursesBetweenStates0_1;
        SSPLib.SquareMatrix.update transMatrix 0 2 coursesBetweenStates0_2;
        let result = multiply sitMatrix transMatrix
        let sitsInState0 = Array.get result 0 |> _extractSituations
        let sitsInState1 = Array.get result 1 |> _extractSituations
        let sitsInState2 = Array.get result 2 |> _extractSituations
        Assert.AreEqual (0, List.length sitsInState0)
        Assert.AreEqual (1, List.length sitsInState1)
        Assert.AreEqual (1, List.length sitsInState2)
        _areListOfSituationsEqual
            [ { SSPLib.StateSpace.currentSAT=[|(2,7);(1,3)|];currentWalk=[stateTransFunc1] }]
            sitsInState1
        _areListOfSituationsEqual
            [ { SSPLib.StateSpace.currentSAT=[|(1,4);(2,5)|];currentWalk=[stateTransFunc2] }]
            sitsInState2
        ()
    [<TestMethod>]
    member this.MultiplyTest2 () =
        let sat1 = [|(1,0);(2,0)|]
        let sat2 = [|(2,7);(1,5)|]
        let stateTransFunc1 = {
            SSPLib.State.func=_transFun (Map.ofList [(1,2);(2,1)] |> _mapToIndexFun) (Map.ofList [2,7;1,3]);
            SSPLib.State.transitionIdx=3
            }
        let stateTransFunc2 = {
            SSPLib.State.func=_transFun (Map.ofList [(1,1);(2,2)] |> _mapToIndexFun) (Map.ofList [1,4;2,5]);
            SSPLib.State.transitionIdx=7
            }
        let situation1 = SSPLib.StateSpace.initSituation sat1
        let situation2 = SSPLib.StateSpace.initSituation sat2
        let situationInState = Situations [situation1;situation2]
        let coursesBetweenStates0_1 = Courses [stateTransFunc1]
        let coursesBetweenStates0_2 = Courses [stateTransFunc2]
        let sitMatrix = initSituationMatrix situationInState 0 3
        let transMatrix = SSPLib.SquareMatrix.init (fun _ _ -> NoTransitions) 3;
        SSPLib.SquareMatrix.update transMatrix 0 1 coursesBetweenStates0_1;
        SSPLib.SquareMatrix.update transMatrix 0 2 coursesBetweenStates0_2;
        let result = multiply sitMatrix transMatrix
        let sitsInState0 = Array.get result 0 |> _extractSituations
        let sitsInState1 = Array.get result 1 |> _extractSituations
        let sitsInState2 = Array.get result 2 |> _extractSituations
        Assert.AreEqual (0, List.length sitsInState0)
        Assert.AreEqual (2, List.length sitsInState1)
        Assert.AreEqual (2, List.length sitsInState2)
        _areListOfSituationsEqual
            [ 
                { SSPLib.StateSpace.currentSAT=[|(2,7);(1,3)|];currentWalk=[stateTransFunc1] };
                { SSPLib.StateSpace.currentSAT=[|(1,12);(2,10)|];currentWalk=[stateTransFunc1] };
            ]
            sitsInState1
        _areListOfSituationsEqual
            [ 
                { SSPLib.StateSpace.currentSAT=[|(1,4);(2,5)|];currentWalk=[stateTransFunc2] };
                { SSPLib.StateSpace.currentSAT=[|(2,11);(1,10)|];currentWalk=[stateTransFunc2] }
            ]
            sitsInState2
        ()
    [<TestMethod>]
    member this.MultiplyTest3 () =
        let sat1 = [|(1,0);(2,0)|]
        let sat2 = [|(2,7);(1,5)|]
        let stateTransFunc1 = {
            SSPLib.State.func=
                (fun ar -> 
                    let a1,t1 = Array.item 0 ar
                    let a2,t2 = Array.item 1 ar
                    if t1 = t2 then
                        [|(a2,t2+7);(a1,t1+3)|]
                    else
                        [|(-1,-1);(-1,-1)|]
                );
            SSPLib.State.transitionIdx=3
            }
        let stateTransFunc2 = {
            SSPLib.State.func=_transFun (Map.ofList [(1,1);(2,2)] |> _mapToIndexFun) (Map.ofList [1,4;2,5]);
            SSPLib.State.transitionIdx=7
            }
        let situation1 = SSPLib.StateSpace.initSituation sat1
        let situation2 = SSPLib.StateSpace.initSituation sat2
        let situationInState = Situations [situation1;situation2]
        let coursesBetweenStates0_1 = Courses [stateTransFunc1]
        let coursesBetweenStates0_2 = Courses[stateTransFunc2]
        let sitMatrix = initSituationMatrix situationInState 0 3
        let transMatrix = SSPLib.SquareMatrix.init (fun _ _ -> NoTransitions) 3;
        SSPLib.SquareMatrix.update transMatrix 0 1 coursesBetweenStates0_1;
        SSPLib.SquareMatrix.update transMatrix 0 2 coursesBetweenStates0_2;
        let result = multiply sitMatrix transMatrix
        let sitsInState0 = Array.get result 0 |> _extractSituations
        let sitsInState1 = Array.get result 1 |> _extractSituations
        let sitsInState2 = Array.get result 2 |> _extractSituations
        Assert.AreEqual (0, List.length sitsInState0)
        Assert.AreEqual (1, List.length sitsInState1)
        Assert.AreEqual (2, List.length sitsInState2)
        _areListOfSituationsEqual
            [ 
                { SSPLib.StateSpace.currentSAT=[|(2,7);(1,3)|];currentWalk=[stateTransFunc1] };
            ]
            sitsInState1
        _areListOfSituationsEqual
            [ 
                { SSPLib.StateSpace.currentSAT=[|(1,4);(2,5)|];currentWalk=[stateTransFunc2] };
                { SSPLib.StateSpace.currentSAT=[|(2,11);(1,10)|];currentWalk=[stateTransFunc2] }
            ]
            sitsInState2
        ()
    [<TestMethod>]
    member this.MultiplyTest4 () =
        let sat1 = [|(1,0);(2,0)|]
        let stateTransFunc1 = {
            SSPLib.State.func=_transFun (Map.ofList [(1,2);(2,1)] |> _mapToIndexFun) (Map.ofList [2,7;1,3]);
            SSPLib.State.transitionIdx=3
            }
        let stateTransFunc2 = {
            SSPLib.State.func=_transFun (Map.ofList [(1,1);(2,2)] |> _mapToIndexFun) (Map.ofList [1,4;2,5]);
            SSPLib.State.transitionIdx=7
            }
        let situation1 = SSPLib.StateSpace.initSituation sat1
        let situationInState = Situations [situation1]
        let coursesBetweenStates0_1 = Courses [stateTransFunc1]
        let coursesBetweenStates0_2 = Courses [stateTransFunc2]
        let sitMatrix = initSituationMatrix situationInState 0 3
        let transMatrix = SSPLib.SquareMatrix.init (fun _ _ -> NoTransitions) 3;
        SSPLib.SquareMatrix.update transMatrix 0 1 coursesBetweenStates0_1;
        SSPLib.SquareMatrix.update transMatrix 1 2 coursesBetweenStates0_2;
        let result = multiply sitMatrix transMatrix
        let sitsInState0 = Array.get result 0 |> _extractSituations
        let sitsInState1 = Array.get result 1 |> _extractSituations
        let sitsInState2 = Array.get result 2 |> _extractSituations
        Assert.AreEqual (0, List.length sitsInState0)
        Assert.AreEqual (1, List.length sitsInState1)
        Assert.AreEqual (0, List.length sitsInState2)
        _areListOfSituationsEqual
            [ 
                { SSPLib.StateSpace.currentSAT=[|(2,7);(1,3)|];currentWalk=[stateTransFunc1] };
               
            ]
            sitsInState1
        ()