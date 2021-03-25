# State Space Policy (SSP)

This library provides a backbone for generating walks (in graph theory sense) in a state space defined as directed multigraph with transition functions assigned to edges.

## Key abstractions

There are a few essential definitions on which this software is build upon.

 - `State` - a state is atomic construction that defines number of `agents`, their order and a time moment they are in.
 
 Let's consider a state defined as a tiplet of 2-tuples of integers :
 ``` 
 type t = (int*int)*(int*int)*(int*int)
 ``` 
 It can represent all possible combinations of three agents (because there are three 2-tuples). As an example we can consider the following state compatible with above definitions:
 ```
 let some_state = (3,0),(1,777),(2,21)
 ```
In this state the agent with id 3 is the first in order<sup>2</sup> and is in the time moment equal to 0. Two other agents are at the time moment 777 and 21 respectively.

Among states, there might be some that can be considered as "impssible to happen". In that case we mark them as `negligible`. 

Some states are of special interests for us. Such states will be denoted as `desired states`.

We also need to define `state transition functions`. These are functions that allow to change states (order and time moments of agents).

 - `Situation` - a situation is a current state and a walk leading from an initial state to the current state. Situations do not consider negligible states.
 - `Walk` - a walk is an ordered sequence of state transition functions that lead from an initial state to a desired state.
 - `Situation matrix` - a one row matrix consisting of squences of all possible combinations of situations in each state with walks of the same length.
 - `Transition matrix` - a N<sub>S</sub> x N<sub>S</sub> adjacency matrix of a directed multigraph representing the state space of our interest. Elements of this matrix are state transition function.


 ## Miscellaneous

 This software aims to provide an implementation of operations described in detail in:
 https://doi.org/10.3390/s21020622