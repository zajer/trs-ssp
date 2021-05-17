open System
open SSPLib

[<EntryPoint>]
let main argv =
    let numOfAgents = Array.item 0 argv |> int
    let numOfStates = Array.item 1 argv |> int
    let transitionsFile = Array.item 2 argv
    let destStatesFile = Array.item 3 argv
    let numOfSteps = Array.item 4 argv |> int
    let outputFile = Array.item 5 argv
    S._number_of_agents := numOfAgents
    S._number_of_states := numOfStates
    let importedTransFuns = Frontend.import_trans_funs transitionsFile
    let destStates = Frontend.import_dest_states destStatesFile
    let dest_state_idx = Frontend.destination_state_idx Frontend.FirstFound destStates
    let initSituation = Array.init numOfAgents (fun i -> i+1,0) |> SS.init_situation 
    let maxNumOfSteps = int numOfSteps
    let init_sm,tm = Frontend.make_ssp_system importedTransFuns initSituation 0 (S.num_of_states ())
    let situationsMx,num_of_steps,is_found = Frontend.search_for_situation_in_state init_sm tm dest_state_idx maxNumOfSteps
    if is_found then 
        (
            printfn "%s" ("Desired state with id="+ dest_state_idx.ToString()+" found");
            let result_walk = Frontend.walk_from_situation_matrix Frontend.FirstFound situationsMx dest_state_idx
            let walk_to_save = Frontend.export_walk result_walk importedTransFuns
            Frontend.export_trans_funs walk_to_save outputFile
        )
        else
            printfn "%s" ("Desired state has not been reached with "+num_of_steps.ToString()+" steps!")
    0 // return an integer exit code