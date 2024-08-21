
(* ******************************************** *)
(** Basic functions on finite automata *)
(* ******************************************** *)
(**
   Author: Emily Miner
*)
   type symbol = char
   type input = char list
   
   type state = string
   
   (* transition function *)
   type tf = (state * symbol * state) list
   
   (* initial state * transition function * end state *)
   type fa = { states: state list; start:state; tf: tf; final: state list}
   
   
   (* ******************************************** *)
   (* Examples of automata *)
   (* ******************************************** *)
   
   let a = {states = ["q0";"q1";"q2"];
            start = "q0";
            tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")];
            final = ["q2"]}
   
   let a2 = {states = ["q0";"q1";"q2";"q3";"q4"];
             start = "q0";
             tf = [("q0",'a',"q1"); ("q1",'b',"q1")
                  ; ("q1",'c',"q2");  ("q3",'a',"q4")];
             final= ["q2"]
            }
   let tf_of_a = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")]
   
   
   
   (* ******************************************** *)
   (* Helper functions *)
   (* This function takes in a string and returns a char list in reverse order.*)
   (* ******************************************** *)
   
   let input_of_string s =
     let rec exp i l =
       if i < 0 then l else exp (i - 1) (s.[i] :: l) in
     exp (String.length s - 1) []
   
   
   (* ******************************************** *)
   (* Simulating automata *)
   (* ******************************************** *)
   
   let apply_transition_function tf state symbol =
     let rec apply_transition_function_helper = function
       | [] -> None
       | (state', symbol', state'') :: t ->
           if state = state' && symbol = symbol' then Some state''
           else apply_transition_function_helper t
      in apply_transition_function_helper tf

    let accept fa input = 
      let state = fa.start in
      let rec accept_helper input state =
        match input with
        | [] -> List.mem state fa.final
        | symbol :: rest ->
            match apply_transition_function fa.tf state symbol with
            | None -> false
            | Some next_state -> accept_helper rest next_state
      in accept_helper input state

    let next tf state symbol =
      let next' = List.filter (fun (state', symbol', _) -> state = state' && symbol = symbol') tf in
      List.map (fun (_, _, state'') -> state'') next'

    let deterministic fa =
      let deterministic_helper (state, symbol, _) =
        let a = List.filter (fun (state', symbol', _) -> state' = state && symbol' = symbol) fa.tf in
        List.length a = 1 in
        List.for_all deterministic_helper fa.tf

    let valid fa =
      let valid_state state = List.mem state fa.states in
      let no_duplicates lst = List.length lst = List.length (List.sort_uniq compare lst) in
      no_duplicates fa.states && valid_state fa.start && List.for_all valid_state fa.final && deterministic fa

    let reachable fa =
      List.rev(let rec reachable_helper visited current =
        if List.mem current visited then 
          visited
        else
          let new_visited = current::visited in
          List.fold_left (fun acc (a,_,c) -> if a=current then reachable_helper acc c else acc) new_visited fa.tf
      in reachable_helper [] fa.start)

    let non_empty fa =
      let is_reachable = reachable fa in
      List.exists (fun state -> List.mem state fa.final) is_reachable

    let remove_dead_states fa =
      let reachable_states = reachable fa in
      let new_states = List.filter (fun s -> List.mem s reachable_states) fa.states in
      let new_tf = List.filter (fun (a, _, c) -> List.mem a new_states && List.mem c new_states) fa.tf in
      let new_finals = List.filter (fun s -> List.mem s new_states) fa.final in
      { states = new_states; start = fa.start; final = new_finals; tf = new_tf }
      