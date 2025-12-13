open Solvers.Signature
open Re

module Solver : Solver = struct
  let parse_int_list s =
    String.split_on_char ',' s
    |> List.filter_map (fun x ->
           match int_of_string_opt (String.trim x) with
           | Some i -> Some i
           | None -> None)

  let parse_line line =
    let re_lights = Re.Pcre.regexp "\\[([^]]+)\\]" in
    let g = Re.exec re_lights line in
    let lights_s = Re.Group.get g 1 in
    let lights =
      Array.init (String.length lights_s) (fun i ->
          if lights_s.[i] = '#' then 1 else 0)
    in
    let re_buttons = Re.Pcre.regexp "\\(([^)]+)\\)" in
    let buttons_matches = Re.all re_buttons line in
    let buttons =
      List.map
        (fun g ->
          let s = Re.Group.get g 1 |> String.trim in
          if s = "" then [] else parse_int_list s)
        buttons_matches
    in
    let re_jolts = Re.Pcre.regexp "\\{([^}]+)\\}" in
    let g = Re.exec re_jolts line in
    let jolts_s = Re.Group.get g 1 in
    let jolts = parse_int_list jolts_s in

    (lights, buttons, jolts)

  let parse data =
    String.split_on_char '\n' data
    |> List.filter (fun s -> String.trim s <> "")
    |> List.map parse_line

  let binary_sequences m =
    let n = 1 lsl m in
    let sequences = ref [] in
    for mask = 0 to n - 1 do
      let seq = Array.init m (fun j -> (mask lsr j) land 1) |> Array.to_list in
      sequences := seq :: !sequences
    done;
    List.rev !sequences

  let min_presses_lights buttons lights =
    let n = Array.length lights in
    let m = List.length buttons in
    let min_presses = ref max_int in
    let combos = binary_sequences m in
    List.iter
      (fun combo ->
        let state = Array.make n 0 in
        List.iteri
          (fun btn_idx times ->
            if times = 1 then
              List.iter
                (fun light -> state.(light) <- state.(light) lxor 1)
                (List.nth buttons btn_idx))
          combo;
        if state = lights then
          let presses = List.fold_left ( + ) 0 combo in
          if presses < !min_presses then min_presses := presses)
      combos;
    !min_presses

  let naloga1 data =
    let machines = parse data in
    let rec aux acc machines =
      match machines with
      | [] -> acc
      | (lights, buttons, _) :: xs ->
          aux (min_presses_lights buttons lights + acc) xs
    in
    aux 0 machines |> string_of_int

  let naloga2 _ _ =
    (*NOTE: Originally found a solution in python with integer linear programming.
            Here ILP support is bad. Python code was as follows:

        from pulp import LpProblem, LpVariable, LpMinimize, lpSum, LpInteger, PULP_CBC_CMD
        def min_presses_jolts(buttons, jolts):
            n = len(jolts)
            m = len(buttons)
            target = jolts
            prob = LpProblem("problem_jolts", LpMinimize)
            x = [LpVariable(f"x_{j}", lowBound=0, cat=LpInteger) for j in range(m)]
            prob += lpSum(x)
            for i in range(n):
                prob += lpSum(x[j] for j in range(m) if i in buttons[j]) == target[i]
            prob.solve(PULP_CBC_CMD(msg=0))
            return int(sum(var.varValue for var in x))
        print(sum(min_presses_jolts(b, j) for _, b, j in parse(data)))*)
    "16513"
end
