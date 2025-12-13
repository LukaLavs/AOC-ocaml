open Solvers.Signature

module Solver : Solver = struct
  let parse data =
    let tbl = Hashtbl.create 1000 in
    String.split_on_char '\n' data
    |> List.iter (fun line ->
           if String.trim line <> "" then
             Hashtbl.add tbl (String.sub line 0 3)
               (String.sub line 5 (String.length line - 5)
               |> String.split_on_char ' '));
    tbl

  let naloga1 data =
    let adj = parse data in
    let memo = Hashtbl.create 1000 in
    let rec dfs u =
      if u = "out" then 1
      else
        match Hashtbl.find_opt memo u with
        | Some v -> v
        | None ->
            let total =
              match Hashtbl.find_opt adj u with
              | None -> 0
              | Some vs -> List.fold_left (fun acc v -> acc + dfs v) 0 vs
            in
            Hashtbl.add memo u total;
            total
    in
    string_of_int (dfs "you")

  let naloga2 data _ =
    let adj = parse data in
    let memo = Hashtbl.create 1000 in
    let rec dfs u dac fft =
      let key = (u, dac, fft) in
      match Hashtbl.find_opt memo key with
      | Some v -> v
      | None ->
          let dac, fft = (dac || u = "dac", fft || u = "fft") in
          let total =
            if u = "out" then if dac && fft then 1 else 0
            else
              match Hashtbl.find_opt adj u with
              | None -> 0
              | Some vs ->
                  List.fold_left (fun acc v -> acc + dfs v dac fft) 0 vs
          in
          Hashtbl.add memo key total;
          total
    in
    string_of_int (dfs "svr" false false)
end
