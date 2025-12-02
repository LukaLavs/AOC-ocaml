open Solvers.Signature

module Solver : Solver = struct
  let parse data =
    data |> String.split_on_char ','
    |> List.map (fun part ->
           match String.split_on_char '-' part with
           | [ a; b ] -> (a, b)
           | _ -> failwith "Invalid input")

  let is_a_number (s : string) : bool = String.length s = 1 || s.[0] <> '0'

  let is_repeated (s : string) : bool =
    let l = String.length s in
    l mod 2 = 0 && String.sub s 0 (l / 2) = String.sub s (l / 2) (l / 2)

  let is_repeated_any s =
    let len = String.length s in
    let rec check block_len =
      if block_len > len / 2 then false
      else if len mod block_len <> 0 then check (block_len + 1)
      else
        let blocks = len / block_len in
        let sub = String.sub s 0 block_len in
        let rec verify i =
          if i = blocks then true
          else if String.sub s (i * block_len) block_len <> sub then false
          else verify (i + 1)
        in
        if blocks >= 2 && verify 1 then true else check (block_len + 1)
    in
    check 1

  let naloga1 data =
    let ranges = parse data in
    let is_invalid s = (not (is_a_number s)) || is_repeated s in
    let rec go acc = function
      | [] -> Int64.to_string acc
      | (a, b) :: t ->
          let a = Int64.of_string a in
          let b = Int64.of_string b in
          let rec loop i acc =
            if i > b then acc
            else
              let acc' =
                if is_invalid (Int64.to_string i) then Int64.add acc i else acc
              in
              loop (Int64.succ i) acc'
          in
          go (loop a acc) t
    in
    go 0L ranges

  let naloga2 data _ =
    let ranges = parse data in
    let is_invalid s = (not (is_a_number s)) || is_repeated_any s in
    let rec go acc = function
      | [] -> Int64.to_string acc
      | (a, b) :: t ->
          let a = Int64.of_string a in
          let b = Int64.of_string b in
          let rec loop i acc =
            if i > b then acc
            else
              let acc' =
                if is_invalid (Int64.to_string i) then Int64.add acc i else acc
              in
              loop (Int64.succ i) acc'
          in
          go (loop a acc) t
    in
    go 0L ranges
end
