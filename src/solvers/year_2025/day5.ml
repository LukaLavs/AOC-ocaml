open Solvers.Signature
open Str

module Solver : Solver = struct
  let rec take n lst =
    match (n, lst) with
    | 0, _ | _, [] -> []
    | n, x :: xs -> x :: take (n - 1) xs

  let parse1 data =
    let temp = data |> String.split_on_char '\n' in
    let numbers =
      temp |> take 4
      |> List.map (fun l ->
             List.map int_of_string (Str.split (Str.regexp "[ ]+") l))
    in
    let operations =
      List.nth temp 4
      |> Str.split (Str.regexp "[ ]+")
      |> List.map (fun s -> s.[0])
    in
    (numbers, operations)

  let parse2 data =
    let temp =
      data |> String.split_on_char '\n'
      |> List.map (fun l -> List.init (String.length l) (String.get l))
    in
    let numbers =
      take 4 temp |> List.map (fun row -> List.map (String.make 1) row)
    in
    let operations = List.nth temp 4 in
    (numbers, operations)

  let naloga1 data =
    let numbers, operations = parse1 data in
    let rec aux acc (numbers, operations) =
      match (numbers, operations) with
      | [ []; []; []; [] ], [] -> string_of_int acc
      | [ n1 :: n1s; n2 :: n2s; n3 :: n3s; n4 :: n4s ], o :: os ->
          if o = '+' then
            aux (acc + n1 + n2 + n3 + n4) ([ n1s; n2s; n3s; n4s ], os)
          else aux (acc + (n1 * n2 * n3 * n4)) ([ n1s; n2s; n3s; n4s ], os)
      | _ -> failwith "error"
    in
    aux 0 (numbers, operations)

  let naloga2 data _ =
    let numbers, operations = parse2 data in
    let sym = ref ' ' in
    let t = ref Int64.zero in
    let rec aux acc (numbers, operations) =
      match (numbers, operations) with
      | [ []; []; []; [] ], [] -> Int64.add acc !t
      | [ n1 :: n1s; n2 :: n2s; n3 :: n3s; n4 :: n4s ], o :: os ->
          if o = '+' then (
            sym := o;
            t := Int64.zero);
          if o = '*' then (
            sym := o;
            t := Int64.one);
          let c = String.trim (n1 ^ n2 ^ n3 ^ n4) in
          if c = "" then aux (Int64.add acc !t) ([ n1s; n2s; n3s; n4s ], os)
          else (
            if !sym = '+' then t := Int64.add !t (Int64.of_string c);
            if !sym = '*' then t := Int64.mul !t (Int64.of_string c);
            aux acc ([ n1s; n2s; n3s; n4s ], os))
      | _ -> failwith "error"
    in
    Int64.to_string (aux Int64.zero (numbers, operations))
end
