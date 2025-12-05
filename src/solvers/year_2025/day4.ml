open Solvers.Signature
open Str

module Solver : Solver = struct
  let parse data =
    let data = Str.split (Str.regexp "\n\n") data in
    let ranges =
      data |> fun l ->
      List.nth l 0 |> String.split_on_char '\n'
      |> List.map (fun line ->
             match String.split_on_char '-' line |> List.map int_of_string with
             | [ a; b ] -> (a, b)
             | _ -> failwith "error")
    in
    let numbers =
      data |> fun l ->
      List.nth l 1 |> String.split_on_char '\n' |> List.map int_of_string
    in
    (ranges, numbers)

  let naloga1 data =
    let ranges, numbers = parse data in
    List.fold_left
      (fun acc n ->
        if List.exists (fun (a, b) -> n >= a && n <= b) ranges then acc + 1
        else acc)
      0 numbers
    |> string_of_int

  let naloga2 data _ =
    let ranges = parse data |> fst |> List.sort compare in
    let rec merge acc current = function
      | [] -> List.rev (current :: acc)
      | (a_next, b_next) :: r ->
          let a, b = current in
          if a_next <= b + 1 then
            let b' = max b b_next in
            merge acc (a, b') r
          else merge (current :: acc) (a_next, b_next) r
    in
    match ranges with
    | [] -> "0"
    | first :: rest ->
        let merged = merge [] first rest in
        List.fold_left (fun acc (a, b) -> acc + (b - a + 1)) 0 merged
        |> string_of_int
end
