open Solvers.Signature

module Solver : Solver = struct
  let parse data =
    String.split_on_char '\n' data
    |> List.map (fun s -> Array.init (String.length s) (String.get s))

  module IntSet = Set.Make (Int)

  let naloga1 data =
    let index = String.index data 'S' in
    let data = parse data in
    let s = IntSet.empty |> IntSet.add index in
    let splits = ref 0 in
    let rec aux s = function
      | [] -> ()
      | x :: xs ->
          let s =
            IntSet.fold
              (fun e acc ->
                if x.(e) = '^' then (
                  splits := succ !splits;
                  acc |> IntSet.add (e - 1) |> IntSet.add (e + 1))
                else IntSet.add e acc)
              s IntSet.empty
          in
          aux s xs
    in
    aux s data;
    string_of_int !splits

  let naloga2 data _ =
    let index = String.index data 'S' in
    let line_length = String.index data '\n' in
    let data = parse data in
    let t = Array.init line_length (fun x -> if x = index then 1 else 0) in
    let rec aux = function
      | [] -> ()
      | x :: xs ->
          Array.iteri
            (fun i e ->
              if e = '^' then (
                t.(i - 1) <- t.(i - 1) + t.(i);
                t.(i + 1) <- t.(i + 1) + t.(i);
                t.(i) <- 0)
              else ())
            x;
          aux xs
    in
    aux data;
    Array.fold_left (fun acc x -> acc + x) 0 t |> string_of_int
end
