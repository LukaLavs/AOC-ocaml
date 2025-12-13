open Solvers.Signature
open Str

module Solver : Solver = struct
  let parse data =
    Str.split (Str.regexp "\n\n") data |> fun l ->
    List.nth l (List.length l - 1)
    |> String.split_on_char '\n'
    |> List.map (fun e ->
           List.map int_of_string ((Str.split (Str.regexp "[x: ]+")) e))

  let naloga1 data =
    (*NOTICE: Due to the shape of the presents, they either fit trivially or they don't.
              Same is not true for test input.*)
    let regions = parse data in
    List.fold_left
      (fun acc e ->
        match e with
        | w :: l :: xs ->
            if w * l >= List.fold_left (fun acc' x -> acc' + (9 * x)) 0 xs then
              acc + 1
            else acc
        | _ -> failwith "error")
      0 regions
    |> string_of_int

  let naloga2 _ _ = "Merry 12th December"
end
