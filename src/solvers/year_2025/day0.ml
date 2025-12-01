open Solvers.Signature

module Solver : Solver = struct
  let parse data =
    data |> String.split_on_char '\n'
    |> List.filter (( <> ) "")
    |> List.map (fun line ->
           let dir = line.[0] in
           let n = int_of_string (String.sub line 1 (String.length line - 1)) in
           (dir, n))

  let naloga1 d =
    let rec f p c = function
      | [] -> string_of_int c
      | (x, n) :: t ->
          f
            ((p + if x = 'R' then n else 100 - n) mod 100)
            (c + if p = 0 then 1 else 0)
            t
    in
    f 50 0 (parse d)

  let naloga2 d _ =
    let rec f p c = function
      | [] -> string_of_int c
      | (x, n) :: t ->
          let c' = c + if x = 'R' then (p + n) / 100 else (n - p + 99) / 100 in
          if x = 'R' then f ((p + n) mod 100) c' t
          else f ((p + 100 - (n mod 100)) mod 100) c' t
    in
    f 50 0 (parse d)
end
