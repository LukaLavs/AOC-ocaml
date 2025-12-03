open Solvers.Signature

module Solver : Solver = struct
  let parse data =
    data |> String.split_on_char '\n'
    |> List.filter (( <> ) "")
    |> List.map (fun line ->
           line |> String.to_seq
           |> Seq.map (fun c -> int_of_char c - int_of_char '0')
           |> List.of_seq)

  let result part data =
    let rec pow a b = if b = 0 then 1 else a * pow a (b - 1) in
    let data = parse data in
    List.fold_left
      (fun acc d ->
        let d = List.mapi (fun i x -> (i, x)) d in
        let l = List.length d in
        let m = ref 0 in
        let n = ref 0 in
        let st = ref 0 in
        for i = part - 1 downto 0 do
          let slice = List.filter (fun (p, _) -> p >= !st && p < l - i) d in
          let f =
            List.fold_left
              (fun acc x -> if snd x > snd acc then x else acc)
              (List.hd slice) slice
          in
          let p, e = f in
          st := p + 1;
          n := !n + (e * pow 10 i);
          if !n > !m then m := !n
        done;
        acc + !m)
      0 data

  let naloga1 data = string_of_int (result 2 data)
  let naloga2 data _ = string_of_int (result 12 data)
end
