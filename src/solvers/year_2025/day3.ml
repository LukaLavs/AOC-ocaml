open Solvers.Signature

module Solver : Solver = struct
  let parse data =
    data |> String.split_on_char '\n'
    |> List.filter (( <> ) "")
    |> List.map (fun line ->
           line |> String.to_seq
           |> Seq.map (fun c -> if c = '.' then 0 else 1)
           |> Array.of_seq)
    |> Array.of_list

  let result data =
    let d =
      [ (-1, 0); (-1, -1); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]
    in
    let result = ref 0 in
    let copy = Array.map Array.copy data in
    Array.iteri
      (fun i row ->
        Array.iteri
          (fun j e ->
            let rec check d i j e s =
              if e <> 1 then false
              else
                match d with
                | [] -> s < 4
                | x :: xs ->
                    let ni = i + fst x in
                    let nj = j + snd x in
                    if
                      0 <= ni
                      && ni < Array.length data
                      && 0 <= nj
                      && nj < Array.length row
                    then check xs i j e (s + data.(fst x + i).(snd x + j))
                    else check xs i j e s
            in
            if check d i j e 0 then (
              result := !result + 1;
              copy.(i).(j) <- 0))
          row)
      data;
    (!result, copy)

  let naloga1 data =
    let data = parse data in
    string_of_int (fst (result data))

  let naloga2 data _ =
    let data = parse data in
    let rec run data acc =
      let r', d = result data in
      if r' = 0 then string_of_int acc else run d (acc + r')
    in
    run data 0
end
