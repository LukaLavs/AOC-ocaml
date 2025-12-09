open Solvers.Signature

module Solver : Solver = struct
  let parse data =
    String.split_on_char '\n' data
    |> List.map (fun l ->
           match String.split_on_char ',' l |> List.map int_of_string with
           | [ a; b; c ] -> (a, b, c)
           | _ -> failwith "invalid input")
    |> Array.of_list

  let dist (a, b, c) (d, e, f) =
    ((a - d) * (a - d)) + ((b - e) * (b - e)) + ((c - f) * (c - f))

  let rec take n lst =
    match (n, lst) with
    | 0, _ | _, [] -> []
    | n, x :: xs -> x :: take (n - 1) xs

  let naloga1 data =
    let points = parse data in
    let n = Array.length points in

    let edges = ref [] in
    for i = 0 to n - 1 do
      for j = i + 1 to n - 1 do
        edges := (dist points.(i) points.(j), i, j) :: !edges
      done
    done;

    let selected =
      !edges
      |> List.sort (fun (d1, _, _) (d2, _, _) -> compare d1 d2)
      |> take 1000
    in

    let parent = Array.init n (fun i -> i) in
    let rank = Array.make n 0 in

    let rec find x =
      if parent.(x) = x then x
      else (
        parent.(x) <- find parent.(x);
        parent.(x))
    in

    let union x y =
      let rx, ry = (find x, find y) in
      if rx = ry then false
      else (
        if rank.(rx) < rank.(ry) then parent.(rx) <- ry
        else if rank.(rx) > rank.(ry) then parent.(ry) <- rx
        else (
          parent.(ry) <- rx;
          rank.(rx) <- rank.(rx) + 1);
        true)
    in

    List.iter (fun (_, i, j) -> ignore (union i j)) selected;

    let sizes = Array.make n 0 in
    for i = 0 to n - 1 do
      let r = find i in
      sizes.(r) <- sizes.(r) + 1
    done;

    let top3 = sizes |> Array.to_list |> List.sort (fun a b -> compare b a) in

    match top3 with
    | a :: b :: c :: _ -> string_of_int (a * b * c)
    | _ -> "error"

  let naloga2 data _ =
    let points = parse data in
    let n = Array.length points in

    let edges =
      List.init n (fun i -> Array.init (n - i - 1) (fun j -> (i, i + j + 1)))
      |> Array.concat
    in

    Array.sort
      (fun (i1, j1) (i2, j2) ->
        compare (dist points.(i1) points.(j1)) (dist points.(i2) points.(j2)))
      edges;

    let parent = Array.init n (fun i -> i) in
    let rank = Array.make n 0 in
    let rec find x =
      if parent.(x) = x then x
      else (
        parent.(x) <- find parent.(x);
        parent.(x))
    in
    let union x y =
      let rx, ry = (find x, find y) in
      if rx = ry then false
      else (
        if rank.(rx) < rank.(ry) then parent.(rx) <- ry
        else if rank.(rx) > rank.(ry) then parent.(ry) <- rx
        else (
          parent.(ry) <- rx;
          rank.(rx) <- rank.(rx) + 1);
        true)
    in

    let components = ref n in
    let last_edge = ref (0, 0) in
    Array.iter
      (fun (i, j) ->
        if union i j then (
          components := !components - 1;
          if !components = 1 then last_edge := (i, j)))
      edges;

    let i, j = !last_edge in
    let (a, _, _), (d, _, _) = (points.(i), points.(j)) in
    string_of_int (a * d)
end
