open Solvers.Signature

module Solver : Solver = struct
  let parse data =
    String.split_on_char '\n' data
    |> List.map (fun l ->
           let s = String.split_on_char ',' l in
           (int_of_string (List.nth s 0), int_of_string (List.nth s 1)))
    |> Array.of_list

  let area a b c d = (abs (d - b) + 1) * (abs (c - a) + 1)

  let naloga1 data =
    let points = parse data in
    let length = Array.length points in
    let maximal = ref 0 in
    for i = 0 to length - 2 do
      for j = i + 1 to length - 1 do
        let a, b = points.(i) and c, d = points.(j) in
        if area a b c d > !maximal then maximal := area a b c d
      done
    done;
    string_of_int !maximal

  let naloga2 data _ =
    let points = parse data in
    let length = Array.length points in
    let vertical_edges =
      List.init (length / 2) (fun i ->
          let x1, y1 = points.(2 * i) in
          let _, y2 = points.((2 * i) + 1) in
          let y_lo = min y1 y2 in
          let y_hi = max y1 y2 in
          (x1, y_lo, y_hi))
    in
    let slabs =
      Array.to_list points |> List.map snd |> List.sort_uniq compare
      |> Array.of_list
    in
    let length_slabs = Array.length slabs in
    let inside_intervals =
      Array.init (length_slabs - 1) (fun _ -> [| (-1, -1) |])
    in
    for i = 0 to length_slabs - 2 do
      let y_low = slabs.(i) in
      let y_high = slabs.(i + 1) in
      let xs =
        List.filter_map
          (fun (x, lo, hi) ->
            if lo < y_high && hi > y_low then Some x else None)
          vertical_edges
        |> List.sort compare |> Array.of_list
      in
      let length_xs = Array.length xs in
      let intervals =
        Array.init (length_xs / 2) (fun i -> (xs.(2 * i), xs.((2 * i) + 1)))
      in
      inside_intervals.(i) <- intervals
    done;

    let slab_covers k xL xR =
      let rec aux i =
        if i >= Array.length inside_intervals.(k) then false
        else
          let a, b = inside_intervals.(k).(i) in
          if a <= xL && xR <= b then true
          else if a > xL then false
          else aux (i + 1)
      in
      aux 0
    in

    let slab_index y =
      let rec aux low high =
        if low >= high then low
        else
          let mid = (low + high) / 2 in
          if slabs.(mid + 1) <= y then aux (mid + 1) high else aux low mid
      in
      let i = aux 0 (length_slabs - 2) in
      if i < 0 then 0 else if i >= length_slabs - 1 then length_slabs - 2 else i
    in

    let max_area = ref 0 in
    for i = 0 to length - 1 do
      let x1, y1 = points.(i) in
      for j = i + 1 to length - 1 do
        let x2, y2 = points.(j) in
        let xL, xR = if x1 < x2 then (x1, x2) else (x2, x1) in
        let yB, yT = if y1 < y2 then (y1, y2) else (y2, y1) in
        if xL <> xR && yB <> yT then
          let k_start = slab_index yB in
          let k_end = slab_index yT in
          let k_start, k_end =
            if k_end < k_start then (k_end, k_start) else (k_start, k_end)
          in

          let valid =
            let rec check k =
              if k >= k_end then true
              else if not (slab_covers k xL xR) then false
              else check (k + 1)
            in
            check k_start
          in

          if valid then
            let area = (xR - xL + 1) * (yT - yB + 1) in
            if area > !max_area then max_area := area
      done
    done;

    string_of_int !max_area
end
