open Hardcaml
open Signal

module I = struct
  type 'a t = {
    clock : 'a;
    clear : 'a;
    value : 'a [@bits 12];
  } [@@deriving sexp_of, hardcaml]
end

module O = struct 
  type 'a t = {
    pos : 'a [@bits 7];
    zeros : 'a [@bits 16];
  } [@@deriving sexp_of, hardcaml]
end


let create (i : _ I.t) = 
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in 
  let nxt = wire 12 in 
  let sum = reg spec nxt in 
  let count_nxt = wire 16 in 
  let count = reg spec count_nxt in
  let base = sum +: i.value in 
  let pivot s = Signal.mux2 (s >=+. 100) (s -:. 100) (Signal.mux2 (s <+. 0) (s +:. 100) s)
  in let chained_pivots = 
    base
    (* highest value in direction list is <1000 so 10 pivots of +/- 100 are sufficient*)
    |> pivot
    |> pivot
    |> pivot
    |> pivot
    |> pivot
    |> pivot
    |> pivot
    |> pivot
    |> pivot
    |> pivot
  in
  let () = Signal.assign nxt chained_pivots in let is_z = chained_pivots ==:. 0 in 
  let () = Signal.assign count_nxt (Signal.mux2 is_z (count +:. 1) count) in 
  { O.pos = uresize sum 7; zeros = count }

