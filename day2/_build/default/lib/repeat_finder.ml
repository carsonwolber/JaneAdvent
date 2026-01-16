open Hardcaml
open Signal

module U = Unsigned

module I = struct 
  type 'a t = {
    clock: 'a;
    clear: 'a;
    num: 'a[@bits 33];
    digits: 'a[@bits 8]
  } [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    count : 'a[@bits 8];
  } [@@deriving hardcaml]
end

let create (i: _ I.t) = 
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in 
  let opts = [
    (*at most 10 digits are ever in range*)
    gnd;
    gnd;
    (U.modulo i.num (of_int ~width:33 11)) ==:. 0;
    gnd;
    U.(modulo i.num (of_int ~width:33 101)) ==:. 0;
    gnd;
    U.(modulo i.num (of_int ~width:33 1001)) ==:. 0;
    gnd;
    U.(modulo i.num (of_int ~width:33 10001)) ==:. 0;
    gnd;
    U.(modulo i.num (of_int ~width:33 100001)) ==:. 0;
  ] in
  let count_nxt = wire 8 in 
  let count = reg spec count_nxt in
  let choice = Signal.mux i.digits opts in 
  let () = Signal.assign count_nxt count +: choice in
  {O.count = count}

