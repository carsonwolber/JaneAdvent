open Hardcaml
open Signal

module U = Unsigned

module I = struct 
  type 'a t = {
    clock: 'a;
    clear: 'a;
    num: 'a[@bits 32];
    digits: 'a[@bits 8]
  } [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    count : 'a[@bits 8];
  } [@@deriving hardcaml]
end

module D = struct 
  type 'a t = {
    a : 'a[@bits 32];
    b : 'a[@bits 32];
    q : 'a[@bits 32]
  } [@@deriving hardcaml]
end

let rec div (d: _ D.t) = 
  if d.b !==: 0 then let c = d.a - d.b in 1 + div {D.a = c, D.b = b}


let create (i: _ I.t) = 
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in 
  let opts = [
    (*at most 10 digits are ever in range*)
    gnd;
    gnd;
    (i.num (of_int ~width:32 11)) ==:. 0;
    gnd;
    (i.num (of_int ~width:32 101)) ==:. 0;
    gnd;
    (i.num (of_int ~width:32 1001)) ==:. 0;
    gnd;
    (i.num (of_int ~width:32 10001)) ==:. 0;
    gnd;
    (i.num (of_int ~width:32 100001)) ==:. 0;
  ] in
  let count_nxt = wire 8 in 
  let count = reg spec count_nxt in
  let choice = Signal.mux i.digits opts in 
  let () = Signal.assign count_nxt count +: choice in
  {O.count = count}

