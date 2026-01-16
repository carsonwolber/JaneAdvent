open Hardcaml

let parse_ids filename = 
  let file = open_in filename in
  let contents = input_line file in
  close_in file ; String.split_on_char ',' contents

let parse ln = match String.split_on_char '-' ln with 
  | [a;b] -> (int_of_string a, int_of_string b)
  | _ -> failwith "bad input"


let () = 
  let module Sim = Cyclesim.With_interface(Day2.Repeat_finder.I)(Day2.Repeat_finder.O) in 
  let sim = Sim.create Day2.Repeat_finder.create in let inputs = Cyclesim.inputs sim in 
  let outputs = Cyclesim.outputs sim in 
  let ids = parse_ids "ids.txt" in
  List.iter(fun rng -> 
    let (b, t) = parse rng in inputs.bot := Bits.of_int ~width:33 b ; 
    let i = ref b in while !i < t do if !i mod 2 = 0 then inputs.top := Bits.of_int ~width:33 !i; 
      inputs.digits := Bits.of_int ~width:8 (String.length (string_of_int !i)); incr i;
    Cyclesim.cycle sim done
    ) ids ; Printf.printf "final value: %d\n" (Bits.to_int !(outputs.count))