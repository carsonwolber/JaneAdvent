open Hardcaml

let read_lines filename = 
  let file = open_in filename in 
    let rec loop acc =
      match input_line file with
        | line -> loop (line :: acc)
        | exception End_of_file -> close_in file ; List.rev acc
    in
    loop []


let parse line = 
  let d = line.[0] in
  let v = int_of_string (String.sub line 1 (String.length line - 1)) in 
  (d, v)


let () =
  let module Sim = Cyclesim.With_interface(Dial.I)(Dial.O) in 
  let sim = Sim.create Dial.create in let inputs = Cyclesim.inputs sim in 
  let outputs = Cyclesim.outputs sim in
  inputs.value := Bits.of_int ~width:12 50; Cyclesim.cycle sim; 
  let lines = read_lines "directions.txt" in 
  List.iter (fun line -> 
    let (d, v) = parse line in 
      if d = 'L' then inputs.value := Bits.of_int ~width:12 (-1*v) else inputs.value := Bits.of_int ~width:12 v;
      Cyclesim.cycle sim
    ) lines ; Printf.printf "final value: %d\n" (Bits.to_int !(outputs.zeros))