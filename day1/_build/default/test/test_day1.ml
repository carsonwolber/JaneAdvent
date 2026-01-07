open Hardcaml

let testbench () = 
  let module Sim = Cyclesim.With_interface(Day1.Dial.I)(Day1.Dial.O) in
  let sim = Sim.create Day1.Dial.create in

  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

  inputs.value := Bits.of_int ~width:12 10;
  Cyclesim.cycle sim;
  assert (Bits.to_int !(outputs.pos) = 10);

  inputs.value := Bits.of_int ~width:12 (-5);
  Cyclesim.cycle sim;
  assert (Bits.to_int !(outputs.pos) = 5);

  inputs.value := Bits.of_int ~width:12 (-5);
  Cyclesim.cycle sim;
  assert (Bits.to_int !(outputs.pos) = 0);

  inputs.value := Bits.of_int ~width:12 (-1);
  Cyclesim.cycle sim;
  assert (Bits.to_int !(outputs.pos) = 99);

  inputs.value := Bits.of_int ~width:12 2;
  Cyclesim.cycle sim;
  assert (Bits.to_int !(outputs.pos) = 1);

  assert (Bits.to_int !(outputs.zeros) = 1);

  Printf.printf "tests pass!"

let () = testbench ()