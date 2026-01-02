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
  let lines = read_lines "directions.txt" in
  List.iter (fun line ->
    let (d, v) = parse line in
    Printf.printf "Direction: %c, Value: %d\n" d v
) lines