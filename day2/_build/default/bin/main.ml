let parse_ids filename = 
  let file = open_in filename in
  let contents = input_line file in
  close_in file ; String.split_on_char ',' contents

let parse ln = match String.split_on_char '-' ln with 
  | [a;b] -> (int_of_string a, int_of_string b)
  | _ -> failwith "bad input"


let () = let ids = parse_ids "ids.txt" in
List.iter(fun rng -> 
  let (b, t) = parse rng in Printf.printf "bottom %d top %d\n" b t 
  ) ids
