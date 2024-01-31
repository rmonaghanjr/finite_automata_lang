let rec unique_elements x =
  let rec uniq_help l n = 
    match l with
    | [] -> []
    | h :: t -> if n = h then uniq_help t n else h::(uniq_help t n) in
  match x with
  | [] -> []
  | h::t -> h::(uniq_help (unique_elements t) h)
