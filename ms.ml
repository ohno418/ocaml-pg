(* wip *)
(* === Minesweeper === *)

(* Space index
   - 0: bomb
   - 1: safe but not dug
   - 2: dug
   - 3: flag *)

(* Initialize new 5*5 board. *)
let init_board =
  [[0; 1; 1; 0; 1];
   [1; 1; 1; 1; 1];
   [1; 1; 1; 1; 0];
   [1; 1; 1; 1; 0];
   [1; 1; 0; 1; 1]] ;;

(* Print the board. *)
(* print : board -> unit *)
let print board =
  let print_space n =
    if n = 0 || n = 1
      (* hidden spaces *)
      then Printf.printf "%c" '-'
    else if n = 2
      (* dug spaces *)
      then Printf.printf "x"
    else if n = 3
      (* flag *)
      then Printf.printf "f"
    else failwith "unknown space" in
  let print_row row =
    List.iter print_space row;
    Printf.printf "\n" in
  List.iter print_row board;
  Printf.printf "\n" ;;

(* TODO *)
(* is_finish : board -> bool *)
let is_finish board = true ;;

(* TODO *)
(* handle_input : string -> board -> board *)
let handle_input input board =
  [[0; 1; 1; 0; 1];
   [1; 1; 1; 1; 1];
   [1; 1; 1; 1; 0];
   [1; 1; 1; 1; 0];
   [2; 1; 0; 1; 1]] ;;

let main () =
  let board = init_board in
  print board;

  while is_finish board do
    Printf.printf "Input <action>,<space-x>,<space-y>.\n";
    Printf.printf "  actions: \"d\" for dig, \"f\" for flag\n";
    Printf.printf "  (e.g. Input \"d,3,1\" for \"Dig the 3rd spaces of 1st row.\")\n";
    Printf.printf "\n";

    let input = read_line () in
    Printf.printf "your input: %s\n\n" input;

    (* TODO *)
    board = handle_input input board;
    print board;
  done ;;

main () ;;
