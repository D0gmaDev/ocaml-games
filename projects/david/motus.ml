#use "topfind";;
#require "graphics";;
#require "unix";;

open Graphics;;

Random.self_init ();;

(* peut être changé, à condition d'avoir un fichier de mots approprié *)
let word_length = 8;;

(* pour l'efffet momomotus, déconseillé, le son d'ocaml se joue très en retard *)
let sound_on = false;;

type case_color = EMPTY | UNDEFINED | RIGHT_SPOT | ELSEWHERE;;
type case = {letter: char; color: case_color};;

let blank_case () = {letter= '_'; color= EMPTY};;

let word_to_board_line word color = let array = Array.make (String.length word) {letter = 'c'; color = UNDEFINED} in
  for i = 0 to (String.length word - 1) do
      array.(i) <- {letter = word.[i]; color = color}
    done;
  array;;

(* PARAMETRES DES COULEURS DU JEU *)
let gray_color = (rgb 96 96 96);;
let light_gray_color = (rgb 192 192 192);;
let red_color = (rgb 204 0 0);;
let yellow_color = (rgb 204 204 0);;
let aqua_color = (rgb 89 230 240);;

let draw_board board = 
    clear_graph ();
    set_color gray_color;
    fill_rect 0 0 (size_x ()) (size_y ());
    set_color aqua_color;
    draw_rect 40 70 (25 * word_length) 175;
    for i = 0 to (Array.length board) - 1 do  
    let line = board.(i) in 
        for j = 0 to (Array.length line) - 1 do
        moveto ((j * 25) + 50) (size_y () - 40 - (i * 25));
        let case = line.(j) in (
          (match case.color with
          | EMPTY -> set_color light_gray_color
          | UNDEFINED -> set_color white
          | RIGHT_SPOT -> set_color red_color
          | ELSEWHERE -> set_color yellow_color);
          draw_char case.letter
        )
        done
    done;
    synchronize ();;

let rec next_key () = let e = Graphics.wait_next_event [Graphics.Key_pressed] in
    if e.Graphics.keypressed then e.Graphics.key else next_key ();;

let next_letter = 
  let is_letter c = match c with
  | 'a' .. 'z' -> true
  | _ -> false
  in let next charConsumer returnConsumer = let key = next_key () in
          if is_letter key then charConsumer key else
          if key = '\b' then returnConsumer ()
  in next;;

let next_number = 
  let is_number c = match c with
  | '0' .. '9' -> true
  | _ -> false
  in let rec next () = let key = next_key () in
          if is_number key then int_of_string (String.make 1 key)
          else next ()
  in next;;

let wait_any_key () = next_letter (fun c -> ()) (fun r -> ());;

exception Won of string * int;; (* Mot * essais *)

exception Lost of string;; (* Mot non trouvé *)

let play_sound array freq = 
  if sound_on then (
      draw_board array;
      sound freq 200
  );;

let evaluate_word array id answer = let word_array = array.(id) and won = ref true in 
  let in_word cha = String.contains answer cha
  in
  for i = 0 to word_length - 1 do
    if word_array.(i).letter = answer.[i] then 
      (word_array.(i) <- {letter = word_array.(i).letter; color = RIGHT_SPOT};
      play_sound array 600;
      if id < 5 then for j = id + 1 to 5 do 
       array.(j).(i) <- {letter = word_array.(i).letter; color = EMPTY}
      done)
      else (
        won := false;
        if in_word word_array.(i).letter then 
          (word_array.(i) <- {letter = word_array.(i).letter; color = ELSEWHERE};
          draw_board array;
          play_sound array 500)
        else (
          play_sound array 300
        )
      )
  done;
  draw_board array;
  !won;;

let play_word word =
  clear_graph ();

  let arr = Array.make_matrix 6 word_length (blank_case ()) in 

  let attempt = ref 0 and word_size = ref 0 in 

      (* donner la premiere lettre du mot *)
      for i = 0 to 5 do
        arr.(i).(0) <- {letter = word.[0]; color = EMPTY}
      done;

      draw_board arr;
      
      while true do
        
         next_letter (fun char -> 

          arr.(!attempt).(!word_size) <- {letter = char; color = UNDEFINED};
          draw_board arr;
          incr word_size;
            
          if !word_size = word_length then (
            if evaluate_word arr !attempt word then raise (Won(word, (!attempt + 1)));
            word_size := 0;
            incr attempt;

            if !attempt = 6 then raise (Lost word)
          )
         ) 
         (fun () -> 
          
          if !word_size > 0 then (
            arr.(!attempt).(!word_size - 1) <- blank_case ();
            draw_board arr;
            decr word_size;
          )
         )
      done;;

let degrade =
  let interpolation x1 x2 y1 y2 x = (y2-y1)*x/(x2-x1) + y1-(y2-y1)*x1/(x2-x1) in
  let root_inter x1 x2 y1 y2 x = int_of_float (sqrt (float_of_int (interpolation x1 x2 (y1*y1) (y2*y2) x))) in
  let degrade (r1, g1, b1) (r2, g2, b2) =
	  let largeur = size_x() and hauteur = size_y() in
		  for x = 0 to largeur - 1 do
			  set_color (rgb (root_inter 0 (largeur -1) r1 r2 x )
    		  (root_inter 0 (largeur -1) g1 g2 x)
    		  (root_inter 0 (largeur -1) b1 b2 x));
    		moveto x 0;
    		lineto x (hauteur-1);
    	done
  in degrade;;

let show_won s = (
  clear_graph ();
  degrade (20, 140, 20) (30, 160, 10);
  moveto 30 140;
  set_color white;
  draw_string ("Bon mot: " ^ s);
  moveto 30 125;
  draw_string "Tape une touche pour continuer";
  synchronize ();
  wait_any_key (); (* attends la prochaine frappe de lettre *)
);;

let show_lost s = (
  set_color white;
  moveto 10 25;
  draw_string ("Le mot etait " ^ s ^ ".");
  moveto 10 10;
  draw_string "Tape une lettre pour continuer";
  synchronize ();
  wait_any_key () (* attends la prochaine frappe de lettre *)
);;

let choose_random_word = 
  let words = Arg.read_arg ("C:/Users/david/Documents/dev/ocaml/motus/mots" ^ (string_of_int word_length) ^".txt")
  in let select () = words.(Random.int (Array.length words)) (* la borne du random est exclusive *)
  in select;;

(* TEST: choose_random_word ();; *)

let show_summary gamesList = 
  clear_graph ();
  degrade (250, 250, 250) (210, 250, 230);
  List.iteri (fun i -> fun game -> (
    moveto 10 (240 - (i*20));
    set_color (if snd game = 0 then red else green);
    draw_string ((fst game) ^ " : ");
    draw_string (if snd game = 0 then "Perdu" else (((string_of_int (snd game))) ^ " essais"));
  )) gamesList;
  synchronize ();; 

let generate_intent rounds victoires track = 
  let mean = if victoires = 0 then 0. else (float_of_int (List.fold_left (+) 0 (List.map snd track)) /. (float_of_int rounds)) in
    "https://twitter.com/intent/tweet?text=J%27ai%20fait%20" ^ (string_of_int victoires)
    ^ "%20/%20" ^ (string_of_int rounds) ^ "%20au%20Motus%20de%20David%20%28" ^ (string_of_float mean) ^ "%20essais%20en%20moyenne%29&hashtags=ocaml";;

let show_intro_menu () = 
  degrade (89, 230, 240) (100, 250, 220);
  set_color white;
  moveto 35 140;
  draw_string "MOTUS PAR DAVID MAREMBERT";
  synchronize ();
  wait_any_key ();;

let select_rounds () = 
  set_color gray_color;
  fill_rect 0 0 (size_x ()) (size_y ());
  set_color white;
  moveto 35 140;
  draw_string "Tape un nombre de rounds:";
  synchronize ();
  next_number ();;

let play_motus () = let victoire = ref 0 and defaite = ref 0 and track = ref [] in
  open_graph (string_of_int (max 200 (25 * word_length) + 100) ^ "x300");
  auto_synchronize false;

  show_intro_menu ();

  let rounds = select_rounds () in 

    while !victoire + !defaite < rounds do
      try play_word (choose_random_word ()) with
      | Won (s, a) -> (incr victoire; show_won s; track := (s, a)::!track)
      | Lost s -> (incr defaite; show_lost s; track := (s,0)::!track)
    done;

    (* print result to log *)
    print_string (
      "RESULTAT: " ^ (string_of_int !victoire) ^
      " victoires et " ^ (string_of_int !defaite) ^
      " défaites\n"
    );

  (* show summary of played rounds *)
  show_summary !track;

  wait_any_key ();
  close_graph ();

  let intentLink = generate_intent rounds !victoire !track in 
    !track, intentLink;;

(* joue au jeu :o *)
play_motus ();;