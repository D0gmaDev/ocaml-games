#load "graphics.cma";;
open Graphics;;

(* Je m'inspire pour ce jeu des jeux de rythmes classiques de type "piano" du même genre que osu!mania,
soundvoltex, piano tiles, etc.
A la base je voulais faire quelque chose qui ressemblerait à osu!, le jeu avec des cercles mais vous
avez fait un truc semblable avant.... l'idée a donc été abandonnée :( 
Je suis un amateur de jeux de ryhtme !! *)

let vitesse = 999.;;

(* les tuiles sont modélisées par des rectangles noirs de dimension 70x30 *)
type tuile = { colonne : int ; (*timing : float ;*) mutable height : float };;

exception MapFinie;;


(* fais apparaître une tuile à la ième colonne et la fait tomber jusqu'à atteindre la zone limite. *)

let tuile_qui_tombe colonne =
	open_graph "1000x620";
	auto_synchronize false;
	let datePrecedente = ref (Sys.time ()) 
	and position = ref 590. in
	
	while !position > 110. do (* une fois que height = 110, la tuile s'arrête *)
		let present = Sys.time() in
		let tempsEcoule = present -. !datePrecedente in
			datePrecedente := present;
			position := !position -. tempsEcoule *. vitesse;

		set_color (rgb 225 255 255);
		fill_rect (350+(colonne-1)*80) 110 70 510;
		set_color (rgb 0 0 0);
		fill_rect (350+(colonne-1)*80) (int_of_float !position) 70 30;
		synchronize();
	done;;

(* tuile_qui_tombe 1;; *)


(* la map est le patterne qui sera joué, liste de n lignes qui sont des listes de 4 éléments de 0 ou 1.
Le but étant que lorsque la ligne comporte un 1, une tuile est affichée. Si elle en comporte plusieurs,
les tuiles sont affichées sur une même ligne *)

let creer_map nb_lignes =
	if nb_lignes = 0 then [[];[];[];[]] (*MapFinie*)
	else
	let rec aux nb_lignes_restantes l = match nb_lignes_restantes with (*créer les 4-listes de 0 et 1*)
	| 0 -> l
	| _ -> let rec aux2 k ligne = match k with
			 | 0 -> ligne
			 | _ -> aux2 (k-1) ((Random.int 2)::ligne)
			 in aux (nb_lignes_restantes-1) ((aux2 4 [])::l)
	in aux nb_lignes [];;

creer_map 10;;
creer_map 3;;


(* cette fonction convertit la map qui est un int list list en tuile list qui me semble plus 
simple à manipuler avec les informations contenues dans le type.
Les tuiles sont séparées d'une hauteur de 300 entre elles.
La première tuile est à la hauteur 990 pour pas que la map se lance trop brutalement ensuite *)

let map_convert map =
	let rec aux m liste hauteur = match m with
	| [] -> liste
	| t::q -> let rec aux2 c ligne tuiles = match ligne with
				 | [] -> tuiles
				 | 1::q -> aux2 (c+1) q ({colonne = c ; height = 990. +. (float_of_int hauteur)}::tuiles)
				 | 0::q -> aux2 (c+1) q tuiles
				 | _::q -> failwith "ne doit pas arriver"
				 in aux q ((aux2 1 t [])@liste) (hauteur+300)
	in List.rev(aux map [] 0);;

map_convert [[0; 0; 1; 0]; [1; 0; 0; 1]; [1; 1; 1; 1]];;
let exemple = [{colonne = 3; height = 590.}; {colonne = 1; height = 840.};
				  {colonne = 4; height = 840.}; {colonne = 1; height = 1090.};
				  {colonne = 2; height = 1090.}; {colonne = 3; height = 1090.};
				  {colonne = 4; height = 1090.}];;


(* fonction qui range la tuile list en tuile list list par colonne *)

let rec arrange_tuiles map =
	let rec aux1 i l = match i with
	| k when k<5 -> let rec aux2 m c = match m with
						 | [] -> c
						 | t::q -> if t.colonne = i then aux2 q (t::c) else aux2 q c
						 in aux1 (i+1) ((List.rev(aux2 map []))::l) (*List.rev -> ordre croissant en height*)
	| _ -> l
	in List.rev(aux1 1 []);; (*List.rev -> ordre croissant en colonne*)

arrange_tuiles exemple;;
let exemple_arrange = [[{colonne = 1; height = 840.}; {colonne = 1; height = 1090.}];
							  [{colonne = 2; height = 1090.}];
							  [{colonne = 3; height = 590.}; {colonne = 3; height = 1090.}];
							  [{colonne = 4; height = 840.}; {colonne = 4; height = 1090.}]];;


(* affiche sur les tuiles d'une tuile list list *)

let rec affiche_map map_tuiles = match map_tuiles with
| [] -> plot 0 0
| t::q -> let rec aux c = match c with
			 | [] -> plot 0 0
			 | tc::qc -> fill_rect (350+(tc.colonne-1)*80) (int_of_float tc.height) 70 30;
							 aux qc
			 in aux t;
			 affiche_map q;;


(* open_graph "1000x620";;
affiche_map (arrange_tuiles [{colonne = 3; height = 100.}; {colonne = 1; height = 300.};
 {colonne = 4; height = 300.}; {colonne = 1; height = 500.};
 {colonne = 2; height = 500.}; {colonne = 3; height = 500.};
 {colonne = 4; height = 500.}]);; *)


(* met à jour la position verticale des tuiles en la réduisant de x=defilement pour les listes de tuiles
arrangées en colonne *)

let update_map map defilement =
	let rec aux1 liste new_map = match liste with
	| [] -> new_map
	| t::q -> let rec aux2 l1 l2 = match l1 with
				 | [] -> l2
				 | t1::q1 -> aux2 q1 ({colonne = t1.colonne ; height = t1.height -. defilement}::l2)
				 in aux1 q ((List.rev(aux2 t []))::new_map)
	in List.rev(aux1 map []);;

update_map (arrange_tuiles exemple) 300.;;


let enleve_premier_et_renvoie map c = match map with
(*décompose la map en tuile * tuile list list soit en (tuile,map_reste) où map_reste est map privé de
tuile dans la colonne corespondante*)
| [] -> failwith "ne doit jamais arriver" (*ici une map est forcément une tuile list list*)
| [[];[];[];[]] -> raise MapFinie
| colonne1::colonne2::colonne3::colonne4::[] ->
	if c = 1 then match colonne1 with (*on cherche à quelle colonne on veut retirer la première tuile*)
					  | [] -> ({colonne = c; height = 1.},map) (*tuile par défaut*)
					  | t::q -> (t,q::colonne2::colonne3::colonne4::[])
	else if c = 2 then match colonne2 with
					       | [] -> ({colonne = c; height = 1.},map)
							 | t::q -> (t,colonne1::q::colonne3::colonne4::[])
		  else if c = 3 then match colonne3 with
									| [] -> ({colonne = c; height = 1.},map)
									| t::q -> (t,colonne1::colonne2::q::colonne4::[])
				 else if c = 4 then match colonne4 with
										  | [] -> ({colonne = c; height = 1.},map)
										  | t::q -> (t,colonne1::colonne2::colonne3::q::[])
						else ({colonne = c ; height = 1. },map)
| _ -> ({colonne = c ; height = 1. },map);;


enleve_premier_et_renvoie exemple_arrange 4;;
let a =[[{colonne = 1; height = 1000.}];
  [];
  [{colonne = 3; height = 590.}; {colonne = 3; height = 1090.}];
  [{colonne = 4; height = 1090.}]];;
enleve_premier_et_renvoie a 1;;


(*le but est d'afficher la map en entier directement puis de faire "tomber" la map au fur et à mesure*)

let defile_map map =
	open_graph "1000x620";
	auto_synchronize false;
	let datePrecedente = ref (Sys.time ())
	and defilement = ref 0. in
	
	while true do
		let present = Sys.time() in
		let tempsEcoule = present -. !datePrecedente in
			datePrecedente := present;
			defilement := !defilement +. tempsEcoule *. vitesse;
			
		let new_map = update_map map !defilement in 
		
			set_color (rgb 225 255 255); (*je ne fais pas clear_graph pour garder mes éléments autour *)
			for i=0 to 3 do
				fill_rect (350+i*80) 0 70 620;
			done;
			
			set_color (rgb 0 0 0);
			affiche_map new_map;
			synchronize();
	done;;

arrange_tuiles (map_convert (creer_map 200));;
(* defile_map (arrange_tuiles (map_convert (creer_map 200)));; *)


let decompte() =
	open_graph "1000x620";
	let datePrecedente = ref (Sys.time ()) in
   let decompte = ref 3 in (* decompte en seconde (3) *)
   let tE = ref 0. in
   set_font "segoe";
   set_color (rgb 0 0 0);
	while !decompte <> 0 do
		moveto ((size_x()/2)-12) ((size_y()/2)-18);
		set_text_size 100;
		draw_string (string_of_int !decompte);
		while !tE < 1. do
			tE := (Sys.time()) -. !datePrecedente;
		done;
		clear_graph();
		tE := 0.;
		decompte := !decompte - 1;
		datePrecedente := Sys.time();
	done;;

(* decompte();; *)


let jeu() =
	open_graph "1000x620";
	decompte();
	auto_synchronize false;
	let datePrecedente = ref (Sys.time ()) in
	
	let map = ref (arrange_tuiles (map_convert (creer_map 200)))
	and score = ref 0
	and erreur = ref 0
	and defilement = ref 0. in
	
	while true do
		
		set_color (rgb 255 255 255);
		fill_rect 0 0 300 300; (*équivalent à un clear_graph sur x=0 y=0 dans une zone de 300x300*)
		set_color (rgb 0 0 0);
		set_font "calibri";
		set_text_size 30;
		moveto 100 10;
		draw_string (string_of_int !score);
		moveto 100 40;
		draw_string (string_of_int !erreur);
		moveto 20 10;
		draw_string "score: ";
		moveto 20 40;
		draw_string "erreur: ";
		set_color (rgb 0 0 0); (*élements décoratifs*)
		fill_rect 338 0 2 620;
		fill_rect 670 0 2 620;
		
		let present = Sys.time() in
			let tempsEcoule = present -. !datePrecedente in
				datePrecedente := present;
				defilement := !defilement +. tempsEcoule *. vitesse;
		
		(* affichage de la map *)
			   map:=(update_map !map !defilement);
			   defilement := 0.;
				set_color (rgb 225 255 255); (*je ne fais pas clear_graph pour garder mes éléments autour *)
				for i=0 to 3 do
					fill_rect (350+i*80) 0 70 620;
				done;
				set_color (rgb 0 0 0);
				affiche_map !map;
				set_color (rgb 255 82 82); (* zone "ok" = [80,150] *)
				fill_rect 338 80 3 100;
				fill_rect 338 80 331 3;
				fill_rect 669 80 3 100;
				fill_rect 338 180 334 3;
				synchronize();
		
		(*gameplay*)
		let t1,reste1 = enleve_premier_et_renvoie !map 1
		and t2,reste2 = enleve_premier_et_renvoie !map 2
		and t3,reste3 = enleve_premier_et_renvoie !map 3
		and t4,reste4 = enleve_premier_et_renvoie !map 4
		and evenement = wait_next_event [Poll; Key_pressed] in
		
		(* lorsque une touche est appuyé (d,f,j ou k), une tuile est supprimé dans la colonne
		correspondante et si la tuile est dans la zone "ok", le score augmente de 1 sinon erreur augmente
		de 1,
		si les tuiles vont en dessous de la zone "ok" (50 car les tuiles sont de hauteur 30 et la zone est
		à la hauteur 70) elles disparaissent et une erreur est commise *)
			if t1.height<20. then
				map:= reste1;
			if t2.height<20. then
				map:= reste2;
			if t3.height<20. then
				map:= reste3;
			if t4.height<20. then
				map:= reste4;
			
			(*lorsque je fais générer une erreur ici, ça fait un truc plutôt étrange avec cette portion de code*)
(*			if t1.height<20. then
				begin
					map:= reste1;
					erreur:= erreur+1;
				end;
			if t2.height<20. then
				begin
					map:= reste2;
					erreur:= erreur+1;
				end;
			if t3.height<20. then
				begin
					map:= reste3;
					erreur:= erreur+1;
				end;
			if t4.height<20. then
				begin
					map:= reste4;
					erreur:= erreur+1;
				end;
*)
			
			if evenement.keypressed then
			
				let touche = read_key() in
				
							match touche with
							| 'd' -> map := reste1;
										if t1.height > 50. && t1.height < 180. then
											score:= !score +1
										else
											erreur:= !erreur +1;
							
							| 'f' -> map := reste2;
										if t2.height > 50. && t2.height < 180. then
											score:= !score +1
										else
											erreur:= !erreur+1;
							
							| 'j' -> map := reste3;
										if t3.height > 50. && t3.height < 180. then
											score:= !score +1
										else
											erreur:= !erreur+1;
							
							| 'k' -> map := reste4;
										if t4.height > 50. && t4.height < 180. then
											score:= !score +1
										else
											erreur:= !erreur+1;
							
							| _ -> map := !map;
		
		if !map = [[];[];[];[]] then raise MapFinie
		
	done;;

(*il y a au max 200x4 tuiles dans la map ici donc le score max vaut 800*)
jeu();;

