#load "graphics.cma";;
open Graphics;;
auto_synchronize true ;;



(* Fonction du systeme *)

let affiche_matrice matrice =
	for i = 0 to 3 do
		for j = 0 to 3 do
			print_int matrice.(i).(j) ;
			print_string " " ;
		done ;
		print_newline ();
		done ;;

let copie matrice =
	let nouvelleMatrice = [| [|0;0;0;0|];[|0;0;0;0|];[|0;0;0;0|];[|0;0;0;0|] |] in
	for i = 0 to 3 do
		for j = 0 to 3 do
			nouvelleMatrice.(i).(j) <- matrice.(i).(j)
		done ;
	done ;
	nouvelleMatrice ;;
	
let test_egalite m1 m2 =
	let egalite = ref true in
	for i = 0 to 3 do
		for j = 0 to 3 do
			if m1.(i).(j) <> m2.(i).(j) then begin egalite := false end
		done ;
	done ;
	!egalite ;;
	
let transposee matrice =
	for i = 0 to 3 do
		for j = i to 3 do
			let aux = matrice.(i).(j) in
			matrice.(i).(j) <- matrice.(j).(i) ;
			matrice.(j).(i) <- aux ;
		done ;
	done ;;

let possibilite_mouvement_ligne matrice =
	let nbrLignesPleines = ref 0 in
	for i = 0 to 3 do
		begin
		let l = matrice.(i) in
		let ligne_pleine = ref 0 in
		for j = 0 to 3 do
			match j with
			| 3 -> if l.(j) <> 0 then ligne_pleine := !ligne_pleine + 1
			| _ -> if l.(j) <> 0 && l.(j+1) <> 0 && l.(j) <> l.(j+1) then ligne_pleine := !ligne_pleine + 1
		done ;
		if !ligne_pleine = 4 then nbrLignesPleines := !nbrLignesPleines + 1
		end
	done ;
	if !nbrLignesPleines = 4 then false else true ;;

let rec trouve_prochain_non_nulG ligne i = match i, ligne.(i) with
	| 3, 0 -> 1
	| _, 0 -> trouve_prochain_non_nulG ligne (i+1)
	| _, _ -> i ;;

let rec trouve_prochain_non_nulD ligne i = match i, ligne.(i) with
	| 0, 0 -> 1
	| _, 0 -> trouve_prochain_non_nulD ligne (i-1)
	| _, _ -> i ;;

(* Fonction qui crée une nouvelle case de manière aléatoire *)
	
let valeur_au_hasard () = let n = Random.int 10 in match n with
	| 0 -> 4
	| _ -> 2 ;;
	(* Après plusieurs esais sur le site officiel, la proportion semble être de 1 pour 10 *)
	
					
let rec place matrice n = match n with
	| 0 -> ()
	| 1 -> 	let l = Random.int 4 in
				let c = Random.int 4 in
				if matrice.(l).(c) = 0
				then 
				begin 
				matrice.(l).(c) <- valeur_au_hasard () ;
				place matrice 0 ;
				end
				else (place matrice 1)
	| 2 ->	let l = Random.int 4 in
				let c = Random.int 4 in
				if matrice.(l).(c) = 0
				then begin matrice.(l).(c) <- valeur_au_hasard () ; place matrice 1 end
				else place matrice 2
	| _ ->  	failwith "personne n'ajoute autant de cases quand même " ;;

(* Fonction définissant les déplacements *)

(*Gauche*)
let sommeG l i =
		let j = trouve_prochain_non_nulG l (i+1) in
		if i<>j && l.(i) = l.(j) then
			begin
			l.(i) <- 2 * l.(i) ;
			l.(j) <- 0 ; 
			end	;;		
let gauche_ligne l =
	for i = 0 to 2 do	
		if l.(i) = 0
		then
		begin
		let k = ref i in
		while !k < 4 && l.(i) = 0 do
			for j = i to 2 do
				l.(j) <- l.(j+1) ;
			done ;
			l.(3) <- 0 ;
			k := !k + 1;
		done ;
		if l.(i) <> 0 then
		begin
		sommeG l i ;
		end
		end
		else
		begin
		sommeG l i
		end
	done ;
	l ;;
		
let gauche matrice =
		begin
		for i = 0 to 3 do
			matrice.(i) <- (gauche_ligne matrice.(i))
		done ;
		end ;;

(*Droite*)
let sommeD l i =
		let j = trouve_prochain_non_nulD l (i-1) in
		if i<>j && l.(i) = l.(j) then
			begin
			l.(i) <- 2 * l.(i) ;
			l.(j) <- 0 ;
			end	;;
let droite_ligne l =
	for i = 0 to 2 do	
		if l.(3-i) = 0
		then
		begin
		let k = ref (3-i) in
		while !k >= 0 && l.(3-i) = 0 do
			for j = i to 2 do
				l.(3-j) <- l.(3-j-1) ;
			done ;
			l.(0) <- 0 ;
			k := !k - 1;
		done ;
		if l.(3-i) <> 0 then
		begin
		sommeD l (3-i) ;
		end
		end
		else
		begin
		sommeD l (3-i)
		end
	done ;
	l ;;
	
let droite matrice =
		begin
		for i = 0 to 3 do
			matrice.(i) <- (droite_ligne matrice.(i))
		done ;
		end ;;	
	
(*Haut*)
let haut matrice =
	transposee matrice ;
	gauche matrice ;
	transposee matrice ;;

(*Bas*)
let bas matrice =
	transposee matrice ;
	droite matrice ;
	transposee matrice ;;

(* Détection de Défaite ou Victoire *)
let perdu matrice =
	let perduHorizontal = possibilite_mouvement_ligne matrice in
	transposee matrice ;
	let perduVertical = possibilite_mouvement_ligne (matrice) in
	transposee matrice ;
	match perduHorizontal, perduVertical with
	| false, false -> true
	| _ , _ -> false ;;
	
let detecte_Victoire matrice =
	let victoire = ref false in
	for i = 0 to 3 do
		for j = 0 to 3 do
			if matrice.(i).(j) = 2048
			then victoire := true
		done ;
	done ;
	!victoire ;;

(* Fonctions de représentation Graphique *)

let trace_grille () =
set_color white ;
fill_rect 0 0 500 500 ;
set_color black ;
	moveto 20 20 ;
	for i = 0 to 4 do
		lineto 	(20 + i*101) (20 + 404) ;
		moveto	(20 + (i+1) * 101) (20) ;
	done ;
	moveto 20 20 ;
	for i = 0 to 4 do
		lineto 	(20 + 404) (20 + i * 101) ;
		moveto	(20) (20 + (i+1) * 101) ;
	done ;;

(* Quelques fonctions auxiliaires *)
let trace_rectangle i j color =
	moveto i j ;
	set_color color ;
	fill_rect i j 100 100 ;;
	
let trace_chiffre i j color chiffre =
	moveto (i+50) (j + 50) ;
	set_color color ;
	draw_string chiffre ;;

(* Représente la matrice dans la grille *)
let affiche_grille matrice =
	auto_synchronize false ;
	open_graph "500x500";
	moveto 21 21 ;
	for i = 0 to 3 do
		for j = 0 to 3 do
			match matrice.(i).(j) with
			| 0 -> 	trace_rectangle (21 + j * 101) (324 - i * 101 ) (rgb 255 255 255)
						
			| 2 -> 	trace_rectangle (21 + j * 101) (324 - i * 101 ) (rgb 255 255 204) ;
						trace_chiffre (21 + j * 101) (324 - i * 101 ) (rgb 0 0 0) "2"
			| 4 -> trace_rectangle (21 + j * 101) (324 - i * 101 ) (rgb 255 255 153) ;
						trace_chiffre (21 + j * 101) (324 - i * 101 ) (rgb 0 0 0) "4"
			| 8 -> trace_rectangle (21 + j * 101) (324 - i * 101 ) (rgb 255 204 153) ;
						trace_chiffre (21 + j * 101) (324 - i * 101 ) (rgb 0 0 0) "8"
			| 16 -> trace_rectangle (21 + j * 101) (324 - i * 101 ) (rgb 255 153 51) ;
						trace_chiffre (21 + j * 101) (324 - i * 101 ) (rgb 0 0 0) "16"
			| 32 -> trace_rectangle (21 + j * 101) (324 - i * 101 ) (rgb 255 102 102) ;
						trace_chiffre (21 + j * 101) (324 - i * 101 ) (rgb 0 0 0) "32"
			| 64 -> trace_rectangle (21 + j * 101) (324 - i * 101 ) (rgb 255 51 51) ;
						trace_chiffre (21 + j * 101) (324 - i * 101 ) (rgb 0 0 0) "64"
			| 128 -> trace_rectangle (21 + j * 101) (324 - i * 101 ) (rgb 255 255 51) ;
						trace_chiffre (21 + j * 101) (324 - i * 101 ) (rgb 0 0 0) "128"
			| 256 -> trace_rectangle (21 + j * 101) (324 - i * 101 ) (rgb 255 255 0) ;
						trace_chiffre (21 + j * 101) (324 - i * 101 ) (rgb 0 0 0) "256"
			| 512 -> trace_rectangle (21 + j * 101) (324 - i * 101 ) (rgb 255 255 0) ;
						trace_chiffre (21 + j * 101) (324 - i * 101 ) (rgb 0 0 0) "512"
			| 1024 -> trace_rectangle (21 + j * 101) (324 - i * 101 ) (rgb 204 204 0) ;
						trace_chiffre (21 + j * 101) (324 - i * 101 ) (rgb 0 0 0) "1024"
			| 2048 -> trace_rectangle (21 + j * 101) (324 - i * 101 ) (rgb 0 0 0) ;
						trace_chiffre (21 + j * 101) (324 - i * 101 ) (rgb 0 0 0) "2048"
		done ;
	done ;
	synchronize () ;;


			
(* Le jeu *)
let jeu () =
	open_graph "500x500";
	trace_grille () ;
	let matrice = [| [|0;0;0;0|] ; [|0;0;0;0|] ; [|0;0;0;0|] ; [|0;0;0;0|] |] in
	place matrice 2 ;
	affiche_matrice matrice ;
	synchronize () ;
	print_newline () ;
	let continu = ref 0 in
	while !continu = 0 do
		if key_pressed ()
		then
		let touche = read_key () in
		begin
		match touche with
		| 'z' -> let testEgalite = copie matrice in
					haut testEgalite ;
					if not (test_egalite testEgalite matrice) then
						begin
						haut matrice ;
						place matrice 1 ;
						affiche_matrice matrice ;
						print_newline () ;
						affiche_grille matrice ;
						trace_grille () ;
						begin if perdu matrice = true then continu := 1 end ;
						begin if detecte_Victoire matrice then continu := 2 end ;
						end
					else ()
		
		| 'q' -> let testEgalite = copie matrice in
					gauche testEgalite ;
					if not (test_egalite testEgalite matrice) then
						begin
						gauche matrice ;
						place matrice 1 ;
						affiche_matrice matrice ;
						print_newline () ;
						affiche_grille matrice ;
						trace_grille () ;
						begin if perdu matrice = true then continu := 1 end ;
						begin if detecte_Victoire matrice then continu := 2 end ;
						end
					else ()
		
		| 's' -> let testEgalite = copie matrice in
					bas testEgalite ;
					if not (test_egalite testEgalite matrice) then
						begin
						bas matrice ;
						place matrice 1 ;
						affiche_matrice matrice ;
						print_newline () ;
						affiche_grille matrice ;
						trace_grille () ;
						begin if perdu matrice = true then continu := 1 end ;
						begin if detecte_Victoire matrice then continu := 2 end ;
						end
					else ()
		
		| 'd' -> let testEgalite = copie matrice in
					droite testEgalite ;
					if not (test_egalite testEgalite matrice) then
						begin
						droite matrice ;
						place matrice 1 ;
						affiche_matrice matrice ;
						print_newline () ;
						affiche_grille matrice ;
						trace_grille () ;
						begin if perdu matrice = true then continu := 1 end ;
						begin if detecte_Victoire matrice then continu := 2 end ;
						end
					else ()
		
		| _ -> ()
		end
	done ;
	set_color white ;
	fill_rect 0 0 500 500 ;
	set_color black ;
	moveto 230 240 ;
	if !continu = 1 
	then begin
	draw_string "GAME OVER" ;
	print_newline ();
	print_string "GAME OVER" end
	else begin
	draw_string "You win !" ;
	print_newline ();
	print_string "YOU win !" end ;
	synchronize () ;;
	
	
jeu () ;;
