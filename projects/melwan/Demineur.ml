(* Jeu du démineur *)

(* Instructions : 
lancer la dernière instruction ecran_debut () pour commencer
cliquer sur nouvelle partie avec la souris
sélectionner le niveau de difficulté avec la lettre sur le clavier
dans une partie : 
- pour ouvrir la case : s
- pour poser un drapeau : d
- pour retirer un drapeau déjà posé : d
- pour revenir au menu après la fin de la partie : m
*)

(* Ouverture de l'interface graphique *)

Random.self_init();;
#load "graphics.cma";;
open Graphics;;
open_graph "";;

(* Préparation de la grille *)

let grille_carree n =
	let largeurFenetre = size_x() and hauteurFenetre = size_y() in
		let coteMax = n*((min (largeurFenetre*4/5) (hauteurFenetre*4/5))/n) in
			set_color 0;
			moveto 0 0;
			lineto 0 coteMax;
			moveto 0 0;
			lineto coteMax 0;
			moveto 0 coteMax;
			lineto coteMax coteMax;
			moveto coteMax 0;
			lineto coteMax coteMax;
			let cote = coteMax/n in
				for i = 0 to n-1 do
					moveto 0 (i*cote);
					lineto coteMax (i*cote);
				done;
				for i = 0 to n-1 do
					moveto (i*cote) 0;
					lineto (i*cote) coteMax;
				done;;

(* Placement des mines dans une liste *)

let contient tableau elt = 
	let booleen = ref false in
		for i = 0 to Array.length tableau do 
			if tableau.(i) = elt then booleen := true
		done;
	!booleen;;

let liste_voisins i j n =
	let liste = ref [] in
		for k=max (i-1) 0 to min(i+1) (n-1) do
			for l=max(j-1) 0 to min(j+1) (n-1) do
				if k != i || l != j then
					liste := (k,l)::(!liste);
			done;
		done;
	!liste;;

let tableau_initial n = Array.make_matrix n n 0;;

let placement_mines n m =
	let rec aux tableau n m = match m with
	| 0 -> tableau
	| a -> let x,y = Random.int n, Random.int n in
		tableau.(x).(y) <- 1;
		aux tableau n (a-1)
	in aux (tableau_initial n) n m;;

(* Placement des mines sur le graphique *)

let placement_mines_graphique tableau = 
	let n = Array.length tableau in
		set_color (rgb 0 0 0);
		clear_graph();
		grille_carree n;
		set_color (rgb 255 0 0);
		for i = 0 to n-1 do
			for j = 0 to n-1 do
				if tableau.(i).(j) = 1
				then
				let largeurFenetre = size_x() and hauteurFenetre = size_y() in
					let cote = (min (largeurFenetre*4/5) (hauteurFenetre*4/5))/n in
						fill_rect (j*cote) (i*cote) cote cote;
			done
		done;;

let mines_a_proximite tableau i j =
	let n = Array.length tableau in
	let listeVoisins = liste_voisins i j n in
		let rec aux tableau liste = match liste with
		| [] -> 0
		| t::q -> if tableau.(fst t).(snd t) = 1 then 1+aux tableau q else aux tableau q
		in aux tableau listeVoisins;;


let tableau=placement_mines 15 50;;
placement_mines_graphique tableau;;
mines_a_proximite tableau 0 12;;
placement_proximite_graphique tableau;;

let placement_proximite_graphique tableau =
	let n = Array.length tableau and largeurFenetre = size_x() and hauteurFenetre = size_y() in
		let cote = (min (largeurFenetre*4/5) (hauteurFenetre*4/5))/n in
			for i = 0 to n-1 do
				for j = 0 to n-1 do 
					moveto (j*cote+cote/2) (i*cote+cote/2);
					set_color (rgb 0 0 0);
					let mines = mines_a_proximite tableau i j in
						if mines > 0 && tableau.(i).(j) = 0 then
							set_color tableauCouleurs.(mines-1);
							draw_string (string_of_int mines);
				done;
			done;;

let tableauCouleurs = [|rgb 0 0 255; rgb 0 255 0;rgb 255 0 0;rgb 30 0 100;rgb 128 0 0;rgb 32 178 170;rgb 0 0 0;rgb 128 128 128|];;

let creation_est_revele n = Array.make_matrix n n 0;;

let rec reveler_mines tableau estRevele i j =
	let n = Array.length tableau and largeurFenetre = size_x() and hauteurFenetre = size_y() in
		let cote = (min (largeurFenetre*4/5) (hauteurFenetre*4/5))/n in
			let mines = mines_a_proximite tableau i j in
				set_color (rgb 255 255 255);
				fill_rect (j*cote+1) (i*cote+1) (cote-1) (cote-1);
				moveto (j*cote+cote/2) (i*cote+cote/2);
				estRevele.(i).(j) <- 1;
				if mines > 0 then
					set_color tableauCouleurs.(mines-1);
					draw_string (string_of_int mines);
				if mines = 0
				then
					let listeVoisins = liste_voisins i j n in
						let rec aux liste = match liste with
						| [] -> ()
						| t::q ->
							if estRevele.(fst t).(snd t) = 0 then
								reveler_mines tableau estRevele (fst t) (snd t);
								estRevele.(fst t).(snd t) <- 1;
								aux q;
						in aux listeVoisins;;		

let place_drapeau estRevele n i j =
	let largeurFenetre = size_x() and hauteurFenetre = size_y() in
		let cote = (min (largeurFenetre*4/5) (hauteurFenetre*4/5))/n in
			set_color (rgb 255 0 0);
			draw_circle (j*cote + cote/2) (i*cote + cote/2) (cote/3);
			estRevele.(i).(j) <- 2;;

let retire_drapeau estRevele n i j =
	let largeurFenetre = size_x() and hauteurFenetre = size_y() in
		let cote = (min (largeurFenetre*4/5) (hauteurFenetre*4/5))/n in
			set_color (rgb 200 200 200);
			fill_rect (j*cote+1) (i*cote+1) (cote-1) (cote-1);
			estRevele.(i).(j) <- 0;;
			


nouvelle_partie 15 50;;

let nouvelle_partie n m difficulte scoreD scoreA scoreE =
	clear_graph();
	let largeurFenetre = size_x() and hauteurFenetre = size_y() in
		let cote  = (min (largeurFenetre*4/5) (hauteurFenetre*4/5))/n in
			set_color (rgb 200 200 200);
			fill_rect 0 0 (n*cote) (n*cote);
			grille_carree n;
			moveto (largeurFenetre *81/100) (hauteurFenetre*98/100);
			draw_string "Progression";
			moveto (largeurFenetre *81/100) (hauteurFenetre*90/100);
			draw_string "Mines à placer";
			moveto (largeurFenetre*93/100) (hauteurFenetre*94/100);
			draw_string "%";
			moveto (largeurFenetre*81/100) (hauteurFenetre*82/100);
			draw_string "Score";
			let tableauMines = placement_mines n m and estRevele = creation_est_revele n in
			let partieReussie = ref 0 in
				while !partieReussie = 0 do
					let attends =wait_next_event[Key_pressed] in
						let coordX=attends.mouse_x and coordY = attends.mouse_y in
							let j,i = (coordX/cote),(coordY/cote) in
								if String.make 1 attends.key = "s" then
									if tableauMines.(i).(j) = 1 then
										begin
											partieReussie := -1;
											set_color (rgb 255 0 0);
											fill_rect (j*cote+1) (i*cote+1) (cote-1) (cote-1);
										end
									else reveler_mines tableauMines estRevele i j;
									let reveles = ref 0 and nombreMines = ref 0 and drapeaux=ref 0 in
											begin
												for i = 0 to n-1 do
													for j = 0 to n-1 do
															if estRevele.(i).(j)=1 || estRevele.(i).(j)=2
																then incr reveles;
															if tableauMines.(i).(j)=1 
																then incr nombreMines;
															if estRevele.(i).(j)=2 
																then incr drapeaux;
													done;
												done;
												end;
												if !reveles = n*n && drapeaux=nombreMines then partieReussie :=1
												else if String.make 1 attends.key = "d" then
												if estRevele.(i).(j)=2 then
												begin
													retire_drapeau estRevele n i j
												end
												else 
													if estRevele.(i).(j) = 0 then
														place_drapeau estRevele n i j;
												let score = ((!reveles-(!drapeaux))*3+((!drapeaux)*2))*(n/m*difficulte) in
													set_color (rgb 255 255 255);
													fill_rect (largeurFenetre*85/100) (hauteurFenetre*94/100) (largeurFenetre*8/100) (hauteurFenetre*4/100);
													fill_rect (largeurFenetre*85/100) (hauteurFenetre*86/100) (largeurFenetre*8/100) (hauteurFenetre*4/100);
													fill_rect (largeurFenetre*90/100) (hauteurFenetre*78/100) (largeurFenetre*10/100) (hauteurFenetre*4/100);
													set_color 0;
													moveto (largeurFenetre*90/100) (hauteurFenetre*94/100);
													draw_string (string_of_int ((100*(!reveles))/(n*n)));
													moveto (largeurFenetre*90/100) (hauteurFenetre*86/100);
													draw_string (string_of_int ((!nombreMines)-(!drapeaux)));
													moveto (largeurFenetre*90/100) (hauteurFenetre*78/100);
													draw_string (string_of_int score);
													if difficulte = 1 then
														begin
															scoreD := max (!scoreD) score;
														end;
													else if difficulte = 2 then
															scoreA := max (!scoreA) score;
													else
															scoreE := max (!scoreE) score;
														
				done;
				clear_graph ();
				moveto (largeurFenetre*2/5) (hauteurFenetre*48/100);
				if !partieReussie=1 then
					begin
						set_color (rgb 0 255 0);
						draw_string "Bravo,c'est gagné !";
					end
				else
					begin
						set_color (rgb 255 0 0);
						draw_string "Oh non, c'est perdu !";
					end;
				let enregistrement=wait_next_event[Key_pressed] in
						if String.make 1 enregistrement.key = "m" then ecran_debut ();;

let ecran_debut scoreD scoreA scoreE =
	open_graph "";
	clear_graph();
	let largeurFenetre = size_x() and hauteurFenetre = size_y() in
		set_color 0;
		moveto (largeurFenetre*2/5) (hauteurFenetre*9/20);
		lineto (largeurFenetre*3/5) (hauteurFenetre*9/20);
		lineto (largeurFenetre*3/5) (hauteurFenetre*11/20);
		lineto (largeurFenetre*2/5) (hauteurFenetre*11/20);
		lineto (largeurFenetre*2/5) (hauteurFenetre*9/20);
		moveto (largeurFenetre*41/100) (hauteurFenetre*48/100);
		draw_string "Novuelle partie";
		moveto (largeurFenetre*2/5) (hauteurFenetre*9/10);
		draw_string "Jeu du Démineur";
		let enregistrement = wait_next_event[Button_up] in
			let coordX = enregistrement.mouse_x and coordY = enregistrement.mouse_y in
				if (largeurFenetre*2/5) <= coordX && coordX <= (largeurFenetre*3/5) && (hauteurFenetre*9/20) <= coordY && coordY <= (hauteurFenetre*11/20)
				then
					clear_graph();
					let largeurFenetre = size_x() and hauteurFenetre = size_y() in
					set_color 0;
					moveto (largeurFenetre*2/5) (hauteurFenetre*9/10);
					draw_string "Sélectionnez la difficulté";
					for i = 0 to 2 do
						moveto (largeurFenetre*(1+3*i)/10) (hauteurFenetre*9/20);
						lineto (largeurFenetre*(1+3*i)/10) (hauteurFenetre*11/20);
						lineto (largeurFenetre*3*(i+1)/10) (hauteurFenetre*11/20);
						lineto (largeurFenetre*3*(i+1)/10) (hauteurFenetre*9/20);
						lineto (largeurFenetre*(1+3*i)/10) (hauteurFenetre*9/20);
					done; 
					moveto (largeurFenetre*12/100) (hauteurFenetre*48/100);
					draw_string "Débutant [d]";
					moveto (largeurFenetre*42/100) (hauteurFenetre*48/100);
					draw_string "Avancé [a]";
					moveto (largeurFenetre*72/100) (hauteurFenetre*48/100);
					draw_string "Expert [e]";
					(* retirer ce qui suit à la première évaluation, sinon ça plante :( *)
					let enregistrement = wait_next_event[Key_pressed] in
						let aux lettre = match lettre with
						| "d" -> nouvelle_partie 9 15 1
						| "a" -> nouvelle_partie 16 40 2
						| "e" -> nouvelle_partie 30 100 3
						| _ -> ()
						in aux (String.make 1 enregistrement.key);;
					
					
					
					
ecran_debut ();;

(* Il reste l'implémentation du "high score" *);;
