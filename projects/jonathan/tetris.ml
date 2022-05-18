(* Exercice 6 - Tétris *)
(* ------------------- *)

(* Commandes du jeu *)
(*
z : faire une rotation horaire
q : aller à gauche
s : aller en bas
d : aller à droite
p : mettre sur pause / sortir de pause
o : quitter
*)



(* Modules *)

#use "topfind" ;;
#require "graphics" ;;
open Graphics ;;
    


(* Couleurs *)

let couleurs = [|rgb 0 0 0 ;           (* Noir *)
                 rgb 255 0 0 ;         (* Rouge *)
                 rgb 255 165 0 ;       (* Orange *)
                 rgb 255 255 0 ;       (* Jaune *)
                 rgb 0 128 0 ;         (* Vert *)
                 rgb 0 255 255 ;       (* Cyan *)
                 rgb 0 0 255 ;         (* Bleu *)
                 rgb 128 0 128 ;       (* Violet *)
                 rgb 255 255 255 |] ;; (* Blanc *)



(* Strcutures de données *)

type tetrimino = {mutable ligne : int ;
                  mutable colonne : int ;
                  mutable forme : int array array} ;;

type etat = {puits : int array array ;
             mutable tetrimino : tetrimino ;
             mutable score : int ;
             mutable niveau : int ;
             mutable nombreLignes : int} ;;



(* Champs de jeu *)

let genere_puits nbLignes nbColonnes = Array.make_matrix nbLignes nbColonnes 0 ;;



(* Affichage *)

let affiche_etat etat =
    let nbLignes = Array.length etat.puits and nbColonnes = Array.length etat.puits.(0) in
    let unite = min (((size_y ()) - 36) / nbLignes) (size_x () / nbColonnes) in
    let x = (size_x () - nbColonnes * unite) / 2 and y = (size_y () - 36 - nbLignes * unite) / 2 in
    let largeur = nbColonnes * unite and hauteur = nbLignes * unite in
    auto_synchronize false ;  (* pour éviter les scintillements de l'image,
    on ne rafraîchit l'écran qu'au moment du synchronize *)
    clear_graph () ;
    (* Affichage du puits *)
    for i = 0 to nbLignes - 1 do
        for j = 0 to nbColonnes - 1 do
            set_color couleurs.(etat.puits.(i).(j)) ;
            fill_rect (x + j * unite)
                      (y + hauteur - (i + 1) * unite)
                      unite unite
        done
    done ;
    (* Affichage du tetrimino actif *)
    for i = 0 to Array.length etat.tetrimino.forme - 1 do
        for j = 0 to Array.length etat.tetrimino.forme.(0) - 1 do
            if etat.tetrimino.forme.(i).(j) <> 0
            then begin
                 set_color couleurs.(etat.tetrimino.forme.(i).(j)) ;
                 fill_rect (x + (etat.tetrimino.colonne + j) * unite)
                           (y + hauteur - (etat.tetrimino.ligne + i + 1) * unite)
                           unite unite
                 end
        done
    done ;
    (* Affichage du quadrillage *)
    for i = 0 to nbLignes do
        set_color couleurs.(8) ;
        moveto x (y + i * unite) ;
        lineto (x + largeur) (y + i * unite) ;
    done ;
    for j = 0 to nbColonnes do
        set_color couleurs.(8) ;
        moveto (x + j * unite) y ;
        lineto (x + j * unite) (y + hauteur) ;
    done ;
    (* Affichage du score, du niveau et du nombre de lignes *)
    set_color couleurs.(0) ;
    moveto x (y + hauteur) ;
    draw_string ("Lignes : " ^ string_of_int etat.nombreLignes) ;
    moveto x (y + hauteur + 12) ;
    draw_string ("Niveau : " ^ string_of_int etat.niveau) ;
    moveto x (y + hauteur + 24) ;
    draw_string ("Score : " ^ string_of_int etat.score) ;
    (* Rafraichissement de l'écran *)
    synchronize () ;;



(* Opérations sur le puits *)

let lignes_completes puits =
    let rec parcourt_lignes lignesASupprimer i =
        if i < Array.length puits
        then let rec parcourt_colonnes j =
             if j < Array.length puits.(i)
             then if puits.(i).(j) = 0
                  then parcourt_lignes lignesASupprimer (i + 1)
                  else parcourt_colonnes (j + 1)
             else parcourt_lignes (i::lignesASupprimer) (i + 1)
             in parcourt_colonnes 0
        else lignesASupprimer
    in parcourt_lignes [] 0 ;;

let supprime_lignes etat lignes =
    let rec sup_lignes liste_lignes = match liste_lignes with
      | t::q -> etat.puits.(t) <- Array.make (Array.length etat.puits.(t)) 0 ;
                sup_lignes q
      |  []  -> affiche_etat etat
    in sup_lignes lignes ;
    let rec deplace_lignes liste_lignes k = match liste_lignes with
      | t::q -> for i = 0 to t + k - 1 do
                    etat.puits.(t + k - i) <- etat.puits.(t + k - i - 1)
                done ;
                deplace_lignes q (k + 1)
      |  []  -> for i = 0 to k - 1 do
                    etat.puits.(i) <- Array.make (Array.length etat.puits.(i)) 0
                done
    in deplace_lignes lignes 0 ;
    affiche_etat etat ;;



(* Tétriminos *)

let tetriminoZ = Array.make_matrix 3 3 0 ;;
tetriminoZ.(0).(0) <- 1 ;;
tetriminoZ.(0).(1) <- 1 ;;
tetriminoZ.(1).(1) <- 1 ;;
tetriminoZ.(1).(2) <- 1 ;;

let tetriminoL = Array.make_matrix 3 3 0 ;;
tetriminoL.(0).(2) <- 2 ;;
tetriminoL.(1).(0) <- 2 ;;
tetriminoL.(1).(1) <- 2 ;;
tetriminoL.(1).(2) <- 2 ;;

let tetriminoO = Array.make_matrix 2 2 3 ;;

let tetriminoS = Array.make_matrix 3 3 0 ;;
tetriminoS.(0).(1) <- 4 ;;
tetriminoS.(0).(2) <- 4 ;;
tetriminoS.(1).(0) <- 4 ;;
tetriminoS.(1).(1) <- 4 ;;

let tetriminoI = Array.make_matrix 4 4 0 ;;
tetriminoI.(1).(0) <- 5 ;;
tetriminoI.(1).(1) <- 5 ;;
tetriminoI.(1).(2) <- 5 ;;
tetriminoI.(1).(3) <- 5 ;;

let tetriminoJ = Array.make_matrix 3 3 0 ;;
tetriminoJ.(0).(0) <- 6 ;;
tetriminoJ.(1).(0) <- 6 ;;
tetriminoJ.(1).(1) <- 6 ;;
tetriminoJ.(1).(2) <- 6 ;;

let tetriminoT = Array.make_matrix 3 3 0 ;;
tetriminoT.(0).(1) <- 7 ;;
tetriminoT.(1).(0) <- 7 ;;
tetriminoT.(1).(1) <- 7 ;;
tetriminoT.(1).(2) <- 7 ;;

let listeTetriminos = [|tetriminoZ ; tetriminoL ; tetriminoO ; tetriminoS ; tetriminoI ; tetriminoJ ; tetriminoT|] ;;

exception PartiePerdue

let genere_tetrimino etat =
    Random.self_init () ;
    let formeTetrimino = listeTetriminos.(Random.int (Array.length listeTetriminos)) in
    let milieuPuits = Array.length etat.puits.(0) / 2
    and milieuTetrimino = Array.length formeTetrimino.(0) / 2 in
    (* Problème de génération *)
    let probleme = ref false in
    for i = 0 to Array.length formeTetrimino - 1 do
        for j = 0 to Array.length formeTetrimino.(0) - 1 do
            if formeTetrimino.(i).(j) <> 0 && etat.puits.(i).(milieuPuits - milieuTetrimino + j) <> 0
            then probleme := true
        done
    done ;
    if !probleme
    then begin
         close_graph () ;
         print_string "Score : " ;
         print_int etat.score ;
         print_newline () ;
         print_string "Niveau : " ;
         print_int etat.niveau ;
         print_newline () ;
         print_string "Lignes : " ;
         print_int etat.nombreLignes ;
         print_newline () ;
         print_newline () ;
         raise PartiePerdue
         end
    else begin
         etat.tetrimino.ligne   <- 0 ;
         etat.tetrimino.colonne <- milieuPuits - milieuTetrimino ;
         etat.tetrimino.forme   <- formeTetrimino
         end ;;



(* Opérations sur un tétrimino *)

let inscrit_tetrimino etat =
    for i = 0 to Array.length etat.tetrimino.forme - 1 do
        for j = 0 to Array.length etat.tetrimino.forme.(0) - 1 do
            if etat.tetrimino.forme.(i).(j) <> 0
            then etat.puits.(etat.tetrimino.ligne + i).(etat.tetrimino.colonne + j) <- etat.tetrimino.forme.(i).(j)
        done
    done ;;

let droite etat = 
    let probleme = ref false in
    for i = 0 to Array.length etat.tetrimino.forme - 1 do
        for j = 0 to Array.length etat.tetrimino.forme.(i) - 1 do
            if etat.tetrimino.forme.(i).(j) <> 0 && etat.tetrimino.colonne + j + 1 >= Array.length etat.puits.(i)  (* Le tétrimino sort du puits *)
            then probleme := true
            else if etat.tetrimino.forme.(i).(j) <> 0 && etat.puits.(etat.tetrimino.ligne + i).(etat.tetrimino.colonne + j + 1) <> 0  (* Le tétrimino va sur un autre tétrimino *)
                 then probleme := true
        done
    done ;
    if not !probleme
    then begin
         etat.tetrimino.colonne <- etat.tetrimino.colonne + 1 ;
         affiche_etat etat
         end ;;

let gauche etat = 
    let probleme = ref false in
    for i = 0 to Array.length etat.tetrimino.forme - 1 do
        for j = 0 to Array.length etat.tetrimino.forme.(i) - 1 do
            if etat.tetrimino.forme.(i).(j) <> 0 && etat.tetrimino.colonne + j - 1 < 0 (* Le tétrimino sort du puits *)
            then probleme := true
            else if etat.tetrimino.forme.(i).(j) <> 0 && etat.puits.(etat.tetrimino.ligne + i).(etat.tetrimino.colonne + j - 1) <> 0  (* Le tétrimino va sur un autre tétrimino *)
                 then probleme := true
        done
    done ;
    if not !probleme
    then begin
         etat.tetrimino.colonne <- etat.tetrimino.colonne - 1 ;
         affiche_etat etat
         end ;;

let rotation_horaire etat =
    let nouvelleForme = Array.make_matrix (Array.length etat.tetrimino.forme) (Array.length etat.tetrimino.forme.(0)) 0
    and probleme = ref true
    and ligne = ref 0
    and colonne = ref 0 in
    for i = 0 to Array.length etat.tetrimino.forme - 1 do
        for j = 0 to (Array.length etat.tetrimino.forme.(0)) - 1 do
            nouvelleForme.(i).(j) <- etat.tetrimino.forme.((Array.length etat.tetrimino.forme.(0)) - 1 - j).(i) ;
        done
    done ;
    (* Problème de sortie du puits *)
    let k = ref 0 in
    while !probleme && !k < (Array.length nouvelleForme) * 2 do
        probleme := false ;
        incr k ;
        for i = 0 to Array.length nouvelleForme - 1 do
            for j = 0 to Array.length nouvelleForme - 1 do
                if nouvelleForme.(i).(j) <> 0
                then if etat.tetrimino.ligne + !ligne + i >= Array.length etat.puits
                     then begin
                          decr ligne ;
                          probleme := true
                          end
                     else if etat.tetrimino.ligne + !ligne + i < 0
                          then begin
                               incr ligne ;
                               probleme := true
                               end
                          else if etat.tetrimino.colonne + !colonne + j >= Array.length etat.puits.(0)
                               then begin
                                    decr colonne ;
                                    probleme := true
                                    end
                               else if etat.tetrimino.colonne + !colonne + j < 0
                                    then begin
                                         incr colonne ;
                                         probleme := true
                                         end
            done
        done
    done ;
    (* Problème de position sur une autre pièce *)
    if not !probleme
    then begin
         for i = 0 to Array.length nouvelleForme - 1 do
             for j = 0 to Array.length nouvelleForme - 1 do
                 if nouvelleForme.(i).(j) <> 0
                 then if etat.puits.(etat.tetrimino.ligne + !ligne + i).(etat.tetrimino.colonne + !colonne + j) <> 0
                      then probleme := true
             done
         done ;
         if not !probleme
         then begin
              etat.tetrimino.ligne   <- etat.tetrimino.ligne   + !ligne ;
              etat.tetrimino.colonne <- etat.tetrimino.colonne + !colonne ;
              etat.tetrimino.forme   <- nouvelleForme ;
              affiche_etat etat
              end
         end ;;

let bas etat =
    let probleme = ref false in
    for i = 0 to Array.length etat.tetrimino.forme - 1 do
        for j = 0 to Array.length etat.tetrimino.forme.(0) - 1 do
            if etat.tetrimino.forme.(i).(j) <> 0 && etat.tetrimino.ligne + i + 1 >= Array.length etat.puits  (* Le tetrimino sort du puits *)
            then probleme := true
            else if etat.tetrimino.forme.(i).(j) <> 0 && etat.puits.(etat.tetrimino.ligne + i + 1).(etat.tetrimino.colonne + j) <> 0  (* Le tétrimino rencontre un obstacle *)
                 then probleme := true
        done
    done ;
    if !probleme
    then begin
         inscrit_tetrimino etat ;
         let lignes = lignes_completes etat.puits in
         if List.length lignes > 0
         then begin
              supprime_lignes etat lignes ;
              let dizaine = etat.nombreLignes / 10 in
              etat.nombreLignes <- etat.nombreLignes + List.length lignes ;
              begin
              match List.length lignes with
                | 1 -> etat.score <- etat.score + 40 * (etat.niveau)
                | 2 -> etat.score <- etat.score + 100 * (etat.niveau)
                | 3 -> etat.score <- etat.score + 300 * (etat.niveau)
                | _ -> etat.score <- etat.score + 1200 * (etat.niveau)
              end ;
              if etat.nombreLignes / 10 > dizaine
              then etat.niveau <- etat.niveau + 1
              end ;
         genere_tetrimino etat
         end
    else begin
         etat.tetrimino.ligne <- etat.tetrimino.ligne + 1 ;
         affiche_etat etat
         end ;;



(* Mise en route du jeu *)

let joue_tetris () =
    let etat = {puits = genere_puits 22 10 ; tetrimino = {ligne = 0 ; colonne = 0 ; forme = listeTetriminos.(0)} ; score = 0 ; niveau = 1 ; nombreLignes = 0}
    and joue = ref true
    and touche = ref ' '
    and date_descente = ref (Sys.time ())
    and delais_descente = 1. (* en secondes *)
    and temps = ref 0. in
    genere_tetrimino etat ;
    open_graph "" ;
    while !joue do
        affiche_etat etat ;
        temps := Sys.time () ;
        (* Descente *)
        if !temps -. !date_descente >= (delais_descente /. (float_of_int etat.niveau))
        then begin
             bas etat ;
             affiche_etat etat ;
             date_descente := !temps
             end ;
        (* Correcpondance des touches *)
        if key_pressed ()
        then begin
             touche := read_key () ;
             match !touche with
               | 'o' -> joue := false
               | 'p' -> let stop = ref true in
                        while !stop do
                            if key_pressed ()
                            then begin
                                 touche := read_key () ;
                                 if !touche = 'p'
                                 then stop := false
                                 else if !touche = 'o'
                                      then begin
                                           joue := false ;
                                           stop := false
                                           end
                                 end
                        done
               | 'z' -> rotation_horaire etat
               | 'q' -> gauche etat
               | 's' -> bas etat
               | 'd' -> droite etat
               |  _  -> ()
             end
    done ;
    close_graph () ;;

joue_tetris () ;;
