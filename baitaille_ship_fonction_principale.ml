(* Exercice 1*) (* Ngan *)


(* récupérer la valeur du 'dx' dans 'prm'. Ce qui représente la dimension du largeur de la grille *)
let matrix_dx(prm:t_param): int = prm.mat.dx;;

(* Récupérer la valeur du 'dy' dans 'prm'. Ce qui représente la dimension de la longeur de la grille *)
let matrix_dy(prm:t_param): int = prm.mat.dy;;

(* Récupérer la valeur du 'len' dans 'prm'. Ce qui représent le nombre de bateaux des joueurs *) 
let ships_number(prm:t_param):int = prm.ships.len;;

(* Récupérer la valeur du 'sh_type' dans 'prm'. Ce qui représente les types des bateaux *)
let ships_type(prm:t_param):int array = prm.ships.sh_type;;

(* Récupérer la valeur du 'shoots_nb dans 'prm'. Ce qui représente nombre de tirs que chaque joueur émet à chque tour de jeu *)
let shoots_number(prm:t_param):int = prm.shoots_nb;;



(* Exercice 2 *) (* Nhu *)

(* Le résultat est une valeur de paramètre permettant de jouer avec des grilles de taille10 x 10, 5 bateaux de tailles respectives5, 4, 3, 3 et 2, et d’émettre 5 tirs à chaque tour de jeu (pour chaque joueur) *)

let init_param():t_param = {mat = {dx = 10;dy = 10}; ships = {len = 5;sh_type = [|5;4;3;3;2|]}; shoots_nb = 5};;



(* Exercice 3 *) (* Ngan *)

(* Vérifier que les coordonnées x et y correspondent à une case d’une grille ou non *)

let is_in_mat (x,y,prm: int*int*t_param): bool =
  (x <= matrix_dx(prm)) && (y <= matrix_dy(prm))
;;



(* Exercice 4 *) (* Nhu *)

(* Vérifier que les coordonnées x et y correspondent à une case d’une grille ou non *)

let is_empty (x,y,m: int*int*t_matrix): bool =
  m.(x).(y)= EMPTY
;;


 (* permettant de savoir si une case de coordonnées x et y(supposées valides) contient une partie de bateaux *)

let is_ship (x,y,m: int*int*t_matrix): bool =
  m.(x).(y) = SHIP
;;



(* Exercice 5 *) (* Ngan *)

(* Vérifier que les coordonnées x et y correspondent à une case vide de la matrice 'm' ou non *)

let valid_empty_position(x, y, m, prm : int * int * t_matrix * t_param) : bool =
  if is_in_mat(x,y,prm) 
  then is_empty(x,y,m)
  else false
;;



(* Exercice 6 *) (* Ngan, Nhu *)

(* Vérifier que les coordonnées x et y correspondent à une case vide de la matrice m ou non. cette fonction est utilisée dans le choix aléatoire de la position de bateaux, et l’objectif de la fonction est de tester s’il n’y a pas de bateaux dans le voisinage immédiat d’unecase de coordonnées(x, y) *)

let is_empty_neighbour(x, y, m, prm : int * int * t_matrix * t_param) : bool =
  let empty_neighbour: bool ref = ref true in
  for i = x-1 to x+1
  do
    for j = y-1 to y+1
    do
      if is_in_mat(i, j, prm)
      then empty_neighbour:= !empty_neighbour && is_empty(i, j, m)
      else ();
    done;
  done;
  !empty_neighbour
;;



(* Exercice 7 *) (* Nhu *)

(*  Tester la validité des coordonnées x et y  *)

let valid_empty_neighbour(x, y, m, prm : int * int * t_matrix * t_param) : bool =
  if is_in_mat(x,y,prm) 
  then is_empty_neighbour(x,y,m,prm)
  else false
;;



(* Execice 8 *) (* Ngan *)

(* Calculer les coordonnées de la case suivant la case de coordonnées 'pt' dans la direction 'dir'. 
Il existe 8 directions possibles : vers la droite (codée0), en bas à droite (codée1), versle bas (codée2), en bas à gauche (codée3), vers la gauche (codée4), en haut à gauche (codée5), vers le haut (codée6), en haut à droite (codée7) *)

let next_position(pt, dir : t_position * int) : t_position =
  let dir_x: int array = [|1; 1; 0; -1; -1; -1; 0; 1|] in
  let dir_y: int array = [|0;-1; -1; -1; 0;  1; 1; 1|] in
  let new_x: int = pt.cx + dir_x.(dir) in
  let new_y: int = pt.cy + dir_y.(dir) in
  { cx = new_x; cy = new_y}
;;



(* Exercice 9 *) (* Nhu *)

(* Insérer le nouveau bateau décrit par la liste 'l' dans la grille 'm' *)

let rec insert_ship_matrix(l, m : t_pos_list * t_matrix) : unit =
  if l = []
  then ()
  else
    (
      let fst: t_position = fst(l) in
      m.(fst.cx).(fst.cy) <- SHIP;
      insert_ship_matrix(rem_fst(l),m)
    )
;;
            
  
  
  (* Exercice 10 *) (* Ngan, Nhu *)

(* Calculer et insère un bateau de taille 'nb_pos' dans la grille 'm':
- Calculer un bateau aléatoire de taille 'nb_pos'
- Insérer le nouveau bateau valide dans la grille 'm'
- Ajouter les coordonnées des cases du nouveau bateau à la liste contenue dans le paramètre *)

let insert_rand_ship(nb_pos, m, l, prm : int * t_matrix * t_remaining_pos * t_param) : unit =
  let pos_bateaux : t_position ref = ref ({cx = 0 ; cy = 0}) and dir : int = rand_int(0, 7)
  and laux : t_remaining_pos = ref [] and theend : bool ref = ref false
  and insert : bool ref = ref true
  in
  (
    while not(!theend)
    do
      pos_bateaux:= {cx = rand_int(0, matrix_dx(prm)-1); cy = rand_int(0, matrix_dy(prm)-1)} ;
      laux := [!pos_bateaux] ;
      insert := valid_empty_neighbour((!pos_bateaux).cx, (!pos_bateaux).cy, m, prm) ;
      for i = 2 to nb_pos
      do
        pos_bateaux:= next_position(!pos_bateaux, dir) ;
        laux := add_lst(!laux, !pos_bateaux) ;
        insert := !insert && valid_empty_neighbour((!pos_bateaux).cx, (!pos_bateaux).cy, m, prm) ;
      done ;
      theend := !insert ;
    done ;
    insert_ship_matrix(!laux, m) ;
    l := concat(!l, !laux) 
  )
;;
    
  

  (* Exercice 11 *) (* Ngan *)

(* Calculer une grille contenant des bateaux aléatoires de tous les types décrits dans le paramètre prm, et donner la liste des coordonnées de toutes les cases de tous les bateaux de la grille *)

let  init_matrix_ships_computer(prm:t_param): t_matrix*t_remaining_pos=
  let l:t_remaining_pos = ref[] in
  let m:t_matrix = mat_make((prm.mat.dx),(prm.mat.dy),EMPTY) and
    len:int = prm.ships.len in
  for i = 0 to len - 1
  do insert_rand_ship((prm.ships.sh_type.(i)),m,l,prm);
  done;
  m,l
;;

 

 (* Exercice 12 *) (* Ngan *)

(* Calculer une grille vide ne contenant aucun bateau et donner une liste vide de coordonnées *)

let init_matrix_ships_human(myname, prm : string* t_param) : t_matrix * t_remaining_pos=
  (mat_make(matrix_dx(prm), matrix_dy(prm), EMPTY), ref []);;

 

(* Exercice 13 *) (* Nhu *)

(* Indiquer que le joueur est un personne si le paramètre hum est true, est le simulé si false *)

let init_matrix_ships(myname, hum, prm : string * bool * t_param) : t_matrix * t_remaining_pos =
  if hum 
  then init_matrix_ships_human(myname, prm)
  else init_matrix_ships_computer(prm)
;;


 (* Exercice 14 *) (* Nhu *)

(* Calculer le nombre de cases composant l’ensemble des bateaux prévu par le paramètre prm *)

let count_positions_ship(prm : t_param) :int =
  let count:int ref = ref 0 in
  (
    for i = 0 to prm.ships.len - 1
    do count:= !count + prm.ships.sh_type.(i)
    done;
    !count
  )
;;

 

 (* Exercixe 15 *) (* Ngan *)

(* Initialiser un joueur est une personne qui a le nom myname si le paramètre hum est true, ou un joueur est le simulé si le paramètre hum est false *)

let init_player(myname, hum, prm : string * bool * t_param) : t_player =
  let ( m, l): t_matrix * t_remaining_pos = init_matrix_ships(myname, hum, prm) and 
  opp : t_matrix = mat_make(matrix_dx(prm), matrix_dy(prm), EMPTY) in
  {name = myname ; human = hum ; mymat = m ; remain = l ; opp_mat = opp; opp_nb = ref (count_positions_ship(prm))}
;;

  
 
 (* Exercice 16 *) (* Nhu *)

(* Initialiser les deux joueurs, le prèmier a le nom nm1 si le parelètre hum1 est true et est le simulé si non. L'autre a le nom nm2 si le paramètre hum2 est true et est le simulé si non *)

let init_play(nm1, hum1, nm2, hum2, prm : string *bool * string * bool * t_param) : t_play =
  {pl1 = init_player(nm1, hum1, prm); pl2 = init_player(nm2, hum2, prm)};;



 (* Exercice 17 *) (* Ngan *)

(* tester que une position p appartient à une liste de positions l ou pas *)

let rec position_in_list(p, l : t_position * t_pos_list) : bool = 
  if l = []
  then false
  else 
  if p = fst(l)
  then true
  else position_in_list (p,rem_fst(l))
;;



 (* Exercice 18 *) (* Nhu *)

(* Calculer une position aléatoire correspondant à une case d’une grille *)

let rand_position(prm : t_param) : t_position =
  {cx = rand_int(0,matrix_dx(prm) - 1); cy = rand_int(0,matrix_dy(prm) - 1)}
;;
 


 (* Exercice 19 *) (* Ngan *)

(* Calculer une position aléatoire correspondant à une case vide d’une grille, et qui n’apparaît pas dans la liste l *)

let rec choose_one_shoot(l, m, prm : t_pos_list* t_matrix * t_param) : t_position =
  let p : t_position = rand_position(prm) in
  if not ( position_in_list(p,l)) && is_empty(p.cx,p.cy,m) 
  then p
  else choose_one_shoot(l, m, prm)
;;



(* Exercice 20 *) (* Nhu *)

(* Calculer une liste de destinations de tirs aléatoires distinctes *)

let rec choose_shoots(nb_shoots, m, prm : int * t_matrix * t_param) : t_pos_list =
  if nb_shoots = 0
  then []
  else 
    let l : t_pos_list = choose_shoots(nb_shoots - 1, m, prm) in
    let pos : t_position = choose_one_shoot(l, m, prm) in
    add_fst(l, pos)
;;

  

(* Exercice 21 *) (* Ngan *)

(* Calculer une liste égale à la liste l privée de la position pos *)

let rec rem_position_from_list(pos, l : t_position * t_pos_list) : t_pos_list = 
  if l = [] 
  then l
  else
  if pos = fst(l)
  then rem_fst(l)
  else add_fst(rem_position_from_list(pos,rem_fst(l)), fst(l))
;;

  

(* Exercice 22 *) (* Ngan *)

(* Simuler le tir du joueur player_1 sur la case de coordonnées pos de la grille du joueur player_2:
   - ici les coordonnées sont quelconques et n'ont pas été vérifiées au préalable
   - donc il faut vérifier avec les autres cases comme dehors de la grille, ou s’il a eu pour destination une case sur laquelle lejoueur avait déjà tiré *)

let one_shoot(pos, player_1, player_2, prm : t_position * t_player * t_player * t_param) : bool =
  if not(is_in_mat(pos.cx,pos.cy,prm))
  then false
  else
  if (player_2.mymat).(pos.cx).(pos.cy) = TRIED ||  (player_2.mymat).(pos.cx).(pos.cy) = DAMAGED
  then false
  else
  if (player_2.mymat).(pos.cx).(pos.cy) = SHIP
  then 
    (
      player_2.remain := rem_position_from_list(pos, !(player_2.remain));
      (player_2.mymat).(pos.cx).(pos.cy) <- DAMAGED; 
      (player_1.opp_mat).(pos.cx).(pos.cy) <- DAMAGED; 
      true;
    )
  else 
    (
      (player_2.mymat).(pos.cx).(pos.cy) <- TRIED;
      false;
    )
;;



(* Exercice 23 *) (* Ngan *)

(* Simuler l’ensemble des tirs effectués par le joueur player_1 sur les cases de la grille du joueur player_2:
  - vérifier la varidation des coordonnées comme la fonction précédentes
  - donner le résultat est le nombre de tirs erronés *)

let rec all_shoots_aux(shoots, player_1, player_2, nb, prm : t_pos_list * t_player * t_player * int * t_param) : int =
  if shoots = []
  then nb
  else
  if one_shoot(fst(shoots), player_1, player_2, prm) 
  then all_shoots_aux(rem_fst(shoots), player_1, player_2, nb + 1, prm)
  else all_shoots_aux(rem_fst(shoots), player_1, player_2, nb, prm)
;;

let all_shoots(shoots, player_1, player_2, prm : t_pos_list * t_player * t_player * t_param) : int =
  all_shoots_aux(shoots, player_1, player_2, 0, init_param())
;;




(* Exercice 24 *) (* Nhu *)

(* Effectuer un tour de jeu, dans le cas où le joueur player_1 est un joueur simulé et donner le nombre de tirs erronés *)

let play_one_player_computer(player_1, player_2, prm : t_player * t_player * t_param) : int =
  all_shoots(choose_shoots(shoots_number(prm), player_2.mymat, prm), player_1, player_2,prm)
;;



(* Exercice 25 *) (* Nhu *)

(* Retouner une valeur 0 *) 

let play_one_player_human(player_1, player_2, prm : t_player * t_player * t_param) : int = 0
;;



(* Exercice 26 *) (* Ngan *)

(* Simuler un tour de jeu où c’est au tour du joueur player_1 de jouer, selon que ce joueur soit simulé ou humain *)

let play_one_player(player_1, player_2, prm : t_player * t_player * t_param) : int =
  if player_1.human
  then play_one_player_human(player_1, player_2, prm)
  else play_one_player_computer(player_1, player_2, prm)
;;



(* Exercice 27 *) (* Nhu *)

(* Prendre en paramètre le numéro du joueur qui vient de jouer (1 ou 2) et retourner le numéro du joueur *)

let new_who(w : int) : int=
  if w = 1
  then 2
  else 1
;;


(* ----------------------- *)
(* ----------------------- *)
(*  fonction principale    *)
(* ----------------------- *)
(* ----------------------- *)



let play(nm_1, hum_1, nm_2, hum_2 : string * bool * string * bool) : unit =
  let prm : t_param = init_param() and who : int ref = ref 1 
      and the_end : bool ref = ref false and nb_failed : int ref = ref 0
  in
  let pl : t_play = init_play(nm_1, hum_1, nm_2, hum_2, prm) 
  in
    (
    print_string("initialisation") ; print_newline() ;
    print_play(pl, prm) ;
    wait(8) ;
    while not(!the_end) 
    do
      print_string("joueur "^string_of_int(!who)^" joue :") ; print_newline() ;
      if !who = 1
      then nb_failed := play_one_player(pl.pl1, pl.pl2, prm)
      else nb_failed := play_one_player(pl.pl2, pl.pl1, prm) ;
      print_play(pl, prm) ;
      if !nb_failed > 0
      then print_mess(!who, string_of_int(!nb_failed) ^" tirs deja tires")
      else () ;
      wait(8) ;
      who := new_who(!who) ;
      the_end := (!(pl.pl1.opp_nb) = 0) || (!(pl.pl2.opp_nb) = 0)
    done 
    )
;;


(* Exercice 28 *) (* Ngan *)

  type t_ship = {len : int; pos : t_pos_list};;
  type t_remain = t_ship list;;


(* Exercice 29 *) (* Ngan *)

  type t_player = {name : string ; human : bool ; mymat : t_matrix ; remain : t_remain ; opp_mat : t_matrix ; opp_nb : int ref} ;; 
  type t_opp_nb = t_ship ref;;
  
(* Exercice 30 *) (* Ngan *)

  type t_player = {name : string ; human : bool ; mymat : t_matrix ; remain : t_remain ; opp_mat : t_matrix ; opp_remain : t_opp_nb} ;;



