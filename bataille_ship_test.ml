
(* Exercice 1 *) (* Ngan *)

(* Tester la valide de la fonction matrix_dx *)

let test_matrix_dx(status : t_test_status) : unit=
  let test_step : t_test_step = test_start(status, "test_matrix_dx") in
  let test_result : int t_test_result = test_exec(test_step, matrix_dx,
                                                  ({mat = {dx = 2; dy = 4} ; ships = {len = 5; sh_type = [|5;4;3;3;2|]}; shoots_nb = 4})) in
  (
    if test_is_success(test_result)
    then
      assert_equals(test_step, "valid", test_get(test_result),2)
    else
      test_error(test_step);
    test_end(test_step)
  )
;;


(* Tester la valide de la fonction matrix_dy *)

let test_matrix_dy(status : t_test_status) : unit=
  let test_step : t_test_step = test_start(status, "test_matrix_dy") in
  let test_result : int t_test_result = test_exec(test_step, matrix_dy,
                                                  ({mat = {dx = 2; dy = 4} ; ships = {len = 5; sh_type = [|5;4;3;3;2|]}; shoots_nb = 4})) in
  (
    if test_is_success(test_result)
    then
      assert_equals(test_step, "valid", test_get(test_result),4)
    else
      test_error(test_step);
    test_end(test_step)
  )
;;


(* Tester la valide de la fonction ships_number *)

let test_ships_number(status : t_test_status) : unit=
  let test_step : t_test_step = test_start(status, "test_ships_number") in
  let test_result : int t_test_result = test_exec(test_step, ships_number,
                                                  ({mat = {dx = 2; dy = 4} ; ships = {len = 5; sh_type = [|5;4;3;3;2|]}; shoots_nb = 4})) in
  (
    if test_is_success(test_result)
    then
      assert_equals(test_step, "valid", test_get(test_result),5)
    else
      test_error(test_step);
    test_end(test_step)
  )
;;

(* Tester la valide de la fonction ships_type *)

let test_ships_type(status : t_test_status) : unit=
  let test_step : t_test_step = test_start(status, "test_ships_type") in
  let test_result : int array t_test_result = test_exec(test_step, ships_type,
                                                        ({mat = {dx = 2; dy = 4} ; ships = {len = 5; sh_type = [|5;4;3;3;2|]}; shoots_nb = 4})) in
  (
    if test_is_success(test_result)
    then
      assert_equals(test_step, "valid", test_get(test_result),[|5;4;3;3;2|])
    else
      test_error(test_step);
    test_end(test_step)
  )
;;

(* Tester la valide de la fonction shoots_number *)

let test_shoots_number(status : t_test_status) : unit=
  let test_step : t_test_step = test_start(status, "test_shoots_number") in
  let test_result : int t_test_result = test_exec(test_step, shoots_number,
                                                  ({mat = {dx = 2; dy = 4} ; ships = {len = 5; sh_type = [|5;4;3;3;2|]}; shoots_nb = 4})) in
  (
    if test_is_success(test_result)
    then
      assert_equals(test_step, "valid", test_get(test_result),4)
    else
      test_error(test_step);
    test_end(test_step)
  )
;;


(* Exercice 2 *) (* Nhu *)

(* tester la fonction init_param *)
let test_init_param(status : t_test_status) : unit=
  let test_step : t_test_step = test_start(status, "init_param") in 
  let test_result : t_param t_test_result = test_exec(test_step, init_param, ()) in
  (
    if test_is_success(test_result)
    then
      (
        let init_param: t_param = test_get(test_result) in
        assert_equals(test_step, "dx", init_param.mat.dx, 10);
        assert_equals(test_step, "dy", init_param.mat.dy, 10);
        assert_equals(test_step, "len", init_param.ships.len, 5);
        assert_equals(test_step, "sh_type", init_param.ships.sh_type, [|5;4;3;3;2|]);
        assert_equals(test_step, "shoots_nb", init_param.shoots_nb, 5);
      )
    else
      test_error(test_step);
    test_end(test_step)
  )
;;


(* Exercice 3*) (* Ngan *)

(* Tester la fonction is_in_mat *)

let test_is_in_mat_false(status : t_test_status) : unit=
  let test_step : t_test_step = test_start(status, "is_in_mat_false") in
  let param : t_param = init_param() in
  let test_result : bool t_test_result = test_exec(test_step, is_in_mat, (6,12,param)) in
  (
    if test_is_success(test_result)
    then
      assert_equals(test_step, "false", test_get(test_result), false)
    else
      test_error(test_step);
    test_end(test_step)
  )
;;

let test_is_in_mat_1(status : t_test_status) : unit=
  let test_step : t_test_step = test_start(status, "is_in_mat_1") in
  let param: t_param = init_param() in
  let test_result : bool t_test_result = test_exec(test_step, is_in_mat, (2,5,param)) in
  (
    if test_is_success(test_result)
    then
      assert_equals(test_step, "true", test_get(test_result),true)
    else
      test_error(test_step);
    test_end(test_step)
  )
;;

let test_is_in_mat_2(status : t_test_status) : unit=
  let test_step : t_test_step = test_start(status, "is_in_mat_2") in
  let param : t_param = init_param() in
  let test_result : bool t_test_result = test_exec(test_step, is_in_mat, (9,4,param)) in
  (
    if test_is_success(test_result)
    then
      assert_equals(test_step, "true", test_get(test_result),true)
    else
      test_error(test_step);
    test_end(test_step)
  )
;;



(* Exercice 4 *) (* Nhu *)

(* Test la fonction is_empty *)

let test_is_empty_false_1(status : t_test_status) : unit=
  let test_step : t_test_step = test_start(status, "is_empty_false_1") in
  let mat : t_matrix = [|[|SHIP|]|] in
  let test_result : bool t_test_result = test_exec(test_step, is_empty, (2,4,mat)) in
  (
    if test_is_success(test_result)
    then
      assert_equals(test_step, "false", test_get(test_result),false)
    else
      test_error(test_step);
    test_end(test_step)
  )
;;

let test_is_empty_false_2(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "is_empty_false_2") in
  let mat : t_matrix = [|[|DAMAGED|]|] in
  let test_result : bool t_test_result = test_exec(test_step, is_empty, (3,3,mat)) in
  (
    if test_is_success(test_result)
    then
      assert_equals(test_step, "false" ,test_get(test_result),false)
    else
      test_error(test_step);
    test_end(test_step)
  )
;;

let test_is_empty(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "is_empty") in
  let mat : t_matrix = [|[|EMPTY|]|] in
  let test_result : bool t_test_result = test_exec(test_step, is_empty, (2,3,mat)) in
  (
    if test_is_success(test_result)
    then
      assert_equals(test_step, "true" ,test_get(test_result),true)
    else
      test_error(test_step);
    test_end(test_step)
  )
;;

let test_is_ship_false_1(status : t_test_status) : unit=
  let test_step : t_test_step = test_start(status, "is_ship_false_1") in
  let mat : t_matrix = [|[|EMPTY|]|] in
  let test_result : bool t_test_result = test_exec(test_step, is_ship, (2,4,mat)) in
  (
    if test_is_success(test_result)
    then
      assert_equals(test_step, "false", test_get(test_result),false)
    else
      test_error(test_step);
    test_end(test_step)
  )
;;

let test_is_ship_false_2(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "is_ship_false_2") in
  let mat : t_matrix = [|[|DAMAGED|]|] in
  let test_result : bool t_test_result = test_exec(test_step, is_ship, (3,3,mat)) in
  (
    if test_is_success(test_result)
    then
      assert_equals(test_step, "false" ,test_get(test_result),false)
    else
      test_error(test_step);
    test_end(test_step)
  )
;;

let test_is_ship(status : t_test_status) : unit=
  let test_step : t_test_step = test_start(status, "is_ship") in
  let mat : t_matrix = [|[|SHIP|]|] in
  let test_result : bool t_test_result = test_exec(test_step, is_ship, (2,4,mat)) in
  (
    if test_is_success(test_result)
    then
      assert_equals(test_step, "true", test_get(test_result),true)
    else
      test_error(test_step);
    test_end(test_step)
  )
;;


(* Exercice 5 *) (* Ngan *)

(* Tester la fonction valid_empty_position *)  (* test fonctionnel *)

let test_valid_empty_position_false(status : t_test_status) : unit=
  let test_step : t_test_step = test_start(status, "valid_empty_position_false") in
  let prm: t_param = init_param() in
  let mat : t_matrix = [|[|SHIP|]|] in
  let test_result : bool t_test_result = test_exec(test_step, valid_empty_position, (2,4,mat,prm)) in
  (
    if test_is_success(test_result)
    then
      assert_equals(test_step, "false", test_get(test_result),false)
    else
      test_error(test_step);
    test_end(test_step)
  )
;;

let test_valid_empty_position(status : t_test_status) : unit=
  let test_step : t_test_step = test_start(status, "valid_empty_position") in
  let prm: t_param = init_param() in
  let mat : t_matrix = [|[|EMPTY|]|] in
  let test_result : bool t_test_result = test_exec(test_step, valid_empty_position, (2,4,mat,prm)) in
  (
    if test_is_success(test_result)
    then
      assert_equals(test_step, "true", test_get(test_result),true)
    else
      test_error(test_step);
    test_end(test_step)
  )
;;

(* Exercice 6 *) (* Ngan, Nhu *)

(* Test la fonction is_empty_neighbour *) (* test fonctionnel *)

let test_is_empty_neighbour(status:t_test_status):unit=
  let test_step:t_test_step= test_start(status,"is_empty_neighbour")in
  let test_result:bool t_test_result= test_exec(test_step,is_empty_neighbour, (1,1,[|[|EMPTY;EMPTY;EMPTY|];[|EMPTY;EMPTY;EMPTY|];[|EMPTY;EMPTY;EMPTY|]|] ,init_param())) in
  (
    if test_is_success(test_result)
    then assert_equals(test_step,"true",test_get(test_result),true)
    else test_error(test_step);
    test_end(test_step)
  )
;;

(* Exercice 7 *) (* Nhu *)

(* Test la fonction valid_empty_neighbour *)

let test_valid_empty_neighbour(status : t_test_status) : unit=
  let test_step : t_test_step = test_start(status,"test_valid_empty_neighbour") in
  let m : t_matrix = mat_make(2,4,EMPTY) in
  let param : t_param = init_param() in
  let test_result : bool t_test_result = test_exec(test_step, valid_empty_neighbour,(1,1,m,param)) in
  (
    if test_is_success(test_result)
    then
      assert_equals(test_step,"valid",test_get(test_result),true)
    else
      test_error(test_step);
    test_end(test_step)
  )
;;

(* Exercice 8 *) (* Ngan *)

(* Test la fonction next_position *)

let test_next_position(status : t_test_status) : unit=
  let test_step : t_test_step = test_start(status,"test_next_position") in
  let m : t_matrix = mat_make(2,4,EMPTY) in
  let param : t_param = init_param() in
  let test_result : bool t_test_result = test_exec(test_step, valid_empty_neighbour,(1,1,m,param)) in
  (
    if test_is_success(test_result)
    then
      assert_equals(test_step,"valid",test_get(test_result),true)
    else
      test_error(test_step);
    test_end(test_step)
  )
;;

(* Exercice 9 *) (* Nhu *)
 
(* Test la fonction insert_ship_matrix *)  (* test structurel *)

let test_insert_ship_matrix(statut:t_test_status):unit=
  let test_step:t_test_step=test_start(statut, "insert_ship_matrix" )in
  let test_result:unit t_test_result= test_exec(test_step,insert_ship_matrix,([{cx=2;cy=3};{cx=1;cy=2};{cx=3;cy=4};{cx=8;cy=4}], mat_make(10,10,EMPTY))) in
  (
    if test_is_success(test_result)
    then assert_equals(test_step,"insert_ship_matrix",test_get(test_result),())
    else 
      test_error(test_step);
    test_end(test_step)
  )
;;

(* Exercice 10 *) (* Ngan *)

(* Tester la fonction insert_rand_ship *) (* test fonctionel *)
                                          
let test_insert_rand_ship(status : t_test_status) : unit = 
  let test_step : t_test_step = test_start(status,"insert_rand_ship") in
  let bateaux : t_remaining_pos  = ref [] in
  let m : t_matrix = mat_make(10,10,EMPTY) in
  let param : t_param = init_param() in
  let test_result : unit t_test_result = test_exec(test_step, insert_rand_ship,(5,m,bateaux,param)) in
  (
    if test_is_success(test_result)
    then
      (
        for i=0 to 3
        do
          let pos : t_position = nth(!bateaux,i) in
          assert_equals(test_step, "insert_rand_ship_true"^string_of_int(i), m.(pos.cy).(pos.cx), SHIP)
        done;
      )
    else 
      test_error(test_step); 
    test_end(test_step)
  )
;;

(* Exercice 11 *) (* Ngan *)

(* Tester la fonction init_matrix_ships_computer *) (* test fontionnel *)

let test_init_matrix_ships_computer(status : t_test_status) : unit = 
  let test_step : t_test_step = test_start(status,"init_matrix_ships_computer") in
  let param : t_param = init_param() in
  let test_result : (t_matrix * t_remaining_pos) t_test_result = test_exec(test_step, init_matrix_ships_computer,(param)) in
  (
    if test_is_success(test_result)
    then
      (
        let (m,l) : t_matrix * t_remaining_pos =  test_get(test_result) in
        assert_equals(test_step, "valid", len(!l), 17 );
        for i = 0 to 16
        do
          let pos : t_position = nth(!l,i) in
          assert_equals(test_step, "valid"^string_of_int(i), m.(pos.cy).(pos.cx), SHIP) 
        done;
      )
    else 
      test_error(test_step); 
    test_end(test_step)
  )
;;

(* Exercice 12 *) (* Ngan *)

(* Tester la fonction init_matrix_ships_human *) 

let test_init_matrix_ships_human(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status,"test_init_matrix_ships_human")in
  let myname : string = "ABC" in
  let param: t_param = init_param() in
  let test_result : (t_matrix * t_remaining_pos) t_test_result = test_exec(test_step,init_matrix_ships_human, (myname, param)) in
  (
    if test_is_success(test_result)
    then
      (
        let (m, l) : t_matrix * t_remaining_pos = test_get(test_result) in
        let count : int ref = ref 0 in
        for i = 0 to 9
        do
          for j = 0 to 9
          do
            if m.(i).(j) = EMPTY
            then count := !count +1
            else ()
          done
        done;
        assert_equals(test_step,"grille", !count,100);
      )
    else
      test_error(test_step); 
    test_end(test_step)
  )
;;

(* Exercice 13 *) (* Nhu *)

(* Tester la fonction init_matrix_ships *) 

let test_init_matrix_ships_true(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status,"test_init_matrix_ships_true")in
  let myname : string = "ABC" in
  let param: t_param = init_param() in
  let hum : bool = true in
  let test_result : (t_matrix * t_remaining_pos) t_test_result = test_exec(test_step,init_matrix_ships, (myname, hum, param)) in
  (
    if test_is_success(test_result)
    then
      (
        assert_equals(test_step,"human",hum,true);
        let (m, l) : t_matrix * t_remaining_pos = test_get(test_result) in
        let count : int ref = ref 0 in
        for i = 0 to 9
        do
          for j = 0 to 9
          do
            if m.(i).(j) = EMPTY
            then count := !count +1
            else ()
          done
        done;
        assert_equals(test_step,"grille", !count,100);
      )
    else
      test_error(test_step); 
    test_end(test_step)
  )
;;


let test_init_matrix_ships_false(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status,"test_init_matrix_ships_false")in
  let myname : string = "ABC" in
  let param: t_param = init_param() in
  let hum : bool = false in
  let test_result : (t_matrix * t_remaining_pos) t_test_result = test_exec(test_step,init_matrix_ships, (myname, hum, param)) in
  (
    if test_is_success(test_result)
    then
      (
        assert_equals(test_step,"computer",hum,false);
        let (m,l) : t_matrix * t_remaining_pos =  test_get(test_result) in
        assert_equals(test_step, "valid", len(!l), 17 );
        for i = 0 to 16
        do
          let pos : t_position = nth(!l,i) in 
          assert_equals(test_step, "valid"^string_of_int(i), m.(pos.cy).(pos.cx), SHIP)
        done;
      )
    else
      test_error(test_step); 
    test_end(test_step)
  )
;;
            

(* Exercice 14 *) (* Nhu *)

(* Tester la fonction count_positions_ship *)

let test_count_positions_ship(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status,"test_count_positions_ship")in 
  let param: t_param = init_param() in 
  let test_result : int t_test_result = test_exec(test_step,count_positions_ship, (param)) in
  (
    if test_is_success(test_result)
    then
      (
        let count : int ref = ref 0 in
        for i = 0 to param.ships.len - 1
        do 
          count:= !count + param.ships.sh_type.(i);
        done; 
        assert_equals(test_step, "nombre de cases composant partie des bateaux", !count, 17) 
      )
    else
      test_error(test_step); 
    test_end(test_step) 
  )
;;

(* Exercice 15 *) (* Ngan *)

(* Tester la valide des information des joueurs dans la fonction init_player *)

let test_init_player_true(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_init_player_true") in
  let myname : string = "ABC" in
  let param : t_param = init_param() in
  let test_result : t_player t_test_result = test_exec(test_step , init_player , (myname, true, param)) in
  (
    if test_is_success(test_result)
    then
      ( 
        let joueur :t_player = test_get(test_result) in
        assert_equals(test_step,"joueur_1" , joueur.name,"ABC");
        assert_equals(test_step,"joueur_2",  joueur.human, true);
        assert_equals(test_step, "opp_nb", joueur.opp_nb, ref 17);
      )
    else
      test_error(test_step);
    test_end(test_step)

  )
;;


let test_init_player_false(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_init_player_false") in
  let myname : string = "ABC" in
  let param : t_param = init_param() in
  let test_result : t_player t_test_result = test_exec(test_step , init_player , (myname, false, param)) in
  (
    if test_is_success(test_result)
    then
      ( 
        let joueur :t_player = test_get(test_result) in
        assert_equals(test_step,"joueur_1" , joueur.name,"ABC");
        assert_equals(test_step,"joueur_2",  joueur.human, false);
        assert_equals(test_step, "opp_nb", joueur.opp_nb, ref 17);
      )
    else
      test_error(test_step);
    test_end(test_step)

  )
;;

(* Exercice 16 *) (* Nhu *)

(* Tester la valide des informations des joueurs dans la fonction init_play *)

let test_init_play_1(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_init_play_1") in
  let myname1 : string = "ABC" in
  let myname2 : string = "DEF" in
  let hum1 : bool = true in
  let hum2: bool = false in
  let param : t_param = init_param() in
  let test_result : t_play t_test_result = test_exec(test_step , init_play , (myname1, hum1, myname2, hum2, param)) in
  (
    if test_is_success(test_result)
    then
      ( 
        let play : t_play = test_get(test_result) in
        assert_equals(test_step,"joueur_1" , play.pl1.name,"ABC");
        assert_equals(test_step,"joueur_1" , play.pl1.opp_nb,ref 17);
        assert_equals(test_step,"joueur_2" , play.pl2.human,false);
        assert_equals(test_step,"joueur_2" , play.pl1.opp_nb,ref 17);
      )
    else
      test_error(test_step);
    test_end(test_step)

  )
;; 

let test_init_play_2(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_init_play_2") in
  let myname1 : string = "ABC" in
  let myname2 : string = "DEF" in
  let hum1 : bool = true in
  let hum2: bool = true in
  let param : t_param = init_param() in
  let test_result : t_play t_test_result = test_exec(test_step , init_play , (myname1, hum1, myname2, hum2, param)) in
  (
    if test_is_success(test_result)
    then
      ( 
        let play : t_play = test_get(test_result) in
        assert_equals(test_step,"joueur_1" , play.pl1.name,"ABC");
        assert_equals(test_step,"joueur_1" , play.pl1.opp_nb,ref 17);
        assert_equals(test_step,"joueur_2" , play.pl2.name,"DEF");
        assert_equals(test_step,"joueur_2" , play.pl1.opp_nb,ref 17);
      )
    else
      test_error(test_step);
    test_end(test_step)

  )
;; 

(* Exercice 17 *) (* Ngan *)

(* Tester la fonction position_in_list_true *) (* test structurel *)

let test_position_in_list_true(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_position_in_list_true") in
  let p : t_position = {cx = 2; cy = 9} in
  let l : t_pos_list =[{cx=1;cy=0};{cx=2;cy=9};{cx=2;cy=7};{cx=4;cy=5}] in 
  let test_result : bool t_test_result = test_exec(test_step , position_in_list , (p,l)) in
  (
    if test_is_success(test_result)
    then 
      assert_equals(test_step,"position_in_list",test_get(test_result),true)
    else
      test_error(test_step);
    test_end(test_step)
  )
;;

let test_position_in_list_false(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_position_in_list_true") in
  let p : t_position = {cx = 0; cy = 9} in
  let l : t_pos_list =[{cx=1;cy=0};{cx=2;cy=9};{cx=2;cy=7};{cx=4;cy=5}] in 
  let test_result : bool t_test_result = test_exec(test_step , position_in_list , (p,l)) in
  (
    if test_is_success(test_result)
    then 
      assert_equals(test_step,"position_in_list",test_get(test_result),false)
    else
      test_error(test_step);
    test_end(test_step)
  )
;;

(* Exercice 18 *) (* Ngan *)

(* Tester la fonction rand_position *)

let test_rand_position(status : t_test_status) : unit =
  let step : t_test_step = test_start(status, "test_rand_position") in 
  let param : t_param = init_param()in
  let test_result : t_position t_test_result = test_fail_exec(step , rand_position , (param)) in
  (
    if test_is_success(test_result)
    then 
      assert_true(step, "position is in mat", test_is_success(test_result))
    else 
      test_error(step) ;
    test_end(step)   
  )
;;

(* Exercice 19 *) (* Ngan *)

(* Tester la fonction choose_one_shoot *)

let test_choose_one_shoot(status : t_test_status) : unit=
  let test_step : t_test_step = test_start(status,"test_choose_one_shoot_aux_1") in
  let m : t_matrix = mat_make(10,10,EMPTY) in
  let param : t_param = init_param()in
  let l : t_pos_list =[{cx=1;cy=0};{cx=2;cy=9};{cx=2;cy=7};{cx=4;cy=5}] in
  let test_result :t_position t_test_result = test_exec(test_step ,choose_one_shoot,(l,m,param))in
  (
    if test_is_success(test_result)
    then
      (
        assert_equals(test_step,"test_choose_one_shoot", test_get(test_result), {cx=1;cy=0});
        assert_equals(test_step,"test_choose_one_shoot", test_get(test_result), {cx=2;cy=9});
        assert_equals(test_step,"test_choose_one_shoot", test_get(test_result), {cx=2;cy=7});
        assert_equals(test_step,"test_choose_one_shoot", test_get(test_result), {cx=4;cy=5}) 
      )
    else
      test_error(test_step);
    test_end(test_step)
  )
;;

(* Exercice 20 *) (* Nhu *)

(* Tester la fonction choose_shoots *)

let test_choose_shoots(status : t_test_status) : unit=
  let test_step : t_test_step = test_start(status,"test_choose_shoots") in
  let m : t_matrix = mat_make(10,10,EMPTY) in
  let param : t_param = init_param()in 
  let test_result :t_pos_list t_test_result = test_exec(test_step ,choose_shoots,(5,m,param))in
  ( 
    if test_is_success(test_result)
    then
      (
        let l : t_pos_list = test_get(test_result) in
        assert_equals(test_step,"nombre de tirs", len(l), 5);
      )
    else
      test_error(test_step);
    test_end(test_step)
  )
;;

(* Exercice 21 *) (* Ngan *)

(* Tester la fonction rem_position_from_list *)

let test_rem_position_from_list(status : t_test_status) : unit=
  let test_step : t_test_step = test_start(status,"test_rem_position_from_list") in 
  let l : t_pos_list =[{cx=1;cy=0};{cx=2;cy=9};{cx=2;cy=7};{cx=4;cy=5}] in
  let pos : t_position = {cx = 2; cy = 9 } in
  let test_result :t_pos_list t_test_result = test_exec(test_step ,rem_position_from_list,(pos, l))in
  ( 
    if test_is_success(test_result)
    then
      (
        let rem_l : t_pos_list = test_get(test_result) in
        assert_equals(test_step,"list", len(rem_l), 3);
      )
    else
      test_error(test_step);
    test_end(test_step)
  )
;;

(* Exercice 22 *) (* Ngan *)

(* Tester la fonction one_shoot *)

let test_one_shoot(status : t_test_status) : unit=
  let test_step : t_test_step = test_start(status,"test_one_shoot") in
  let param :t_param = init_param()in
  let p1 : t_player= init_player("joueur_1", true, param)in
  let p2 : t_player= init_player("joueur_2", true, param)in
  let test_result :bool t_test_result = test_exec(test_step ,one_shoot,({cx=2;cy=2},p1,p2,param))in
  ( 
    if test_is_success(test_result)
    then
      (
        assert_equals(test_step,"test_one_shoot", test_get(test_result),true);
      )
    else
      test_error(test_step);
    test_end(test_step)
  )
;;

(* Exercice 23 *) (* Ngan *)

(* tester la fonction all_shoots *)

let test_all_shoots(status : t_test_status) : unit=
  let test_step : t_test_step = test_start(status,"test_all_shoots") in
  let shoots : t_pos_list = [{cx=1;cy=0};{cx=2;cy=9};{cx=2;cy=7};{cx=4;cy=5}] in
  let param :t_param = init_param()in
  let p1 : t_player= init_player("joueur_1", true, param)in
  let p2 : t_player= init_player("joueur_2", true, param)in
  let test_result : int t_test_result = test_exec(test_step ,all_shoots,(shoots, p1, p2, param ))in
  ( 
    if test_is_success(test_result)
    then
      (
        assert_equals(test_step,"nombre de tirs", test_get(test_result),4);
      )
    else
      test_error(test_step);
    test_end(test_step)
  )
;;

(* Exercice 24 *) (*Nhu *)

(* tester la fonction play_one_player_computer *)

let test_play_one_player_computer(status : t_test_status) : unit=
  let test_step : t_test_step = test_start(status,"test_play_one_player_computer") in
  let param :t_param = init_param()in
  let p1 : t_player= init_player("joueur_1", false, param)in
  let p2 : t_player= init_player("joueur_2", true, param)in
  let test_result : int t_test_result = test_exec(test_step ,play_one_player_computer,(p1, p2, param ))in
  ( 
    if test_is_success(test_result)
    then
      (
        assert_equals(test_step,"nombre de tirs", test_get(test_result),4);
      )
    else
      test_error(test_step);
    test_end(test_step)
  )
;;

(* Exercice 25 *) (* Nhu *)

(* tester la fonction play_one_player_human *)

let test_play_one_player_human(status : t_test_status) : unit=
  let test_step : t_test_step = test_start(status,"test_play_one_player_human") in
  let param :t_param = init_param()in
  let p1 : t_player= init_player("joueur_1", false, param)in
  let p2 : t_player= init_player("joueur_2", true, param)in
  let test_result : int t_test_result = test_exec(test_step ,play_one_player_human,(p1, p2, param ))in
  ( 
    if test_is_success(test_result)
    then
      ( 
        assert_equals(test_step,"joueur_1" , p1.human,false);
        assert_equals(test_step,"joueur_2", p2.name, "joueur_2");
        assert_equals(test_step, "opp_nb_1", p1.opp_nb, ref 17);
        assert_equals(test_step, "opp_nb_2", p2.opp_nb, ref 17);
      )
    else
      test_error(test_step);
    test_end(test_step)
  )
;;

(* Exercice 26 *) (* Nhu *)

(* tester la fonction play_one player *)

let test_play_one_player_1(status : t_test_status) : unit=
  let test_step : t_test_step = test_start(status,"test_play_one_player_1") in
  let param :t_param = init_param()in
  let p1 : t_player= init_player("joueur_1", false, param)in
  let p2 : t_player= init_player("joueur_2", true, param)in
  let test_result : int t_test_result = test_exec(test_step ,play_one_player,(p1, p2, param ))in
  ( 
    if test_is_success(test_result)
    then
      ( 
        assert_equals(test_step,"joueur_1" , p1.human,false);
        assert_equals(test_step,"joueur_2", p2.name, "joueur_2");
        assert_equals(test_step, "opp_nb_1", p1.opp_nb, ref 17);
        assert_equals(test_step, "opp_nb_2", p2.opp_nb, ref 17);
        assert_equals(test_step, "nombre de tirs", test_get(test_result), play_one_player_computer(p1, p2, param));
      )
    else
      test_error(test_step);
    test_end(test_step)
  )
;; 


let test_play_one_player_2(status : t_test_status) : unit=
  let test_step : t_test_step = test_start(status,"test_play_one_player_2") in
  let param :t_param = init_param()in
  let p1 : t_player= init_player("joueur_1", true, param)in
  let p2 : t_player= init_player("joueur_2", true, param)in
  let test_result : int t_test_result = test_exec(test_step ,play_one_player,(p1, p2, param ))in
  ( 
    if test_is_success(test_result)
    then
      ( 
        assert_equals(test_step,"joueur_1" , p1.name, "joueur_1");
        assert_equals(test_step,"joueur_2", p2.name, "joueur_2");
        assert_equals(test_step, "opp_nb_1", p1.opp_nb, ref 17);
        assert_equals(test_step, "opp_nb_2", p2.opp_nb, ref 17);
        assert_equals(test_step, "nombre de tirs", test_get(test_result), play_one_player_human(p1, p2, param));
      )
    else
      test_error(test_step);
    test_end(test_step)
  )
;; 

(* Exercice 27 *) (* Ngan *)

(* tester la fonction new_who *)

let test_new_who_1(status : t_test_status) : unit=
  let test_step : t_test_step = test_start(status,"test_new_who_1") in 
  let test_result : int t_test_result = test_exec(test_step ,new_who, 1)in
  ( 
    if test_is_success(test_result)
    then
      ( 
        assert_equals(test_step, "numéro du joueur", test_get(test_result), 2);
      )
    else
      test_error(test_step);
    test_end(test_step)
  )
;; 

let test_new_who_1(status : t_test_status) : unit=
  let test_step : t_test_step = test_start(status,"test_new_who_1") in 
  let test_result : int t_test_result = test_exec(test_step ,new_who, 2)in
  ( 
    if test_is_success(test_result)
    then
      ( 
        assert_equals(test_step, "numéro du joueur", test_get(test_result), 1);
      )
    else
      test_error(test_step);
    test_end(test_step)
  )
;; 
        
        