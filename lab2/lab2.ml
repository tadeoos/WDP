(* procedura head zwraca pierwsze n elementow listy l *)
let head l n =
  let rec pom l n acc =
    match l,n with
    | _, 0 -> acc
    | [], _ -> acc
    | h::t, _ -> pom t (n-1) (h::acc)
  in List.rev (pom l n [])

(* procedura tail zwraca ostatnie n elementow listy l *)
let tail l n =
  let rec pom l n acc =
    match l,n with
    | _, 0 -> acc
    | [], _ -> acc
    | h::t, _ -> pom t (n-1) (h::acc)
  in pom (List.rev l) n [];;

(* procedura bioraca liste par i zwracajaca pare list, gdzie *)
let split l =
  let rec pom l x y =
    match l with
    | [] -> (List.rev x, List.rev y)
    | (a,b)::t -> pom t (a::x) (b::y)
  in pom l [] [];;

(*
let combine l1 l2 =
  let rec pom *)



(* testy *)
let l = [1;2;3;4;5;6;7;8];;
let ls = [(1,2);(3,4);(5,6)];;
(* tail *)
assert (tail l 4 = [5;6;7;8]);
assert (tail l 10 = l);
assert (tail l 0 = []);
assert (tail l (-4) = l);
(* head *)
assert (head l 4 = [1;2;3;4]);
assert (head l 10 = l);
assert (head l 0 = []);
assert (head l (-4) = l);;
(* split *)
assert (split ls = ([1;3;5],[2;4;6]));;


(* tail (head l 3) 1;; *)

(* zad 3 polegajace na zwroceniu kombinacji listy list - zobacz na wazniaku *)
