
let factorial n =
	let rec help_fact x acc =
		match x with
		| 0 -> acc
		| _ -> (help_fact (x-1) (x*acc))
	in match n with
	| _ when n<0 -> failwith "input should be non negative"
	| _ -> help_fact n 1;;


(* This function takes two rectangles specified as bottom-left (first) and top-right (second) corners given by date cartesian coordinates and returns a rectangle of shared space. If the cooridnates indicate that other corners were chosen it denotes "empty" triangle which means arguments do not overlap each other *)
let rect ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) =
	((max x1 x3, max y1 y3),(min x2 x4, min y2 y4));;

(* list reverse *)
let list_reverse l =
  let rec help_list l acc =
    match l with
    | [] -> acc
    | hd::tl -> help_list tl (hd::acc)
  in help_list l [];;

(* fibonacci *)
let rec fib_num n =
    match n with
    | _ when n<1 -> 0
    | 1 -> 1
    | _ -> fib_num (n-1) + fib_num (n-2)

(* fibonacci with tail recursion*)
let fib_num_tail n =
    let rec fib_help x acc1 acc2 =
        match x with
        | _ when x<1 -> acc1
        | _ -> (fib_help (x-1) (acc2) (acc1+acc2))
    in match n with
    | _ -> fib_help n 0 1;;

(* this function takes x and y and returns x^y in O(n) *)
let podnies_do_potegi x y =
    let rec pom_pod b c = if b=0 then c else pom_pod (b-1) (c*x)
in if y<0 then -1 else pom_pod y 1;;

(* and now in O(logn) *)
let pp x y =
    let rec fp pod wyk wyn =
    if wyk = 0 then wyn else
    if wyk mod 2 = 1 then fp (pod*pod) (wyk/2) (wyn*pod) else
    fp (pod*pod) (wyk/2) wyn
in if y<0 then -1 else fp x y 1;;

(* this function takes a function nat(N) -> int(Z) and gives the maximal sum of some subset of section (1,n)*)
let max_suma f n =
    let rec policz g s minim wynik =
        if g = n then (max wynik (s-minim))
        else let noweS = s + (f (g+1)) in
        policz (g+1) noweS (min minim noweS) (max wynik (noweS-minim))
    in if n>0 then policz 1 (f 1) (min 0 (f 1)) (max 0 (f 1)) else -1;;

let f n =
  match n with
  | _ when n mod 3 = 1 -> 7*n-2
  | _ -> -n*3+7;;


(* measure execution time; taken from http://stackoverflow.com/questions/27510037/ocaml-measure-execution-time-in-toplevel *)
let time f x =
      let t = Sys.time() in
      let fx = f x in
      Printf.printf "execution time: %fs\n" (Sys.time() -. t);
      fx;;

(* let rec average_time n res acc f x =
  match acc with
  | 0 -> res./.n.
  | _ -> average_time n (res. +. time_raw (f x)) (acc-1) f x;; *)

print_string "\nmax suma n=14\n";;
time max_suma f 700;;

print_string "\npodnies do potegi\n";;
time podnies_do_potegi 16 40022;;
time pp 16 40022;;

print_string "\nfibonacii\n";;
(* huuuge difference! *)
time fib_num 20;;
time fib_num_tail 100;;
