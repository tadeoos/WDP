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
