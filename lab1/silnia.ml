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
	((max x1 x3, max y1 y3),(min x2 x4, min y2 y4))

(* list reverse *)


(* fibonacci *)
