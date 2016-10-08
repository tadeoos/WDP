let silnia n = 
	let rec pom_sil x acc =
		match x with
		| 0 -> acc
		| _ -> (pom_sil (x-1) (x*acc))
	in match n with
	| _ when n<0 -> 0		
	| _ -> pom_sil n 1;;

silnia 5;;	

(* This function takes two rectangles specified as bottom-left 
(first) and top-right (second) corners given by date 
cartesian coordinates and returns a rectangle of 
shared space. If the cooridnates indicate that other
 corners were chosen it denotes "empty" triangle which means arguments do not overlap each other *)
let rect ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) = 
	((max x1 x3, max y1 y3),(min x2 x4, min y2 y4))
	