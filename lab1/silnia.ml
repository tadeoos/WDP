let silnia x = 
	let rec pom_sil n acc =
		if n = 0 then acc else (pom_sil (n-1) (n*acc))
	in if x<0 then failwith "invalid arg. n should be non negative" else pom_sil x 1;;

(* This function takes two rectangles specified as bottom-left 
(first) and top-right (second) corners given by date 
cartesian coordinates and returns a rectangle of 
shared space. If the cooridnates indicate that other
 corners were chosen it denotes "empty" triangle which means arguments do not overlap each other *)
let rect ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) = 
	((max x1 x3, max y1 y3),(min x2 x4, min y2 y4))
	