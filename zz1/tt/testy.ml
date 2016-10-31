open Arytmetyka

(* let test_podziel a b c d e f=
  assert ((podzielic (wartosc_od_do a b) (wartosc_od_do c d))  = (wartosc_od_do 3. (-3.)));
  ;; *)

let test_podziel a b c d =
  assert ((podzielic a b) = (wartosc_od_do c d))
  ;;
let test_warotsc a b c =
  Printf.fprintf stdout " %f %f \n" (min_wartosc a) (max_wartosc a);
  assert (a = (wartosc_od_do b c));;
(* let p = druk a;; *)
let druk a w =
  Printf.fprintf stdout "%s %f %f \n" a (min_wartosc w) (max_wartosc w);;


let a = wartosc_od_do (-1.) 1.            (* <-1, 1> *)

let b = wartosc_dokladna (-1.)            (* <-1, -1> *)
let c = podzielic b a                     (* (-inf -1> U <1 inf) *)
;;
test_warotsc c 1. (-1.);;
let d = plus c a                          (* (-inf, inf) *)
let e = wartosc_dokladna 0.               (* <0, 0> *)
;;
test_warotsc e 0. (0.);;
let f = razy c e                          (* <0, 0> *)
;;
test_warotsc f 0. (0.);;
let h = wartosc_dokladnosc (-10.) 50.     (* <-15, -5> *)
let i = podzielic h e                     (* nan, przedzial pusty*)
let j = wartosc_od_do (-6.) 5.            (* <-6, 5> *)
let k = razy j j                          (* <-30, 36> *)
let l = plus a b                          (* <-2, 0> *)
let m = razy b l                          (* <0, 2> *)
let n = podzielic l l                     (* <0, inf) *)
let o = podzielic l m                     (* (-inf, 0) *)
let r = minus n n                         (* (-inf, inf) *)
let s = wartosc_dokladnosc (-0.0001) 100. (* <-0.0002, 0> *)
let t = razy n s                          (* (-inf, 0) *)

let g = podzielic j (wartosc_od_do (-4.) 3.)
;;

druk "a" a;;
druk "b" b;;
druk "c" c;;
druk "d" d;;
druk "e" e;;
druk "f" f;;
druk "g" g;;
druk "h" h;;
druk "i" i;;
druk "j" j;;
druk "k" k;;
druk "l" l;;
druk "m" m;;
druk "n" n;;
druk "o" o;;
druk "r" r;;
druk "s" s;;
druk "t" t;;


assert (in_wartosc a 0. == true);
assert ((min_wartosc c, max_wartosc c) = (neg_infinity, infinity));
assert ((sr_wartosc c) == nan);
assert (compare (sr_wartosc c) nan = 0);
assert (in_wartosc c 0. = false);
assert ((in_wartosc c (-1.)) && (in_wartosc c (-100000.)) && (in_wartosc c 1.) && (in_wartosc c 100000.));
assert ((in_wartosc d 0.) && (in_wartosc d (-1.)) && (in_wartosc d (-100000.)) && (in_wartosc d 1.) && (in_wartosc d 100000.));
assert ((min_wartosc f, max_wartosc f, sr_wartosc f) = (0., 0., 0.));
assert ((min_wartosc h, max_wartosc h, sr_wartosc h) = (-15., -5., -10.));
assert ((min_wartosc k, max_wartosc k, sr_wartosc k) = (-30., 36., 3.));
assert ((min_wartosc n, max_wartosc n, sr_wartosc n) = (0., infinity, infinity));
assert ((min_wartosc o, max_wartosc o, sr_wartosc o) = (neg_infinity, 0., neg_infinity));
assert ((min_wartosc r, max_wartosc r, compare (sr_wartosc r) nan) = (neg_infinity, infinity, 0));
assert ((min_wartosc t, max_wartosc t, sr_wartosc t) = (neg_infinity, 0., neg_infinity));

(* [2;-1]/[3;5]=[0.4;-0.2] *)
druk "x" (podzielic (wartosc_od_do 2. (-1.)) (wartosc_od_do 3. 5.));;
let x = podzielic (wartosc_od_do 2. (-1.)) (wartosc_od_do 3. 5.);;
assert (x = wartosc_od_do (2./.5.) (-1./.5.));
;;

(* [2;-1]/[-5;-3]=[0.2;-0.4] *)
let test = podzielic (wartosc_od_do 2. (-1.)) (wartosc_od_do (-5.) (-3.));;
test_warotsc test 0.2 (-0.4);;
druk "test" test;;

(* [2;-1]+[-5;-1]=[-inf; inf] *)
druk "t1" (plus (wartosc_od_do 2. (-1.)) (wartosc_od_do (-5.) (-1.)));;
(* [2;-1]+[-1;1]=[1;0] *)
druk "t2" (plus (wartosc_od_do 2. (-1.)) (wartosc_od_do (-1.) (1.)));;
(* [-4;-1]+[2;-1]=[-inf;inf] *)
druk "t4" (plus (wartosc_od_do (-4.) (-1.)) (wartosc_od_do 2. (-1.)));;
(* [-0.5;1]+[2;-1]=[1.5;0] *)
druk "t5" (plus (wartosc_od_do (-0.5) (1.)) (wartosc_od_do 2. (-1.)));;
(* [-2;-0.5]+[2;-1]=[0;-1.5] *)
druk "t6" (plus (wartosc_od_do (-2.) (-0.5)) (wartosc_od_do 2. (-1.)));;

(* [4;-0.5]+[2;-1]=? *)
druk "t7" (plus (wartosc_od_do (4.) (-0.5)) (wartosc_od_do 2. (-1.)));;
