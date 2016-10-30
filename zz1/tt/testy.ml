open Arytmetyka

(* let a = wartosc_od_do (-1.) 1.            (* <-1, 1> *)
let b = wartosc_dokladna (-1.)            (* <1, 1> *) *)
let a = wartosc_od_do (6.) 8.            (* <-1, 1> *)
let b = wartosc_dokladna (4.)            (* <1, 1> *)
let c = podzielic b a                     (* (-inf -1> U <1 inf) *)
let d = plus c a                          (* (-inf, inf) *)
let e = wartosc_dokladna 0.               (* <0, 0> *)
let f = razy c e                          (* <0, 0> *)
let g = razy d e                          (* <0, 0> *)
let h = wartosc_dokladnosc (-10.) 50.     (* <-15, -5> *)
let i = podzielic h e                     (* nan, przedzial pusty*)
let j = wartosc_od_do (-6.) 5.            (* <-6, 5> *)
let k = razy j j                          (* <-30, 36> *)
let l = plus a b                          (* <-2, 0> *)
let m = razy b l                          (* <0, -2> *)
let n = podzielic l l                     (* <0, inf) *)
let o = podzielic l m                     (* (-inf, 0) *)
let p = razy o a                          (* (-inf, inf) *)
let q = plus n o                          (* (-inf, inf) *)
let r = minus n n                         (* (-inf, inf) *)
let s = wartosc_dokladnosc (-0.0001) 100. (* <-0.0002, 0> *)
let t = razy n s                          (* (-inf, 0) *)
;;

let test_podziel a b c d =
  podzielic (wartosc_od_do a b) (wartosc_od_do c d)
;;

(* assert ((test_podziel 6. 8. (-2.) (-2.)) = (wartosc_od_do 3. (-3.))); *)
(* assert ((min_wartosc c, max_wartosc c) = (neg_infinity, infinity)); *)
(* assert ((sr_wartosc c) == nan) *)
(* assert (compare (sr_wartosc c) nan = 0); *)
(* assert (in_wartosc c 0. = false);
assert ((in_wartosc c (-1.)) && (in_wartosc c (-100000.)) && (in_wartosc c 1.) && (in_wartosc c 100000.));
assert ((in_wartosc d 0.) && (in_wartosc d (-1.)) && (in_wartosc d (-100000.)) && (in_wartosc d 1.) && (in_wartosc d 100000.));
assert ((min_wartosc f, max_wartosc f, sr_wartosc f) = (0., 0., 0.));
assert ((min_wartosc g, max_wartosc g, sr_wartosc g) = (0., 0., 0.));
assert ((min_wartosc h, max_wartosc h, sr_wartosc h) = (-15., -5., -10.));
assert ((compare nan (min_wartosc i), compare nan (sr_wartosc i), compare nan (max_wartosc i)) = (0, 0, 0));
assert ((min_wartosc k, max_wartosc k, sr_wartosc k) = (-30., 36., 3.));
assert ((min_wartosc n, max_wartosc n, sr_wartosc n) = (0., infinity, infinity));
assert ((min_wartosc o, max_wartosc o, sr_wartosc o) = (neg_infinity, 0., neg_infinity));
assert ((min_wartosc p, max_wartosc p, compare (sr_wartosc p) nan) = (neg_infinity, infinity, 0));
assert ((min_wartosc q, max_wartosc q, compare (sr_wartosc q) nan) = (neg_infinity, infinity, 0));
assert ((min_wartosc r, max_wartosc r, compare (sr_wartosc r) nan) = (neg_infinity, infinity, 0));
assert ((min_wartosc t, max_wartosc t, sr_wartosc t) = (neg_infinity, 0., neg_infinity)); *)
(* assert ((test_podziel 6. 8. (-2.) (-2.)) = (wartosc_od_do 3. (-3.))); *)
