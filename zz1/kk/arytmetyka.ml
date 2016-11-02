(************************************************)
(* Zadanie o arytmetyce niedokładnych wartości. *)
(************************************************)
(* autor: Katarzyna Kańska, 320728 *)
(* recenzent: Tadeusz Teleżyński, 305885 *)

(* Typ reprezentujący niedokładne wartości. *)
type wartosc = { poczatek : float ; koniec : float };;
(* Niedokładną wartość rozumiemy jako przedzial obustronnie domkniety lub jego dopelnienie, czyli antyprzedział (tez domkniety). *)
(* Przedzial to para liczb (x,y) taka, że x <= y. *)
(* Antyprzedział to para liczb (x,y) taka, że x > y. *)

(* Implicite zakładamy, że wszystkie argumenty typu float są liczbami *)
(* rzeczywistymi, tzn. są różne od infinity, neg_infinity i nan.      *)


(* wartosc_dokladnosc x p = x +/- p% *)
(* war.pocz.: p > 0                  *)
let wartosc_dokladnosc x p = if (x == infinity) || (x == neg_infinity) || (x == nan) || (p == infinity) || (p == neg_infinity) || (p == nan) then
								failwith "Niepoprawny argument funkcji. x oraz p musza byc skonczonymi liczbami rzeczywistymi."
							else
								{poczatek = (x -. (abs_float x) *. p /. 100.); koniec = (x +. (abs_float x) *. p /. 100.)}
;;

(* wartosc_od_do x y = [x;y]         *)
(* war.pocz.: x <= y                 *)
let wartosc_od_do x y = if (x == infinity) || (y == neg_infinity) || (x == nan) || (y == nan) then
								failwith "Niepoprawny argument funkcji. x oraz y musza byc skonczonymi liczbami rzeczywistymi."
						else
							{poczatek = x; koniec = y}
;;

(* wartosc_dokladna x = [x;x]        *)
let wartosc_dokladna x = if (x == infinity) || (x == neg_infinity) || (x == nan) then
								failwith "Niepoprawny argument funkcji. x musi byc skonczona liczba rzeczywista."
							else
								{poczatek = x; koniec = x}
;;

(* in_wartosc w x = x \in w *)
let in_wartosc w x = if (w.poczatek <= w.koniec) then
						(w.poczatek <= x) && (x <= w.koniec)
					else
						(x <= w.koniec) || (w.poczatek <= x)
;;

(* min_wartosc w = najmniejsza możliwa wartość w,   *)
(* lub neg_infinity jeśli brak dolnego ograniczenia.*)
let min_wartosc w = if (w.poczatek <= w.koniec) then
						w.poczatek
					else
						neg_infinity
;;

(* max_wartosc w = największa możliwa wartość w,    *)
(* lub infinity jeśli brak górnego ograniczenia.    *)
let max_wartosc w = if (w.poczatek <= w.koniec) then
						w.koniec
					else
						infinity
;;

(* środek przedziału od min_wartosc do max_wartosc, *)
(* lub nan jeśli min i max_wartosc nie są określone.*)
let sr_wartosc w = if (w.poczatek <= w.koniec) && (max_wartosc w != infinity) then (* mamy ograniczony przedzial *)
						(min_wartosc w +. max_wartosc w) /. 2.
					else
						nan
;;

(* Operacje arytmetyczne na niedokładnych wartościach. *)
let plus w v =
	let czy_antyprzedzial z = (z.poczatek > z.koniec) in
	if (czy_antyprzedzial w) && (czy_antyprzedzial v) then
		{poczatek = neg_infinity; koniec = infinity} (* wynikiem jest cala prosta rzeczywista *)
	else
		if (czy_antyprzedzial w) || (czy_antyprzedzial v) then
			if ((w.poczatek +. v.poczatek) <= (w.koniec +. v.koniec)) then
				{poczatek = neg_infinity; koniec = infinity} (* wynikiem jest cala prosta rzeczywista *)
			else
				{poczatek = (w.poczatek +. v.poczatek); koniec = (w.koniec +. v.koniec)} (* wynikiem jest antyprzedzial *)
		else
			{poczatek = (min_wartosc w +. min_wartosc v); koniec = (max_wartosc w +. max_wartosc v)}
			(* wynikiem jest przedzial lub cala prosta, jesli ktorys z przedzialow byl cala prosta *)
;;

let minus w v =
	let czy_antyprzedzial z = (z.poczatek > z.koniec) in
	if (czy_antyprzedzial w) && (czy_antyprzedzial v) then
		{poczatek = neg_infinity; koniec = infinity}  (* wynikiem jest cala prosta rzeczywista *)
	else
		if (czy_antyprzedzial w) || (czy_antyprzedzial v) then
			if ((w.poczatek -. v.koniec) <= (w.koniec -. v.poczatek)) then
				{poczatek = neg_infinity; koniec = infinity}  (* wynikiem jest cala prosta rzeczywista *)
			else
				{poczatek = (w.poczatek -. v.koniec); koniec = (w.koniec -. v.poczatek)} (* wynikiem jest antyprzedzial *)
		else
			{poczatek = (min_wartosc w -. max_wartosc v); koniec = (max_wartosc w -. min_wartosc v)}
			(* wynikiem jest przedzial lub cala prosta, jesli ktorys z przedzialow byl cala prosta *)
;;

let razy w v =
	let czy_przedzial z = (z.poczatek <= z.koniec) in
	if (czy_przedzial w) && (czy_przedzial v) then
        if (w.koniec == infinity) || (v.koniec == infinity) then
            {poczatek = neg_infinity; koniec = infinity} (* wynikiem jest cala prosta rzeczywista *)
        else
            {poczatek = min (w.poczatek *. v.poczatek) (min (w.poczatek *. v.koniec) (min (w.koniec *. v.poczatek) (w.koniec *. v.koniec)));
		    koniec  = max (w.poczatek *. v.poczatek) (max (w.poczatek *. v.koniec) (max (w.koniec *. v.poczatek) (w.koniec *. v.koniec)))}
		    (* wynikiem jest przedzial *)
	else
		if (czy_przedzial w) || (czy_przedzial v) then
			if ((czy_przedzial w) && (in_wartosc w 0.)) || ((czy_przedzial v) && (in_wartosc v 0.)) then
				if ((czy_przedzial w) && (w.poczatek = w.koniec)) || ((czy_przedzial v) && (v.poczatek = v.koniec)) then
					{poczatek = 0.; koniec = 0.} (* wynikiem jest wartosc dokladna rowna zero *)
				else
					{poczatek = neg_infinity; koniec = infinity}  (* wynikiem jest cala prosta rzeczywista *)
			else
				if czy_przedzial w then
					if w.koniec < 0. then
						{poczatek = min (v.koniec *. w.poczatek) (v.koniec *. w.koniec);
						koniec = max (v.poczatek *. w.poczatek) (v.poczatek *. w.koniec)}
						(* wynikiem jest antyprzedzial *)
					else
						{poczatek = min (v.poczatek *. w.poczatek) (v.poczatek *. w.koniec);
						koniec = max (v.koniec *. w.poczatek) (v.koniec *. w.koniec)}
						(* wynikiem jest antyprzedzial *)
				else
					if v.koniec < 0. then
						{poczatek = min (w.poczatek *. v.poczatek) (w.poczatek *. v.koniec);
						koniec = max (w.koniec *. v.poczatek) (w.koniec *. v.koniec)}
						(* wynikiem jest antyprzedzial *)
					else
						{poczatek = min (w.koniec *. v.poczatek) (w.koniec *. v.koniec);
						koniec = max (w.poczatek *. v.poczatek) (w.poczatek *. v.koniec)}
						(* wynikiem jest antyprzedzial *)
		else
			if (in_wartosc w 0.) || (in_wartosc v 0.) then
				{poczatek = neg_infinity; koniec = infinity}  (* wynikiem jest cala prosta rzeczywista *)
			else
				{poczatek = min (w.poczatek *. v.poczatek) (w.koniec *. v.koniec);
				koniec  = max (w.poczatek *. v.koniec) (w.koniec *. v.poczatek)}
				(* wynikiem jest antyprzedzial *)
;;

let podzielic w v =
	let czy_antyprzedzial z = (z.poczatek > z.koniec)
	and czy_przedzial z = (z.poczatek <= z.koniec) in
	if (czy_antyprzedzial w) && (czy_antyprzedzial v) then
		{poczatek = neg_infinity; koniec = infinity}  (* wynikiem jest cala prosta rzeczywista *)
	else
        if czy_przedzial w then
            match (in_wartosc w 0.), (in_wartosc v 0.) with
            | true, true -> {poczatek = neg_infinity; koniec = infinity} (* wynikiem jest cala prosta rzeczywista *)
            | false, true ->    if czy_przedzial v then
                                    if w.koniec < 0. then
                                        {poczatek = (w.koniec /. v.poczatek); koniec = (w.koniec /. v.koniec)}
                                        (* wynikiem jest antyprzedzial *)
                                    else
                                        {poczatek = (w.poczatek /. v.koniec); koniec = (w.poczatek /. v.poczatek)}
                                        (* wynikiem jest antyprzedzial *)
                                else
                                    match (w.poczatek < 0.), (v.poczatek < 0.) with
                                    | true, true ->  if (w.poczatek /. v.koniec) >= (w.koniec /. v.poczatek) then
                                                             {poczatek = neg_infinity; koniec = infinity}
                                                             (* wynikiem jest cala prosta rzeczywista *)
                                                        else
                                                            {poczatek = (w.koniec /. v.poczatek); koniec = (w.poczatek /. v.koniec)}
                                                            (* wynikiem jest antyprzedzial *)

                                    | true, false -> if (w.poczatek /. v.poczatek) <= (w.koniec /. v.koniec) then
                                                             {poczatek = neg_infinity; koniec = infinity} (* wynikiem jest cala prosta rzeczywista *)
                                                        else
                                                            {poczatek = (w.poczatek /. v.poczatek); koniec = (w.koniec /. v.koniec)}
                                                            (* wynikiem jest antyprzedzial *)

                                    | false, true -> if (w.koniec /. v.koniec) <= (w.poczatek /. v.poczatek) then
                                                             {poczatek = neg_infinity; koniec = infinity}
                                                             (* wynikiem jest cala prosta rzeczywista *)
                                                        else
                                                            {poczatek = (w.koniec /. v.koniec); koniec = (w.poczatek /. v.poczatek)}
                                                            (* wynikiem jest antyprzedzial *)

                                    | false, false ->   if (w.koniec /. v.poczatek) >= (w.poczatek /. v.koniec) then
                                                             {poczatek = neg_infinity; koniec = infinity} (* wynikiem jest cala prosta rzeczywista *)
                                                        else
                                                            {poczatek = (w.poczatek /. v.koniec); koniec = (w.koniec /. v.poczatek)}
                                                            (* wynikiem jest antyprzedzial *)
            | _, false ->   {poczatek = min (w.poczatek /. v.poczatek) (min (w.poczatek /. v.koniec) (min (w.koniec /. v.poczatek) (w.koniec /. v.koniec)));
                            koniec  = max (w.poczatek /. v.poczatek) (max (w.poczatek /. v.koniec) (max (w.koniec /. v.poczatek) (w.koniec /. v.koniec)))}
                            (* wynikiem jest przedzial *)
        else
            if (in_wartosc v 0.) then
                {poczatek = neg_infinity; koniec = infinity} (* wynikiem jest cala prosta rzeczywista *)
            else
                if (min (w.koniec /. v.poczatek) (w.koniec /. v.koniec)) <= (max (w.poczatek /. v.poczatek) (w.poczatek /. v.koniec)) then
                    {poczatek = neg_infinity; koniec = infinity} (* wynikiem jest cala prosta rzeczywista *)
                else
                    {poczatek = (min (w.koniec /. v.poczatek) (w.koniec /. v.koniec));  koniec = (max (w.poczatek /. v.poczatek) (w.poczatek /. v.koniec))}
                    (* wynikiem jest antyprzedzial *)
;;

(*
Testy od recenzenta:

open Arytmetyka

let t7 = podzielic (wartosc_od_do (5.) (4.)) (wartosc_od_do (-2.) (-2.));;
assert (t7 = (wartosc_od_do (-2.) (-2.5)));;

let a = wartosc_od_do (-1.) 1.            (* <-1, 1> *)
let b = wartosc_dokladna (-1.)            (* <-1, -1> *)
let c = podzielic b a                     (* (-inf -1> U <1 inf) *)
;;
assert (c = (wartosc_od_do 1. (-1.)));;
let d = plus c a                          (* (-inf, inf) *)
let e = wartosc_dokladna 0.               (* <0, 0> *)
;;
assert (e = (wartosc_od_do 0. (0.)));;
let f = razy c e                          (* <0, 0> *)
;;
druk2 "c" c;;
assert (f = (wartosc_od_do 0. (0.)));;
let h = wartosc_dokladnosc (-10.) 50.     (* <-15, -5> *)
;;
assert (h = (wartosc_od_do (-15.) (-5.)));;
let i = podzielic h e                     (* nan, przedzial pusty*)
;;
(* assert (i = (wartosc_od_do (nan) (nan)));; *)
let j = wartosc_od_do (-6.) 5.            (* <-6, 5> *)
;;
assert (j = (wartosc_od_do (-6.) (5.)));;
let k = razy j j                          (* <-30, 36> *)
;;
assert (k = (wartosc_od_do (-30.) (36.)));;
let l = plus a b                          (* <-2, 0> *)
;;
assert (l = (wartosc_od_do (-2.) (0.)));;
let m = razy b l                          (* <0, 2> *)
;;
assert (m = (wartosc_od_do (0.) (2.)));;
let n = podzielic l l                     (* <0, inf) *)
;;
(*assert (n = (wartosc_od_do (0.) (infinity)));;*)
let o = podzielic l m                     (* (-inf, 0) *)
;;
(* assert (o = (wartosc_od_do (neg_infinity) (0.)));; *)
let r = minus n n                         (* (-inf, inf) *)
;;
assert (r = (wartosc_od_do (neg_infinity) (infinity)));;
let s = wartosc_dokladnosc (-0.0001) 100. (* <-0.0002, 0> *)
;;
assert (s = (wartosc_od_do (-0.0002) (0.)));;
let t = razy n s                          (* (-inf, 0) *)
;;

(*assert (sr_wartosc t = neg_infinity);
assert (in_wartosc a 0. == true);
assert ((min_wartosc c, max_wartosc c) = (neg_infinity, infinity));
assert ((sr_wartosc c) == nan);
(* ten test nie dziala nie wiem czemu??? *)
(* assert ((sr_wartosc r) == nan); *)
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
;;*)
(* [2;-1]/[3;5]=[0.4;-0.2] *)
let x = podzielic (wartosc_od_do 2. (-1.)) (wartosc_od_do 3. 5.);;
(* assert (x = wartosc_od_do (2./.5.) (-1./.5.))
;; *)

(* [2;-1]/[-5;-3]=[0.2;-0.4] *)
let test = podzielic (wartosc_od_do 2. (-1.)) (wartosc_od_do (-5.) (-3.));;
assert (test = (wartosc_od_do 0.2 (-0.4)));;


(* [2;-1]+[-5;-1]=[-inf; inf] *)
let t1 = plus (wartosc_od_do 2. (-1.)) (wartosc_od_do (-5.) (-1.));;
assert (t1 = (wartosc_od_do (neg_infinity) (infinity)));;
(* druk "t1" t1;; *)
(* [2;-1]+[-1;1]=[1;0] *)
let t2 = plus (wartosc_od_do 2. (-1.)) (wartosc_od_do (-1.) (1.));;
assert (t2 = (wartosc_od_do (1.) (0.)));;
(* druk "t2" t2;; *)
(* [-4;-1]+[2;-1]=[-inf;inf] *)
let t4 = plus (wartosc_od_do (-4.) (-1.)) (wartosc_od_do 2. (-1.));;
assert (t4 = (wartosc_od_do (neg_infinity) (infinity)));;
(* druk "t4" t4;; *)
(* [-0.5;1]+[2;-1]=[1.5;0] *)
let t5 = plus (wartosc_od_do (-0.5) (1.)) (wartosc_od_do 2. (-1.));;
assert (t5 = (wartosc_od_do (1.5) (0.)));;
(* druk "t5" t5;; *)
(* [-2;-0.5]+[2;-1]=[0;-1.5] *)
let t6 = plus (wartosc_od_do (-2.) (-0.5)) (wartosc_od_do 2. (-1.));;
assert (t6 = (wartosc_od_do (0.) (-1.5)));;
(* druk "t6" t6;; *)

(* [4;-0.5]+[2;-1]=[-inf;inf] *)
let t7 = plus (wartosc_od_do (4.) (-0.5)) (wartosc_od_do 2. (-1.));;
assert (t7 = (wartosc_od_do (neg_infinity) (infinity)));;
(* druk "t7" t7;; *)
(* [4;-0.5]-[2;-1]=[-inf;inf] *)
let t7 = minus (wartosc_od_do (4.) (-0.5)) (wartosc_od_do 2. (-1.));;
assert (t7 = (wartosc_od_do (neg_infinity) (infinity)));;
(* druk "t8" t7;; *)

(* [3;5]*[4;-2]=[12, -6] *)
let t7 = razy (wartosc_od_do (3.) (5.)) (wartosc_od_do 4. (-2.));;
(* druk "t9" t7;; *)
assert (t7 = (wartosc_od_do (12.) (-6.)));;

(* [5;3]*[2;-1]=[neg_infinity, infinity] *)
let t7 = razy (wartosc_od_do (5.) (3.)) (wartosc_od_do 2. (-1.));;
(* druk "t10" t7;; *)
assert (t7 = (wartosc_od_do (neg_infinity) (infinity)));;


(* [4;-0.5]*[2;-1]=[0.5;-1] *)
let t7 = razy (wartosc_od_do (4.) (-0.5)) (wartosc_od_do 2. (-1.));;
(* druk "t11" t7;; *)
assert (t7 = (wartosc_od_do (0.5) (-1.)));;

(* [1;0]*[2;0]=[-inf;inf] *)
let t7 = razy (wartosc_od_do (1.) (0.)) (wartosc_od_do 2. 0.);;
assert (t7 = (wartosc_od_do (neg_infinity) (infinity)));;
(* [-0.5;0.5]*[-0.2;0.6]=? *)
let t7 = razy (wartosc_od_do (-0.5) (0.5)) (wartosc_od_do (-0.2) (0.6));;
assert (t7 = (wartosc_od_do (-0.3) (0.3)));;

(* [15;-20]*[4;-6]= [60;-80] *)
let t7 = razy (wartosc_od_do (15.) (-20.)) (wartosc_od_do 4. (-6.));;
assert (t7 = (wartosc_od_do (60.) (-80.)));;

(* [0;0]*[4;-6]= [0;0] *)
let t7 = razy (wartosc_od_do (0.) (0.)) (wartosc_od_do 4. (-6.));;
assert (t7 = (wartosc_od_do (0.) (0.)));;

(* [0;2]*[-10;-6]= [-20;0] *)
let t7 = razy (wartosc_od_do (0.) (2.)) (wartosc_od_do (-10.) (-6.));;
assert (t7 = (wartosc_od_do (-20.) (0.)));;

(* [3;1]-[1;2]= [1;0] *)
let t7 = minus (wartosc_od_do (3.) (1.)) (wartosc_od_do (1.) (2.));;
assert (t7 = (wartosc_od_do (1.) (0.)));;

(* [3;1]-[-3;2]= [-inf;inf] *)
let t7 = minus (wartosc_od_do (3.) (1.)) (wartosc_od_do (-3.) (2.));;
assert (t7 = (wartosc_od_do (neg_infinity) (infinity)));;

(* [4;-7]-[-2;2]= [2;-5] *)
let t7 = minus (wartosc_od_do (4.) (-7.)) (wartosc_od_do (-2.) (2.));;
assert (t7 = (wartosc_od_do (2.) (-5.)));;

(* [0;2]-[3;-6]= [6;-1] *)
let t7 = minus (wartosc_od_do (0.) (2.)) (wartosc_od_do (3.) (-6.));;
assert (t7 = (wartosc_od_do (6.) (-1.)));;

(* [1;2]-[-3;7]= [-6;5] *)
let t7 = minus (wartosc_od_do (1.) (2.)) (wartosc_od_do (-3.) (7.));;
assert (t7 = (wartosc_od_do (-6.) (5.)));;

(* [1;2]+[-3;7]= [-6;5] *)
let t7 = plus (wartosc_od_do (1.) (2.)) (wartosc_od_do (-3.) (7.));;
assert (t7 = (wartosc_od_do (-2.) (9.)));;

(* [7;-2]+[5;-1]=[-inf;inf] *)
let t7 = plus (wartosc_od_do (7.) (-2.)) (wartosc_od_do (5.) (-1.));;
assert (t7 = (wartosc_od_do (neg_infinity) (infinity)));;

(* [7;-2]-[5;-1]=[-inf;inf] *)
let t7 = minus (wartosc_od_do (7.) (-2.)) (wartosc_od_do (5.) (-1.));;
assert (t7 = (wartosc_od_do (neg_infinity) (infinity)));;

(* [7;-2]-[-3;-1]=[4;-4] *)
let t7 = minus (wartosc_od_do (7.) (-2.)) (wartosc_od_do (-3.) (-1.));;
assert (t7 = (wartosc_od_do (8.) (1.)));;

(* [7;-2]/[5;-9]=[0;-0] *)
let t7 = podzielic (wartosc_od_do (7.) (-2.)) (wartosc_od_do (5.) (-9.));;
assert (t7 = (wartosc_od_do (neg_infinity) (infinity)));;

(* [5;4]/[-2;-3]=[-inf;inf] *)
let t7 = podzielic (wartosc_od_do (5.) (4.)) (wartosc_od_do (-2.) (-3.));;
assert (t7 = (wartosc_od_do (neg_infinity) (infinity)));;

(* [2;-4]/[1;-10]=[0;-0] *)
let t7 = podzielic (wartosc_od_do (2.) (-4.)) (wartosc_od_do (1.) (-10.));;
(* druk "t7" t7;; *)
assert (t7 = (wartosc_od_do (neg_infinity) (infinity)));;

(* [5;4]/[-2;-2]=[-2;-2.5] *)
let t7 = podzielic (wartosc_od_do (5.) (4.)) (wartosc_od_do (-2.) (-2.));;
druk2 "t7" t7;;
assert (t7 = (wartosc_od_do (-2.) (-2.5)));;

assert ((wartosc_od_do (0.) (0.)) != (wartosc_od_do (0.) (-0.)));;
Printf.fprintf stdout "\n--- ALL tests PASSED ---\n";;

*)