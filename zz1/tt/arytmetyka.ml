(* autor: Tadeusz Teleżyński, 305885 *)

type wartosc = { a : float; b : float }

(* metoda pomocnicza mówiaca o tym czy float jest okreslony *)
let okreslone x = x != infinity && x != neg_infinity && x != nan;;

let wartosc_dokladnosc x p =
  let p = x /. p in
  {a=(x-.p); b=(x+.p)}

let wartosc_od_do l r =
  if l <= r then {a=l; b=r}
  else failwith 'lewy brzeg przedziału nie może być większy od prawego'

let wartosc_dokladna x =
  {a=x; b=x}

(* in_wartosc w x = x \in w *)
val in_wartosc: wartosc -> float -> bool
let in_wartosc w x =
  w.a <= x && w.b >= x

(* min_wartosc w = najmniejsza możliwa wartość w,   *)
(* lub neg_infinity jeśli brak dolnego ograniczenia.*)
val min_wartosc: wartosc -> float
let min_wartosc w = w.a

(* max_wartosc w = największa możliwa wartość w,    *)
(* lub infinity jeśli brak górnego ograniczenia.    *)
val max_wartosc: wartosc -> float
let max_wartosc w = w.b
(* środek przedziału od min_wartosc do max_wartosc, *)
(* lub nan jeśli min i max_wartosc nie są określone.*)
val sr_wartosc:  wartosc -> float


(* AAA: pierwsze pytanie -- jeśli min LUB max nie sa okreslone ?? *)

let sr_wartosc w =
  if okreslone(w.a) && okreslone(w.b) then (w.a +. w.b)/.2. else nan

(* Operacje arytmetyczne na niedokładnych wartościach. *)
val plus:      wartosc -> wartosc -> wartosc

let plus wartosc1 wartosc2 =
  {a=(wartosc1.a +. wartosc2.a); b=(wartosc1.b +. wartosc2.b)}

val minus:     wartosc -> wartosc -> wartosc

let minus wartosc1 wartosc2 =
  {a=(wartosc1.a -. wartosc2.b); b=(wartosc1.b +. wartosc2.a)}


val razy:      wartosc -> wartosc -> wartosc
val podzielic: wartosc -> wartosc -> wartosc


(* Testy *)


(* TODO *)
 (* - dodać ładne komentarze do wszystkich metod *)
