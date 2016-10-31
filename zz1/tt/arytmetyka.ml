(* autor: Tadeusz Teleżyński, 305885 *)

type wartosc = { a : float; b : float }


(* metoda pomocnicza mówiaca o tym czy float jest okreslony *)
(* let okreslone x = x != infinity && x != neg_infinity && x != nan *)

(* METODY POMOCNICZE *)

(* niezwykly przedzial to taki ktory jest suma przedzialow od nieskonczonosci *)

let rec print_list = function
[] -> ()
| e::l -> print_float e ; print_string " " ; print_list l

let czy_zwykly w = w.a<=w.b || (w.a==neg_infinity && w.b==infinity)

let skombinuj w1 w2 = [w1.a*.w2.a; w1.a*.w2.b; w1.b*.w2.b; w1.b*.w2.a]

let odwrotnosc w = {a=1./.w.b; b=1./.w.a}

(* let maxw w y =
  match w,y with
  | none,_ -> y
  | _,none -> w
  | _ -> max w y

  let maxw w y =
    match w,y with
    | none,_ -> y
    | _,none -> w
    | _ -> min w y *)
(* let druk w = Printf.fprintf stdout "a=%f b=%f" w.a w.b *)

(* co zwracac tu?  *)
let czy_cala w =
  if czy_zwykly w
  then {a=neg_infinity; b=infinity}
  else w
let minl l =
  let rec help l acc =
    match l with
    | [] -> acc
    | h::t -> if h<acc then help t h else help t acc
  in help l infinity

let maxl l =
  let rec help l acc =
    match l with
    | [] -> acc
    | h::t -> if h>acc then help t h else help t acc
  in help l neg_infinity

let wartosc_dokladnosc x p =
  let delta = (x*.p)/.100.
  in if x<0. then {a=(x+.delta); b=(x-.delta)} else {a=(x-.delta); b=(x+.delta)}

let wartosc_od_do l r =
  (* let _ = Printf.fprintf stdout "%f %f \n" l r in *)
  {a=l; b=r}

let wartosc_dokladna x = {a=x; b=x}

let in_wartosc w x =
  match czy_zwykly w with
  | true -> w.a <= x && w.b >= x
  | false -> w.a <= x || w.b >= x

let min_wartosc w =
  let _ = Printf.fprintf stdout "min war! %f %f \n" w.a w.b in
  match czy_zwykly w with
  | true -> w.a
  | false -> neg_infinity

let max_wartosc w =
  match czy_zwykly w with
  | true -> w.b
  | false -> infinity

let sr_wartosc w =
  match czy_zwykly w with
  | true -> (w.a +. w.b)/.2.
  | false -> nan

(* Operacje arytmetyczne na niedokładnych wartościach. *)
(* val plus:      wartosc -> wartosc -> wartosc *)

let plus w1 w2 =
  if czy_zwykly w1 && czy_zwykly w2
  then {a=(w1.a +. w2.a); b=(w1.b +. w2.b)}
  else czy_cala {a=(w1.a +. (min w2.a w2.b)); b=(w1.b +. (max w2.a w2.b))}

(* val minus:     wartosc -> wartosc -> wartosc *)

let minus w1 w2 =
  if czy_zwykly w1 && czy_zwykly w2
  then {a=(w1.a -. w2.b); b=(w1.b -. w2.a)}
  else czy_cala {a=(w1.a +. (min w2.a w2.b)); b=(w1.a -. (max 0. w2.b))}


(* val razy:      wartosc -> wartosc -> wartosc *)
(* val podzielic: wartosc -> wartosc -> wartosc *)

let razy w1 w2 =
  let _ = Printf.fprintf stdout "razy! w1=%f %f; w2=%f %f \n" w1.a w1.b w2.a w2.b in
  let komb = skombinuj w1 w2
  in if czy_zwykly w1 && czy_zwykly w2
  then {a=minl komb; b=maxl komb}
  else {a=maxl komb; b=minl komb}

let podzielic w1 w2 =
  match in_wartosc w2 0. with
  | false -> razy w1 (odwrotnosc w2)
  | true -> {a=max (w1.a/.w2.a) (w1.b/.w2.a); b=(min (w1.a/.w2.b) (w1.b/.w2.b))}

(* Testy *)


(* TODO *)
 (* - dodać ładne komentarze do wszystkich metod *)
