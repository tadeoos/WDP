(* autor: Tadeusz Teleżyński, 305885 *)

type wartosc = { a : float; b : float }


(* metoda pomocnicza mówiaca o tym czy float jest okreslony *)
(* let okreslone x = x != infinity && x != neg_infinity && x != nan *)

(* METODY POMOCNICZE *)

(* niezwykly przedzial to taki ktory jest suma przedzialow od nieskonczonosci *)

let czy_zwykly w = w.a<=w.b || (w.a==neg_infinity && w.b==infinity)

let skombinuj w1 w2 = [w1.a*.w2.a; w1.a*.w2.b; w1.b*.w2.b; w1.b*.w2.a]

let odwrotnosc w = {a=1./.w.b; b=1./.w.a}

let get_nonzero w = if w.a==0. then w.b else w.a

(* let sign f =
  match f with
  | 0. -> 0.
  | _ when f > 0. -> 1.
  | _ when f < 0. -> -1. *)

let sign f =
  match f>=0. with
  | true when f=0. -> 0.
  | true -> 1.
  | false -> -1.
(* let druk w = Printf.fprintf stdout "a=%f b=%f" w.a w.b *)
(* let _ = Printf.fprintf stdout "sign 1! %f\n" (sign 1.)
let _ = Printf.fprintf stdout "sign 1! %f\n" (sign 4.)
let _ = Printf.fprintf stdout "sign 0! %f\n" (sign 0.)
let _ = Printf.fprintf stdout "sign -5! %f\n" (sign (-5.))
let _ = Printf.fprintf stdout "sign -5! %f\n" (sign (-1.))
let _ = Printf.fprintf stdout "sign -5! %f\n" (sign (-0.)) *)
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
(*
let srl l =
  let rec help l n acc =
  match l, n with
  | h::t, 1 -> help l (n+1) h::acc
  | h::t, 2 -> help l (n+1) h::acc
  | _, 0 -> help l (n+1) acc
  | _ -> acc
  in help (List.sort compare l) 0 [] *)
(* METODY MODUŁU *)

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
  (* let _ = Printf.fprintf stdout "min war! %f %f \n" w.a w.b in *)
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

let plus w1 w2 =
  match czy_zwykly w1, czy_zwykly w2 with
  | true, false -> czy_cala {a=(w1.a +. w2.a); b=(w1.b +. w2.b)}
  | true, true -> {a=(w1.a +. w2.a); b=(w1.b +. w2.b)}
  | false, true -> czy_cala {a=(w1.a +. (min w2.a w2.b)); b=(w1.b +. (max w2.a w2.b))}
  | false, false -> czy_cala {a=(w1.b +. w2.a); b=(w1.a +. w2.b)}
  (* | true, false -> {a=w1. ;b=} *)
let minus w1 w2 =
  if czy_zwykly w1 && czy_zwykly w2
  then {a=(w1.a -. w2.b); b=(w1.b -. w2.a)}
  else czy_cala {a=(w1.a +. (min w2.a w2.b)); b=(w1.a -. (max 0. w2.b))}


let pom_raz w1 w2 =
  match in_wartosc w1 0. || in_wartosc w2 0. with
  | true -> wartosc_od_do (neg_infinity) (infinity)
  | false -> {b=max (w1.a*.w2.b) (w1.b*.w2.a); a=min (w1.a*.w2.a) (w1.b*.w2.b)}

let rec razy w1 w2 =
  (* let _ = Printf.fprintf stdout "razy! w1=%f %f; w2=%f %f \n" w1.a w1.b w2.a w2.b in *)
  let komb = skombinuj w1 w2
  in match czy_zwykly w1, czy_zwykly w2 with
  | true, true -> {a=minl komb; b=maxl komb}
  | true, false -> {a=min (w1.a*.w2.a) (w1.b*.w2.a); b=max (w1.a*.w2.b) (w1.b*.w2.b)}
  | false, true -> razy w2 w1
  | false, false -> pom_raz w1 w2

let pom_dziel w1 w2 =
  let p1 = w1.a/.(get_nonzero w2) in
  let p2 = w1.b/.(get_nonzero w2) in
  if w2.a==0. && w2.b==0. then wartosc_dokladna nan else
  (* jak rozlozony jest licznik, czy jest dokladna wartoscia, jak rozlozony jest mianownik,  *)
  match sign (w1.a*.w1.b), w1.a==w1.b, sign (w2.a*.w2.b), sign (get_nonzero w2) with
  | 0., true, 0., _ -> wartosc_dokladna 0.
  | 0., false, 0., _ -> {a=min 0. ((sign ((w1.a+.w1.b)*.(w2.a+.w2.b)))*.infinity); b=max 0. ((sign ((w1.a+.w1.b)*.(w2.a+.w2.b)))*.infinity) }
  | 1., _, 0., 1. -> {a=min p1 p2; b=infinity}
  | 1., _, 0., -1. -> {a=neg_infinity; b=max p1 p2}
  | 1., _, -1., _ -> {a=w1.b/.w2.b; b=w1.b/.w2.a}
  | -1., _, 1., _ when sign w2.b==1.-> {a=w1.a/.w2.b; b=w1.b/.w2.b}
  | -1., _, 1., _ when sign w2.b==(-1.)-> {a=w1.b/.w2.a; b=w1.a/.w2.a}
  | _ -> let _ = Printf.fprintf stdout "auc! %f %f %f %f\n" w1.a w1.b w2.a w2.b in failwith "nielapany wyjatek"

let podzielic w1 w2 =
  match in_wartosc w1 0., in_wartosc w2 0. with
  | false, false when czy_zwykly w1 && czy_zwykly w2-> razy w1 (odwrotnosc w2)
  | false, false -> pom_dziel w1 w2
  | false, true -> {a=max (w1.a/.w2.a) (w1.b/.w2.a); b=(min (w1.a/.w2.b) (w1.b/.w2.b))}
  | true, true -> if List.mem 0. [w1.a;w1.b;w2.a;w2.b] then pom_dziel w1 w2 else {b=infinity; a=neg_infinity}
  | true, false -> if List.mem 0. [w1.a;w1.b;w2.a;w2.b] then pom_dziel w1 w2 else {b=infinity; a=neg_infinity}

(* Testy *)


(* TODO *)
 (* - dodać ładne komentarze do wszystkich metod *)
