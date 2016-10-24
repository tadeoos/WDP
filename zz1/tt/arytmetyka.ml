(* autor: Tadeusz Teleżyński, 305885 *)

type wartosc = { a : float; b : float }

let wartosc_dokladnosc x p =
  let p = x /. p in
  {a=(x-.p); b=(x+.p)}
