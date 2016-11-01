(************************************************)
(* Zadanie o arytmetyce niedokładnych wartości. *)
(************************************************)

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
								failwith "Niepoprawny argument funckji. x oraz p musza byc skonczonymi liczbami rzeczywistymi."
							else
								{poczatek = (x -. (abs_float x) *. p /. 100.); koniec = (x +. (abs_float x) *. p /. 100.)}
;;

(* wartosc_od_do x y = [x;y]         *)
(* war.pocz.: x <= y                 *)
let wartosc_od_do x y = if (x == infinity) || (y == neg_infinity) || (x == nan) || (y == nan) then
								failwith "Niepoprawny argument funckji. x oraz y musza byc skonczonymi liczbami rzeczywistymi."
						else
							{poczatek = x; koniec = y}
;;

(* wartosc_dokladna x = [x;x]        *)
let wartosc_dokladna x = if (x == infinity) || (x == neg_infinity) || (x == nan) then
								failwith "Niepoprawny argument funckji. x musi byc skonczona liczba rzeczywista."
							else
								{poczatek = x; koniec = x}
;;

(* in_wartosc w x = x \in w *)
let in_wartosc w x = if (w.poczatek <= w.koniec) then
						(w.poczatek <= x) && (x <= w.koniec)
					else
						(x < w.koniec) || (w.poczatek < x)
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
	let czy_antyprzedzial z = (w.poczatek > w.koniec) in
	if (czy_antyprzedzial w) && (czy_antyprzedzial v) then
		{poczatek = neg_infinity; koniec = infinity} (* wynikiem jest cala prosta rzeczywista *)
	else
		if (czy_antyprzedzial w) || (czy_antyprzedzial v) then
			if ((w.poczatek +. v.poczatek) <= (w.koniec +. v.koniec)) then
				{poczatek = neg_infinity; koniec = infinity}  (* wynikiem jest cala prosta rzeczywista *)
			else
				{poczatek = (w.poczatek +. v.poczatek); koniec = (w.koniec +. v.koniec)} (* wynikiem jest antyprzedzial *)
		else
			{poczatek = (min_wartosc w +. min_wartosc v); koniec = (max_wartosc w +. max_wartosc v)}  (* wynikiem jest przedzial lub cala prosta, jesli ktorys z przedzialow byl cala prosta *)
;;

let minus w v =
	let czy_antyprzedzial z = (w.poczatek > w.koniec) in
	if (czy_antyprzedzial w) && (czy_antyprzedzial v) then
		{poczatek = neg_infinity; koniec = infinity}  (* wynikiem jest cala prosta rzeczywista *)
	else
		if (czy_antyprzedzial w) || (czy_antyprzedzial v) then
			if ((w.poczatek -. v.koniec) <= (w.koniec -. v.poczatek)) then
				{poczatek = neg_infinity; koniec = infinity}  (* wynikiem jest cala prosta rzeczywista *)
			else
				{poczatek = (w.poczatek -. v.koniec); koniec = (w.koniec -. v.poczatek)} (* wynikiem jest antyprzedzial *)
		else
			{poczatek = (min_wartosc w -. max_wartosc v); koniec = (max_wartosc w -. min_wartosc v)} (* wynikiem jest przedzial lub cala prosta, jesli ktorys z przedzialow byl cala prosta *)
;;

(* ponizej moze miec sens zamienic ify na match, bo i tak chce wiedziec ktora wartosc jest przedzialem, a ktora antyprzedzialem *)
let razy w v =
	let czy_przedzial z = (w.poczatek <= w.koniec) in
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
				{poczatek = neg_infinity; koniec = infinity}  (* wynikiem jest cala prosta rzeczywista *)
			else
				if czy_przedzial w then
					if w.koniec < 0. then
						{poczatek = min (v.poczatek *. w.poczatek) (v.poczatek *. w.koniec);
						koniec = max (v.koniec *. w.poczatek) (v.koniec *. w.koniec)}
						(* wynikiem jest antyprzedzial *)
					else
						{poczatek = min (v.koniec *. w.poczatek) (v.koniec *. w.koniec);
						koniec = max (v.poczatek *. w.poczatek) (v.poczatek *. w.koniec)}
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
				{poczatek = max (w.poczatek *. v.poczatek) (w.koniec *. v.koniec);
				koniec  = min (w.poczatek *. v.poczatek) (w.koniec *. v.koniec)}
				(* wynikiem jest przedzial *)
;;

let podzielic w v =
    let czy_antyprzedzial z = (z.poczatek >= z.koniec)
    and czy_przedzial z = (z.poczatek <= z.koniec) in
    if (czy_antyprzedzial w) && (czy_antyprzedzial v) then
        {poczatek = neg_infinity; koniec = infinity}
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
                                                             {poczatek = neg_infinity; koniec = infinity} (* wynikiem jest cala prosta rzeczywista *)
                                                        else
                                                            {poczatek = (w.koniec /. v.poczatek); koniec = (w.poczatek /. v.koniec)}
                                                            (* wynikiem jest antyprzedzial *)

                                    | true, false -> if (w.poczatek /. v.poczatek) <= (w.koniec /. v.koniec) then
                                                             {poczatek = neg_infinity; koniec = infinity} (* wynikiem jest cala prosta rzeczywista *)
                                                        else
                                                            {poczatek = (w.poczatek /. v.poczatek); koniec = (w.koniec /. v.koniec)}
                                                            (* wynikiem jest antyprzedzial *)

                                    | false, true -> if (w.koniec /. v.koniec) <= (w.poczatek /. v.poczatek) then
                                                             {poczatek = neg_infinity; koniec = infinity} (* wynikiem jest cala prosta rzeczywista *)
                                                        else
                                                            {poczatek = (w.koniec /. v.koniec); koniec = (w.poczatek /. v.poczatek)}
                                                            (* wynikiem jest antyprzedzial *)

                                    | false, false ->   if (w.koniec /. v.poczatek) >= (w.poczatek /. v.koniec) then
                                                             {poczatek = neg_infinity; koniec = infinity} (* wynikiem jest cala prosta rzeczywista *)
                                                        else
                                                            {poczatek = (w.poczatek /. v.koniec); koniec = (w.koniec /. v.poczatek)}
                                                            (* wynikiem jest antyprzedzial *)
            | _, false ->   {poczatek = min (w.poczatek *. v.poczatek) (min (w.poczatek *. v.koniec) (min (w.koniec *. v.poczatek) (w.koniec *. v.koniec)));
		                    koniec  = max (w.poczatek *. v.poczatek) (max (w.poczatek *. v.koniec) (max (w.koniec *. v.poczatek) (w.koniec *. v.koniec)))}
                            (* wynikiem jest przedzial *)
        else
            if (in_wartosc v 0.) then
                {poczatek = neg_infinity; koniec = infinity} (* wynikiem jest cala prosta rzeczywista *)
            else
                if (min (w.koniec /. v.poczatek) (w.koniec /. v.koniec)) >= (max (w.poczatek /. v.poczatek) (w.poczatek /. v.koniec)) then
                    {poczatek = neg_infinity; koniec = infinity} (* wynikiem jest cala prosta rzeczywista *)
                else
                    {poczatek = (max (w.poczatek /. v.poczatek) (w.poczatek /. v.koniec));  koniec = (min (w.koniec /. v.poczatek) (w.koniec /. v.koniec))}
                    (* wynikiem jest antyprzedzial *)
;;
