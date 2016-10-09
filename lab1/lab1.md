## unix

ssh tt305885@students.mimuw.edu.pl

`top`

pytania? `man top`

`ps aux` - podobny do topa

`kill [PID]`

`kill -9 [PID]`

`killall [name]`

## oCAML

let rec silnia n = if n=0 then 1 else n * (silnia(n-1));;

let silnia x = let rec pom_sil n a =  if n = 0 then 1 else a * (pom_sil (n-1) (n-1)) in if x<0 then failwith "zly argument" else pom_sil x x;;

* wczytywanie w konsoli:

    `#use "[nazwa pliku]";;`
