ocaml doc -- program do dokumentacji, poprzez komentarze w interfejsie.

w modułach nie ma średników!
dlatego moduły kompilujemy:

        ocamlc -c naszeProcedury.mli --> naszeProcedury.cmi

        ocamlc -c naszeProcedury.ml --> naszeProcedury.cmo

kompilujemy najpiejrw mli potem ml

zamiana modułu w wykonywaly program

        ocamlc -o naszProgram naszeProcedury.cmo

        #load "naszeProcedury.cmo";;

bez load nie da sie wykonac open

gdy chcemy zeby cos sie wykonalo przy starcie modulu piszemy

        let _ = print.endline(f x)

jest moduł Printf do wypisywania na ekran i tez to print.endline
