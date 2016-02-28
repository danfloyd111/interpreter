(* PAOLINI DANIELE - MAT. 444260 *)

open Interpreter;;
open Assert;;
open Printf;;

let environment = empty_env               (* funzione che identifica l'ambiente vuoto *)
let execute (e:exp) = eval  e environment;; (* helper per la valutazione delle espressioni su ambiente vuoto *)

(* TEST SU INTERI *)

printf "*** Integer test ***\n";;

(* test assegnamento e somma -> risultato atteso : Int(42) *)
let t1() = let e1 = Let("x",EInt(40),Op("+",Ide("x"),EInt(2))) in execute e1 = Int(42);;
run_test "Test1 - assignment and summation \t\t" t1;;

(* test assegnamento e sottrazione -> risultato atteso : Int(42) *)
let t2() = let e1 = Let("x",EInt(44),Op("-",Ide("x"),EInt(2))) in execute e1 = Int(42);;
run_test "Test2 - assignment and subtraction \t\t" t2;;

(* test assegnamento e moltiplicazione -> risultato atteso : Int(42) *)
let t3() = let e1 = Let("x",EInt(21),Op("*",Ide("x"),EInt(2))) in execute e1 = Int(42);;
run_test "Test3 - assignment and multiplication \t\t" t3;;

(* test assegnamento e divisione -> risultato atteso : Int(42) *)
let t4() = let e1 = Let("x",EInt(84),Op("/",Ide("x"),EInt(2))) in execute e1 = Int(42);;
run_test "Test4 - assignment and division \t\t" t4;;

(* TEST SU FUNZIONI ED ASSEGNAMENTI *)

printf "*** Functions test ***\n";;

(* test assegnamento funzione ed applicazione -> risultato atteso : Int(42) *)
let t5() = let e1 = Let("f",FunDec("x",Op("*",Ide("x"),EInt(2))),FunApp(Ide("f"),EInt(21))) in execute e1 = Int(42);;
run_test "Test5 - function definition and application \t" t5;;

(* test assegnamento funzione esprimibile -> risultato atteso : fun x -> x + 2 *)
let t6() = let e1 = Let("f",FunDec("x",FunDec("y",Op("+",Ide("x"),Ide("y")))),FunApp(Ide("f"),EInt(2)))
  in execute (FunApp(e1,EInt(40))) = Int(42);;
run_test "Test6 - expressible function \t\t\t" t6;;

(* test assegnamento doppio con somma -> risultato atteso : Int(42) *)
let t7() = let e1 = Let("x",EInt(40),Let("y",EInt(2),Op("+",Ide("x"),Ide("y")))) in execute e1 = Int(42);;
run_test "Test7 - double assignment and sum\t\t" t7;;

(* TEST DI VISIBILITA' STATICA *)

printf "*** Scoping test ***\n";;

(* test scoping statico -> risultato atteso : Int(42) *)
let t8() = let e1 = Let("y",EInt(40),Let("f",FunDec("x",Op("+",Ide("x"),Ide("y"))),Let("y",EInt(200),
  FunApp(Ide("f"),EInt(2))))) in execute e1 = Int(42);;
run_test "Test8 - static scoping test\t\t\t" t8;;

(* TEST SU BOOLEANI *)

printf "*** Boolean test ***\n";;

(* test assegnamento e and logico -> risultato atteso : Bool(false) *)
let t9() = let e1 = Let("x",EBool(true),And(Ide("x"),EBool(true))) in execute e1 = Bool(true);;
run_test "Test9 - assignment and logical and\t\t" t9;;

(* test assegnamento e or logico -> risultato atteso : Bool(false) *)
let t10() = let e1 = Let("x",EBool(false),And(Ide("x"),EBool(false))) in execute e1 = Bool(false);;
run_test "Test10 - assignment and logical or\t\t" t10;;

(* test assegnamento e not -> risultato atteso : Bool(false) *)
let t11() = let e1 = Let("x",EBool(true),Not(Ide("x"))) in execute e1 = Bool(false);;
run_test "Test11 - assignment and logical negation\t" t11;;

(* TEST IF THEN ELSE & EQUIVALENZE *)

printf "*** if-then-else and equivalences test ***\n";;

(* test assegnamento, uguaglianza e costrutto if -> risultato atteso : Int(42) *)
let t12() = let e1 = Let("x",EInt(2),If(Op("=",Ide("x"),EInt(0)),EInt(24),EInt(42))) in execute e1 = Int(42);;
run_test "Test12 - if-then-else and equality test\t" t12;;

(* test assegnamento, maggiore o uguale e costrutto if -> risultato atteso : Int(42)  *)
let t13() = let e1 = Let("x",EInt(2),If(Op(">=",Ide("x"),EInt(0)),EInt(42),EInt(24))) in execute e1 = Int(42);;
run_test "Test13 - if-then-else and greater or equal test" t13;;

(* test assegnamento, minore o uguale e costrutto if -> risultato atteso : Int(42)  *)
let t14() = let e1 = Let("x",EInt(0),If(Op(">=",Ide("x"),EInt(0)),EInt(42),EInt(24))) in execute e1 = Int(42);;
run_test "Test14 - if-then-else and lesser or equal test\t" t14;;

(* test assegnamento, maggiore e costrutto if -> risultato atteso : Int(42) *)
let t15() = let e1 = Let("x",EInt(-2),If(Op(">",Ide("x"),EInt(0)),EInt(24),EInt(42))) in execute e1 = Int(42);;
run_test "Test15 - if-then-else and greater test\t\t" t15;;

(* test assegnamento, minore e costrutto if -> risultato atteso : Int(42) *)
let t16() = let e1 = Let("x",EInt(-2),If(Op("<",Ide("x"),EInt(0)),EInt(42),EInt(24))) in execute e1 = Int(42);;
run_test "Test16 - if-then-else and lesser test\t\t" t16;;

(* TEST SU PATTERN MATCHING *)

printf "*** Pattern matching test ***\n";;

(* test assegnamento e patten matching composto -> risultato atteso : Int(42) *)
(*
  pseudo codice:
  let z = 40 in
    let w = 2 in
      try x with (+,z,w) in
        (x = 0  -> 99)::
        (x = 42 -> 42)::
        ( _     -> 88)
*)
let t17() = let e1 = Let("z",EInt(40),Let("w",EInt(2),Try("x",Op("+",Ide("z"),Ide("w")),
  Comp(Op("=",Ide("x"),EInt(0)),EInt(99),Comp(Op("=",Ide("x"),EInt(42)),EInt(42),Default(EInt(88))))))) in execute e1 = Int(42);;
run_test "Test17 - assignment and pattern matching\t" t17;;

(* test contenuto nelle specifiche -> risultato atteso : Int(42) *)
(*
  pseudo codice:
  let z = 40 in
    let w = 1 in
      try x with (+,z,w) in
        (x > 0 -> let succ x = x + 1 in succ x )::
        (x < 0 -> let abs x = if x >= 0 then x * 1 else x * -1 in abs x)::
        ( _    -> x)

  le variabili z, w sono state assegnate per semplicità, come si nota dallo pseudo codice e dal codice effettivo,
  questo test mira anche a controllare l'efficacia e la correttezza dello scoping dichiarando la variabile 'x' come
  parametro formale sia di 'try' che di 'succ' e 'abs'
*)
let t18() = let e1 =
  Let("z",EInt(40),
    Let("w",EInt(1),
      Try("x",Op("+",Ide("z"),Ide("w")),
        Comp(Op(">",Ide("x"),EInt(0)),
          Let("succ",FunDec("x",Op("+",Ide("x"),EInt(1))),FunApp(Ide("succ"),Ide("x"))),
            Comp(Op("<",Ide("x"),EInt(0)),
              Let("abs",FunDec("x",If(Op(">=",Ide("x"),EInt(0)),Op("*",Ide("x"),EInt(1)),Op("*",Ide("x"),EInt(-1)))),
              FunApp(Ide("abs"),Ide("x"))),
                Default(Op("*",Ide("x"),EInt(1)))))))) in execute e1 = Int(42);;
run_test "Test18 - test contained in the specs\t\t" t18;;

(* TEST DELLE ECCEZIONI *)

printf "*** Failures test ***\n";;
printf "*** N.B. THE FOLLOWING TESTS MUST THROWS EXCEPTIONS !!! ***\n";;

(* test operazione non riconosciuta *)
let t19() = let e1 = Op("foo_sum",EInt(2),EInt(40)) in execute e1 = Int(42);;
run_test "Test19 - unrecognized operation test" t19;;

(* test assenza di binding *)
let t20() = let e1 = Ide("x") in
  if execute e1 = Novalue then failwith "unbound identificator" else failwith "SEVERE ERROR";;
run_test "Test20 - unbound identificator test" t20;;

(* test divisione per zero *)
let t21() = let e1 = Op("/",EInt(1),EInt(0)) in execute e1 = Int(42);;
run_test "Test21 - division by zero" t21;;

(* test parametri non booleani *)
let t22() = let e1 = And(EInt(42),EInt(42)) in execute e1 = Int(42);;
run_test "Test22 - non boolean parameters" t22;;

(* test parametri non interi *)
let t23() = let e1 = Op("+",EBool(false),EBool(false)) in execute e1 = Int(42);;
run_test "Test23 - non integer parameters" t23;;

(* test applicazione di funzione su identificatore non funzione *)
let t24() = let e1 = Let("f",FunDec("x",Op("+",Ide("x"),EInt(1))),FunApp(EInt(42),EInt(42))) in execute e1 = Int(43);;
run_test "Test24 - function application on non-function ide" t24;;

(* test valore non esprimibile *)
let t25() = let e1 = Op("=",EInt(42),EBool(false)) in execute e1 = Int(42);;
run_test "Test25 - unexpressible value" t25;;

(* test pattern non coincidente *)
let t26() = let e1 = Try("x",Op("+",EInt(-1),EInt(-1)),Elem(Op("=",Ide("x"),EInt(0)),EInt(42)))
  in execute e1 = Int(42);;
run_test "Test26 - unmatched pattern" t26;;

printf "*** TEST END ***\n";;
