(* PAOLINI DANIELE - MAT. 444260 *)

(* shortcuts *)
let _zerodiv = "division by zero"
let _nonexpr = "not expressible value"
let _nonbool = "non boolean parameters"
let _nonint  = "non integer parameters"
let _nonop   = "unrecognized operation"
let _nonfun  = "non function parameter"
let _nomatch = "expression not matched"

(* definizione di ambiente *)
(*
  tipo polimorfo di funzione che prende come parametro un identificatore, concretizzato da una stringa, e restituisce
  il valore di tipo 't ad esso associato
*)
type 't _env = string -> 't

(* definizione di pattern *)
(*
  tipo polimorfo per la costruzione di pattern, come vedremo verrà caratterizzato sotto in modo da poterlo sfruttare
  per la creazione di pattern su espressioni
*)
type 't _pattern =
  | Default of 't
  | Elem    of 't * 't
  | Comp    of 't * 't * 't _pattern

(* espressioni: la sintassi astratta del linguaggio, tutto ciò che l'utente può esprimere *)
type exp =
  | Ide    of string
  | EInt   of int
  | EBool  of bool
  | And    of exp * exp
  | Or     of exp * exp
  | Not    of exp
  | Op     of string * exp * exp
  | If     of exp * exp * exp
  | Let    of string * exp * exp
  | FunDec of string * exp             (* dichiarazione di funzione con singolo parametro *)
  | FunApp of exp * exp                (* applicazione di funzione con singolo parametro *)
  | Try    of string * exp * exp _pattern;; (* costrutto try ide with exp in pattern *)

(*
  N.B. per estendere il linguaggio con la possibilità di dichiarare ed applicare funzioni con più di un parametro
  si potrebbe utilizzare una lista di string * exp ed una funzione helper per gestire quest'ultima
*)

(* valori denotabili ed esprimibili *)
type value =
  | Int  of int
  | Bool of bool
  | Fun  of string * exp * value _env (* parametro, espressione ed ambiente associato al momento della definizione *)
(*| Fun  of string * exp                 dynamic scoping: parametro ed espressione, l'ambiente associato è quello
                                         presente al momento dell'applicazione
*)
  | Novalue;;

(*
  per semplicità viene caratterizzato il tipo polimorfo _env con il tipo value, adesso identifica un tipo di funzione
  che prende come parametro un identificatore, concretizzato da una stringa, e restituisce il valore esprimibile
  associato a tale identificatore (se esiste), altrimenti, come vedremo, restituirà Novalue
 *)
type env = value _env;;

(* per semplicità viene caratterizzato il tipo polimorfo pattern con il tipo exp *)
type pattern = exp _pattern;;

(*
  N.B. gli identificatori sono rappresentati direttamente da stringhe, senza definire ulteriormente il tipo
  ide = string per questioni di leggibilità
*)

(*
  ambiente vuoto, qualsiasi sia l'identificatore inserito come parametro la funzione restituisce Novalue, ovvero
  l'assenza di binding
*)
let empty_env = fun (ide:string) -> Novalue

(*
  estensione di ambiente, la funzione restituisce l'espressione se l'identificatore passato come parametro coincide
  con il binding corrente, altrimenti affida la ricerca del binding corretto alla funzione en che identificatore
  l'ambiente precedente, risalendo se necessario tutti gli ambienti esterni e terminando con l'ambiente vuoto nell
  caso non si identifichi nessun binding corretto
*)
let extend_env (en:env) (id:string) (va:value) = fun (ide:string) -> if ide = id then va else en ide

(* OPERATORI LOGICI *)

(* 'and' logico *)
let _and (a,b) = match (a,b) with
  | (Bool(x),Bool(y)) -> Bool(x && y)
  | _                 -> failwith _nonbool

(* 'or' logico *)
let _or (a,b) = match (a,b) with
  | (Bool(x),Bool(y)) -> Bool(x || y)
  | _                 -> failwith _nonbool

(* negazione logica *)
let _not a = match a with
  | Bool(x) -> if x = true then Bool(false) else Bool(true)
  | _       -> failwith _nonbool

(* OPERATORI SU INTERI *)

(* somma tra interi *)
let _add (a,b) = match (a,b) with
  | (Int(x),Int(y)) -> Int(x + y)
  | _               -> failwith _nonint

(* sottrazione tra interi *)
let _sub (a,b) = match (a,b) with
  | (Int(x),Int(y)) -> Int(x - y)
  | _               -> failwith _nonint

(* moltiplicazione tra interi *)
let _mul (a,b) = match (a,b) with
  | (Int(x),Int(y)) -> Int(x * y)
  | _               -> failwith _nonint

(* divisione intera *)
let _div (a,b) = match (a,b) with
  | (Int(x),Int(y)) -> if y = 0 then failwith _zerodiv else Int(x / y)
  | _               -> failwith _nonint

(* OPERATORI DI CONFRONTO *)

(* uguaglianza, operatore polimorfo *)
let _equ (a,b) = match (a,b) with
  | (Int(x),Int(y))   -> if x = y then Bool(true) else Bool(false)
  | (Bool(x),Bool(y)) -> if x = y then Bool(true) else Bool(false)
  | _                 -> failwith _nonexpr

(* maggiore o uguale, operatore su interi *)
let _geq (a,b) = match (a,b) with
  | (Int(x),Int(y)) -> if x >= y then Bool(true) else Bool(false)
  | _               -> failwith _nonint

(* maggiore, operatore su interi *)
let _gre (a,b) = match (a,b) with
  | (Int(x),Int(y)) -> if x > y then Bool(true) else Bool(false)
  | _               -> failwith _nonint

(* minore o uguale, operatore su interi *)
let _leq (a,b) = match (a,b) with
  | (Int(x),Int(y)) -> if x <= y then Bool(true) else Bool(false)
  | _               -> failwith _nonint

(* minore o uguale, operatore su interi *)
let _les (a,b) = match (a,b) with
  | (Int(x),Int(y)) -> if x < y then Bool(true) else Bool(false)
  | _               -> failwith _nonint

(* valutazione di espressioni -> restituisce un valore esprimibile *)
(* ex è l'espressione da valutare, mentre en è l'ambiente in cui valutarla *)
let rec eval (ex:exp) (en:env) = match ex with
  | Ide(i)        -> en i
  | EInt(v)       -> Int(v)
  | EBool(v)      -> Bool(v)
  | And(e1,e2)    -> _and((eval e1 en),(eval e2 en))
  | Or(e1,e2)     -> _or((eval e1 en),(eval e2 en))
  | Not(e1)       -> _not((eval e1 en))
  | Op(op,e1,e2)  -> (match op with
    | "+"  -> _add((eval e1 en),(eval e2 en))
    | "-"  -> _sub((eval e1 en),(eval e2 en))
    | "*"  -> _mul((eval e1 en),(eval e2 en))
    | "/"  -> _div((eval e1 en),(eval e2 en))
    | "="  -> _equ((eval e1 en),(eval e2 en))
    | ">=" -> _geq((eval e1 en),(eval e2 en))
    | ">"  -> _gre((eval e1 en),(eval e2 en))
    | "<=" -> _leq((eval e1 en),(eval e2 en))
    | "<"  -> _les((eval e1 en),(eval e2 en))
    | _    -> failwith _nonop)
  | If(g,e1,e2)   -> (if(eval g en = Bool(true)) then eval e1 en else eval e2 en) (* segue la regola esterna *)
  | Let(id,e1,e2) -> let new_en = extend_env en id (eval e1 en) in eval e2 new_en
  | FunDec(p,b)   -> Fun(p,b,en)
(*| FunDec(p,b)   -> Fun(p,b)                               dynamic scoping *)
  | FunApp(e1,e2) -> _exe((eval e1 en),(eval e2 en))
(*| FunApp(e1,e2) -> _exed((eval e1 en),(eval e2 en),en)    dynamic scoping *)
  | Try(id,e,pat) -> let new_en = extend_env en id (eval e en) in _try(new_en,pat)

(* function executor *)
and _exe (f,par) = match f with
  | Fun(p,b,efun) -> let new_en = extend_env efun p par in eval b new_en
  | _              -> failwith _nonfun

(* pattern matcher *)
and _try (en,pat) = match pat with
  | Default(e)    -> eval e en
  | Elem(e1,e2)   -> if eval e1 en = Bool(true) then eval e2 en else failwith _nomatch
  | Comp(e1,e2,p) -> if eval e1 en = Bool(true) then eval e2 en else _try (en,p)

(* Funcion executor for dynamic scoping

and _exed (f,par,en) = match f with
  | Fun(p,b) -> let new_en = extend_env en p par in eval b new_en
  | _        -> failwith _nonfun
*)
