(**exo1**)
type 'a arbre = Feuille of 'a | Arbrebin of  (( 'a arbre ) * ( 'a arbre ));;

let arb1 = Arbrebin( Feuille 5, Arbrebin(  Feuille 1, Arbrebin( Feuille 4, Feuille 2 )));;
let arb2 = Arbrebin( Feuille 12, Arbrebin(  Feuille 35, Arbrebin( Feuille 46, Feuille 53 )));;
let arb3 = Arbrebin( Arbrebin(  Feuille 1, Arbrebin( Feuille 4, Feuille 5 )), Feuille 65 );;

let rec compteNoeud arb =
	match arb with
	Feuille _ -> 0
	| Arbrebin(a1,a2) -> 1 + compteNoeud a1 + compteNoeud a2;;

compteNoeud arb1;;

let rec compteFeuille arb =
	match arb with
	Feuille _ -> 1
	| Arbrebin(a1,a2) -> compteFeuille a1 + compteFeuille a2;;

compteFeuille arb1;;

let max a b = if a < b then b else a;;

let rec profondeur arb =
	match arb with 
	Feuille _ -> 1
	| Arbrebin(a1, a2) -> 1 + max  (profondeur a1) (profondeur a2) ;;

profondeur arb1;;

let rec compar ab1 ab2 =
	match ab1 with
	Feuille _ -> (match ab2 with
		     Feuille _ -> true
		     | Arbrebin (ab21, ab22) -> false)
	| Arbrebin(a1,a2) -> (match ab2 with
		     Feuille _ -> false
		     | Arbrebin (ab21, ab22) -> (compar a1 ab21) && (compar a2 ab22));;
		     
compar arb1 arb3;;

let rec comparV2 ab1 ab2 =
	match ab1, ab2 with
	Feuille _, Feuille _ -> true
	| Arbrebin(a,c), Arbrebin (d,e) -> (comparV2 a d) && (comparV2 c e)
	| _, _ -> false;;
		     
comparV2 arb1 arb1;;		
	
let rec atol ab =
	match ab with
	Feuille x -> [x]
	| Arbrebin (a1,a2) -> atol a1 @ atol a2;; 

atol arb1;;

let rec map_arbre ab f =
	match ab with
	Feuille x -> Feuille (f x)
	| Arbrebin(a1, a2) -> Arbrebin(map_arbre a1 f, map_arbre a2 f);;

let plus n x = n + x;;

map_arbre arb1 ( plus 3 );;


(**exo2**)
type operateur_bin = Mult | Add;;
type operateur_un =  Moins;;
type arbre = 
	Const of int
	| Var of string
	| Noeud1 of (operateur_un * arbre)
	| Noeud2 of (operateur_bin * arbre * arbre);;
	
let ope = Noeud2(Add, Noeud2(Mult, Var "x",Const 3), Var "y");;
	
let rec optos opabre =
	match opabre with 
	Const x -> string_of_int x
	| Var x -> x
	| Noeud1(_,a1) -> "(" ^ "-" ^ optos a1 ^ ")"
	| Noeud2(Add, a1, a2) -> "(" ^ optos a1 ^ "+" ^ optos a2 ^ ")"
	| Noeud2(Mult, a1, a2) -> "(" ^ optos a1 ^ "*" ^ optos a2 ^ ")";;
	
optos ope;;

let listec = [("x",5); ("y",3)];;

let rec matchavec elem li =
	match li with
	[] -> false
	| (x,_)::r -> if elem = x then true else matchavec elem r;;

let rec close e li =
	match e with
	Var x -> matchavec x li
	| Const x -> true
	| Noeud1(_,e1) -> close e1 li
	| Noeud2(_,e1,e2) -> close e1 li && close e2 li;; 
	
close ope listec;;

let rec prendval li e =
	match li with 
	[] -> failwith("pas dans la liste")
	| (x,y)::r -> if e = x then y else prendval r e;;
	
let rec eval opabre li =
	match opabre with 
	Const x -> x
	| Var x -> prendval li x
	| Noeud1(_,a1) -> eval a1 li
	| Noeud2(Add, a1, a2) -> eval a1 li + eval a2 li
	| Noeud2(Mult, a1, a2) -> eval a1 li * eval a2 li;;
	
	
eval ope listec;;


(**exo3**)
type operateur = Mult | Plus | Moins;;
type arbre = C of int
	| N of (operateur * arbre list);;
	
let expArbre1 = N(Mult, [C 1; C 4; C 7]);;
let expArbre2 = N(Moins, [C 1; N(Mult,[C 5; C 2]); N(Plus,[C 1; C 2]) ]);;
let expArbre3 = N(Plus, [C 1; N(Mult,[]); N(Plus,[C 1; C 2]) ]);;

let rec compterConstanteExp ab = 
	match ab with
	C x -> 1
	| N(_, lia) -> List.fold_left (fun base a -> base + compterConstanteExp a) 0 lia;;
	
compterConstanteExp expArbre1;;

let rec exprCorrect ab = 
	match ab with
	C x -> true
	| N(_, [])  -> false
	| N(_, lia) -> List.fold_left(fun base a -> base && exprCorrect a) true lia;;
exprCorrect expArbre3;;

let rec calculer ab =
	if exprCorrect ab then
	match ab with
	C x -> x
	| N(Plus, lia) -> List.fold_left(fun base a -> base + calculer a) 0 lia
	| N(Moins, lia) -> List.fold_left(fun base a -> base - calculer a) 0 lia
	| N(Mult, lia) -> List.fold_left(fun base a -> base * calculer a) 1 lia
	else failwith "exp non correct";;

calculer expArbre1;;

let rec chaine_de_arbre ab =
	if exprCorrect ab then
	match ab with
	C x -> string_of_int x
	| N(Plus, lia) -> "(" ^ String.sub (List.fold_left(fun base a -> base ^ "+" ^ chaine_de_arbre a) "" lia) 1
				(String.length (List.fold_left(fun base a -> base ^ "+" ^ chaine_de_arbre a) "" lia) - 1) 
			  ^ ")"
	| N(Moins, lia) -> "(" ^ String.sub (List.fold_left(fun base a -> base ^ "-" ^ chaine_de_arbre a) "" lia) 1
				(String.length (List.fold_left(fun base a -> base ^ "-" ^ chaine_de_arbre a) "" lia) - 1) 
			  ^ ")"
	| N(Mult, lia) -> "(" ^ String.sub (List.fold_left(fun base a -> base ^ "*" ^ chaine_de_arbre a) "" lia) 1
				(String.length (List.fold_left(fun base a -> base ^ "*" ^ chaine_de_arbre a) "" lia) - 1) 
			  ^ ")"
	else failwith "exp non correct";;


chaine_de_arbre expArbre2;;








