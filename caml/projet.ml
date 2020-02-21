(**PARTIE UNE**)
let graphe1 = [(1,[6;7;8]); (2,[1;4]); (3, [2]); (4, [3;5]); (5, [1]); (6, [5;7]); (7, []); (8, [6;7])];;

(**1**)
let rec liste_sommet_graphe graphe =
	match graphe with
	[] -> []
	| (ns, li)::reste -> ns :: liste_sommet_graphe reste;;
	
(*liste_sommet_graphe graphe1;;*)

(**2**)
let rec liste_succ graphe elem =
	match graphe with 
	[] -> failwith ("ce sommet n'existe pas")
	| (ns, li)::reste -> if ns = elem then li else liste_succ reste elem;;
	
(*liste_succ graphe1 1;;*)

(**3**)
let rec ajoutelem graphe elem = 
	match graphe,elem with
	[],_ -> elem::graphe
	| (x,l)::reste, (ns,li) -> 
		if x = ns 
		then (x,l@li)::reste
		else (x,l) :: ajoutelem reste elem;;
	
let rec combiner g1 g2 =
	List.fold_left (ajoutelem) g1 g2;;
	
let rec inverser graphe =
	List.fold_left (fun base (s,lisuc) -> 
		combiner base  (List.fold_left (fun base2 ei -> ajoutelem base2 (ei,[s]) ) [] lisuc)
	) [] graphe;;


(*inverser graphe1;;*)
	
(**4**)
let rec appartient li e =
	match li with
	[] -> false
	| x :: r -> if e = x then true else appartient r e;;

let sommet noeud =
	match noeud with
	(x,li) -> x;;

let premierNoeud graphe =
	match graphe with
	[] -> failwith "pas de premier Noeud"
	| x::_ -> x;;

let rec getNode graphe sommet =
	match graphe with
	[] -> failwith "pas de sommet"
	| (s,li)::reste -> if s = sommet then (s,li) else getNode reste sommet;;

let parcours graphe = 
	let rec visite listeDejaVisit (s,li) =
		if appartient listeDejaVisit s
			then listeDejaVisit
			else List.fold_left (fun base ei -> visite (s::base) (getNode graphe ei)) listeDejaVisit li 
	in visite [] (getNode graphe 2);;
parcours graphe1;;





