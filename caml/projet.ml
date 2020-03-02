(**PARTIE UNE**)
let graphe1 = [(1,[6;7;8]); (2,[1;4]); (3, [2]); (4, [3;5]); (5, [1]); (6, [5;7]); (7, []); (8, [6;7])];;
let graphesanscycle = [(1, [2;3]); (2, [3]); (3, [])];;

(**1**)
(*nous donne la liste des sommet d'un graphe*)
let rec liste_sommet_graphe graphe =
	match graphe with
	[] -> []
	| (ns, li)::reste -> ns :: liste_sommet_graphe reste;;
	
(*liste_sommet_graphe graphe1;;*)

(**2**)
(*nous donne la liste des succ d'un sommet donné*)
let rec liste_succ graphe elem =
	match graphe with 
	[] -> failwith ("ce sommet n'existe pas")
	| (ns, li)::reste -> if ns = elem then li else liste_succ reste elem;;
	
(*liste_succ graphe1 1;;*)

(**3**)
(*ajoute un arc a un graphe*)
let rec ajoutelem graphe elem = 
	match graphe,elem with
	[],_ -> elem::graphe
	| (x,l)::reste, (ns,li) -> 
		if x = ns 
		then (x,l@li)::reste
		else (x,l) :: ajoutelem reste elem;;
		
(*combine deux graphe ne garde pas les redondances*)
let rec combiner g1 g2 =
	List.fold_left (ajoutelem) g1 g2;;

(*inverse un graphe*)
let rec inverser graphe =
	List.fold_left (fun base (s,lisuc) -> 
		combiner base  (List.fold_left (fun base2 ei -> ajoutelem base2 (ei,[s]) ) [] lisuc)
	) [] graphe;;
	
(*genre de bubble sort juste pour avoir les sommets dans le bon ordre'*)
let rec tri graphe =
	match graphe with
	[] -> []
	| (a,x)::(b,y)::reste -> if a < b 
	then (a,x) :: (tri ((b,y)::reste)) 
	else (tri ((b,y)::(a,x)::reste))
	| (a,x)::[] -> (a,x)::[];;
	
let bbsort graphe =
	let rec tmp graphe n =
	if n > 0
	then tmp (tri graphe) (n-1)
	else graphe
in tmp graphe (List.length graphe / 2 + 1);;

let graphe_inv = bbsort (inverser graphe1);;

	
(**4**)
(*nous donne le noeud a partir de son id*)
let rec getNode graphe sommet =
	match graphe with
	[] -> failwith "pas de sommet"
	| (s,li)::reste -> if s = sommet then (s,li) else getNode reste sommet;;

(*parcours suffixe d'un graphe*)
let parcours graphe =
(*liste de sommets deja visite et sommet courant*)
	let rec visite listeDejaVisit (s,li) =
	(*si la liste des succ est vide alors je place le sommet*)
	match li with
	[] -> if List.mem s listeDejaVisit
		then listeDejaVisit
		else s::listeDejaVisit
	(*si elle ne l'est pas alors je visite ses succs*)
	| _ ->
		List.fold_left (fun base ei ->
		if List.mem s base
 			then visite base (getNode graphe ei)
			else visite (s::base) (getNode graphe ei) 
		) listeDejaVisit li
		
in visite [] (getNode graphe 1);;

parcours graphe1;;





