(**exe1**)

let rec longueur li = match li with
	[] -> 0
	| x::r -> 1 + longueur r;;
	
longueur (2::1::[]);;

let rec concat li1 li2 = match li1 with
	[] -> li2
	| x::r -> x::(concat  r (li2));;
	
concat (1::2::3::[]) (4::5::6::[]);;
	
let tete li = match li with
	[] -> failwith "pas de tete"
	| x::r -> x;;
	
let pop li = match li with
	[] -> failwith "liste deja vide"
	| x::r -> r
	
let rec nieme li nb = match nb with
	1 -> tete li
	| x -> nieme (pop li) (nb-1);;
	
nieme (concat (1::2::3::[]) (4::5::6::[])) 2;;
	
let rec niemelist li nb = match li with
	[] -> failwith "pas d'element a cette pos"
	| x::r -> if nb = 1 
		then x
		else niemelist r (nb-1);; 
niemelist (concat (1::2::3::[]) (4::5::6::[])) 2;;
	
	
(**exe2*)

let rec npremiers li n = 
	 match li with
	[] -> failwith "liste trop petite"
	| x::r -> if n = 0
		then []
		else x::(npremiers r (n-1));;
	
npremiers (concat (1::2::3::[]) (4::5::6::[])) 5;;



let rec met_a_plat lili =
	match lili with
	[] -> []
	| x::r -> x @ met_a_plat r ;;


met_a_plat ((1::2::3::[])::(4::5::6::[])::[]);; 
	
(* (1::2::3::[], 4::5::6::[]) -> ( (1,4)::(2,5)::(3,6)::[] ) *)
let rec paire_vers_liste couli = match couli with
	([],[]) -> []
	| ([], _::r) -> failwith "pas la meme taille"
	| (_::r, []) -> failwith "pas la meme taille"
	| (x::r1,y::r2) -> (x,y)::(paire_vers_liste(r1,r2));;
	
paire_vers_liste (1::2::3::[], 4::5::6::[]);;

(*  ( (1,4)::(2,5)::(3,6)::[] ) -> (1::2::3::[], 4::5::6::[]) *)
(*let rec liste_vers_paire li = match li with
	[] -> ([],[])
	| (a,b)::r ->*)
	

let rec supprime1 li x = match li with
	[] -> li
	| a::r -> if a = x
		then r
		else a::(supprime1 r x);;
		
supprime1 (concat (1::2::3::[]) (4::5::6::[])) 3;;
	 
	
let rec supprime2 li x = match li with
	[] -> li
	| a::r -> if a = x
		then (supprime2 r x)
		else a::(supprime2 r x);;
	
supprime2 (concat (3::2::3::[]) (3::2::3::[])) 3;;


let min_list li = 
	let rec fnc_min l min = match l with
		[] -> min
		| a::r -> if a < min
			then fnc_min r a
			else fnc_min r min
	in fnc_min li (tete li);;
	
min_list (concat (1::2::-3::[]) (3::3::-6::[]));;

let rec doublon l = match l with
	[] -> l
	| a::r -> a::(doublon (supprime2 r a));;
	
doublon (concat (3::2::3::[]) (3::2::3::1::2::3::4::5::6::[]));;
	

(**exe 3*)
(*1*)
let inser_tete li x = x::li;;
 
inser_tete (1::2::3::[]) 5;;
 
let rec inser_tete_elem ll elem = 
	match ll with
	[] -> []
	| li::r -> (inser_tete li elem) :: (inser_tete_elem r elem);;
	
inser_tete_elem ( (1::2::3::[])::(3::2::1::[])::(4::5::6::[])::[] ) 0;;

let rec parties li = 
	match li with
	[] -> [[]]
	| x::r -> (inser_tete_elem (parties r) x) @ parties(r);;
	
parties (1::2::3::[]);;

(*2*)
let rec detruit nb li =
	match li with
	[] -> []
	| x::r -> 	if (longueur x = nb)
			then x :: (detruit nb r)
			else detruit nb r;;
			
detruit 2 [ [1]; [1;2]; [1;2;3]; [3;4] ];;

let rec sous_listes nb li =
	if (nb > longueur li)
		then failwith "impossible car nb < longueur"
		else detruit nb (parties li);;

sous_listes 1 [1;2;3;4;5;6;7;8;9;10];;

(**exo4*)
let inser_tete_map lili elem =
	List.map ( fun li -> elem :: li ) lili;;
	
inser_tete_map [ [1;2;3]; [3;2;1]; [4;5;6] ] 0;;

(*let rec parties_map li =
	List.map ( function*)
	

(**exo5*)
let longueur_fold li =
	List.fold_left ( fun compteur e -> compteur+1) 0 li;;
	
longueur_fold [1;2;3;4;5];;


let concat_fold l1 l2 =
	List.fold_right ( fun e1 l2 -> e1::l2 ) l1 l2;;
	
concat_fold [1;2;3] [4;5;6];;

let met_a_plat_fold lili = 
	List.fold_left ( fun base li -> base @ li) [] lili;;

met_a_plat_fold [ [1;2;3]; [4;5;6] ];;

let supprime2_fold li elem =
	List.fold_left ( fun base ei -> if ei = elem then base else base @ [ei]) [] li;;

supprime2_fold [1;2;3;1;5;6;7;1;9] 1;;


let rec appartient e li =
	match li with 
	[] -> false
	| x::r -> if x=e
		then true
		else appartient e r;; 

let doublon_fold li =
	List.fold_left ( fun base ei -> if (appartient ei base) then base else base @ [ei] ) [] li;;
	
doublon_fold [1;1;1;1;2;2;2;2;3;3;3;3;4;5;6];;






