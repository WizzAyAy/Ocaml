(*exercice 1 :*)
(*a*)	let calTTC ht = ht*.1.2;;
	calTTC 10.;;
 	let bi an = if an mod 4 = 0 then true else false;;
 	bi 2000;;
 	let min c = if c >= 'a' && c <= 'z'
 	then true
 	else false;;
 	min 'a';;
 	min 'B';;	
	
(*b*)	let moy a b = (a+.b)/.2.;;
	moy 10. 20.;;
	let div a b = (a/b, a mod b);;
	div 50 4;;

(*c*)	let p4 n = let carr n = n*n in carr n * carr n;;
	p4 5;;
	
	let toUpper c = 
	if min c 
		then let diff = int_of_char 'A' - int_of_char 'a' in char_of_int (int_of_char c + diff)	
		else c;;
	toUpper 'a';;
	
(*exercice 2 :*) 
(*a*)	let fibo n = 
		let rec fibo2 n un1 un2 =
		match n with 
		1 -> un2 
		| _ -> fibo2 (n-1) un2 (un1+un2) in
	fibo2 n 0 1;;

	fibo 10;;
	
	let rec fiboRec n =
		match n with
		0 -> 0
		| 1 -> 1
		| _ -> fiboRec (n-1) + fiboRec (n-2);;
		
 	fiboRec 10;;
 
 (*b*) let rec sommeNcar n = 
 	match n with 
 	0 -> 0;
 	| 1 -> n;
 	| _ -> n*n + sommeNcar (n-1);;
 	
 	sommeNcar 12;;
 
 (*exo3*)
 	let rec sum f n = match n with
 	1 -> f n
 	| x -> f n + sum f (n-1);;
 	
 	let carrei n = n*n;;
 	let carref n = n*.n;;
 	let plus2 n = n+2;;
 	
 	sum carrei 4;;
 
 
 	let rond f g x = g (f x);;
 	
 	rond plus2 carrei 2;;
 	
(*newton*)
	let rec newton x y esp =
	if (carref y < (x+.esp))
	then y
	else newton x  ((y+.x /. y) /. 2.) esp;;
 
 
 	newton 25. 25. 0.1;;
 	
 	
 
 
 
 
 
 
