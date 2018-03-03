# OCaml
Quelques exercices en OCaml

# insert

 let rec insert a = function
	|[] -> a::[]
	|hd::l when hd>a -> a::(hd::l) 
	|hd::l -> hd::insert a l
;;

val insert : 'a -> 'a list -> 'a list = <fun>

 insert 3 [1;4;6];;
- : int list = [1; 3; 4; 6]

 insert 6 [1;2];;
- : int list = [1; 2; 6]

# sort

 let rec sort = function 
	|[] -> []
	|hd::l -> insert hd (sort l)
;;

val sort : 'a list -> 'a list = < fun >

 sort [5;2;19;5;48;7;3];;
- : int list = [2; 3; 5; 5; 7; 19; 48]

# sort avec predicat

 let rec insert2 p a = function 
	|[] -> a::[]
	|hd::l when p a hd -> a::(hd::l)
	|hd::l -> hd::insert2 p a l
;;

 let rec sort2 p = function 
	|[] -> []
	|hd::l -> insert2 p hd (sort2 p l)
;;

 sort2 ( > ) [3;1;5;6;6;42;12];;
- : int list = [42; 12; 6; 6; 5; 3; 1]

# sort avec prédicat "self-contained"

 let rec sort4 p = 
	let rec insert x y = function 
		|[]-> y::[]
		|hd::l when x y hd -> y::(hd::l)
		|hd::l -> hd::insert x y l 			
	in 
	function 
	|[] -> []
	|hd::l -> insert p hd (sort4 p l)
;;

 sort4 ( > ) [3;1;5;6;6;42;12];;
- : int list = [42; 12; 6; 6; 5; 3; 1]
