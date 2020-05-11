(* Assign 03 Provided Code *)

(*  Version 1.0 *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

(* Description of g: g takes two functions f1, f2 and one pattern p. 
   if see wildcard in p, do f1. if see variable x do f2 with x. 

*)

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end


(**** put all your code after this line ****)


(* 1 *)
fun only_capitals (str_list: string list) =
    List.filter (fn x => Char.isUpper(String.sub(x,0)))  str_list


(* 2 *)
fun longest_string1 (str_list: string list) =
    List.foldl (fn (x,y) => if String.size(x) > String.size(y) then x else y) "" str_list


(* 3 *)
fun longest_string2 (str_list: string list) =
    List.foldl (fn (x,y) => if String.size(x) >= String.size(y) then x else y) "" str_list


(* 4 *)
fun longest_string_helper compare_func str_list =
    List.foldl (fn (x,y) => if compare_func(String.size(x), String.size(y)) then x else y) "" str_list

fun longest_string3 (str_list: string list) =
    longest_string_helper (fn (x,y) => x > y) str_list

fun longest_string4 (str_list: string list) =
    longest_string_helper (fn (x,y) => x >= y) str_list


(* 5 *)
fun longest_capitalized str_list =
    (longest_string1 o only_capitals) (str_list)


(* 6 *)
fun rev_string str =
    (String.implode o rev o String.explode) str


(* 7 *)
fun first_answer fun1 some_list =
    case some_list of
        [] => raise NoAnswer
      | x :: x' => let 
      					val return_val = fun1 x
                   in 
                   		case return_val of
                        	SOME x => x
                          | _ => first_answer fun1 x'
                   end


(* 8 *)
fun all_answers func1 some_list =
    let 
    	fun helper func lst lst_acc =
            case lst of
                [] => lst_acc
              | x :: x' => case func x of
                             NONE => raise NoAnswer
                           | SOME y => (helper func x' (lst_acc @ y))
    in 
    	SOME (helper func1 some_list []) handle NoAnswer => NONE 
    end


(* 9b *)
fun count_wildcards p =
    g (fn x => 1) (fn x => 0) p


(* 9c *)
fun count_wild_and_variable_lengths p =
    g (fn x => 1) (fn x => String.size(x)) p


(* 9d *)
fun count_some_var (s: string, p: pattern) =
    g (fn x => 0) (fn x => if x = s then 1 else 0) p


(* 10 *)
fun check_pat (p: pattern) =
    let 
    	fun get_strings pat =
            case pat of
               Variable z => [z]
             | TupleP ps => List.foldl (fn (p,i) => (get_strings p) @ i) [] ps
             | _ => []
        fun is_unique (str_list: string list) =
            case str_list of
                [] => true
              | x :: x' => if List.exists (fn y => x = y) x' then false else is_unique x'
    in 
    	is_unique (get_strings(p)) 
    end


(* 11 *)
fun match (v: valu, p: pattern) =
    case p of
        Wildcard => SOME []
      | Variable s => SOME [(s, v)]
      | UnitP => (case v of 
                     Unit => SOME [] 
                   | _ => NONE)
      | ConstP i => (case v of 
                        Const j => if i = j then SOME [] else NONE
                      | _ => NONE)
      | TupleP lst => (case v of
                      	  Tuple t_lst => (all_answers (fn (x,y) => match(x,y)) (ListPair.zipEq(t_lst, lst)) handle UnequalLengths => NONE)
                        | _ => NONE)
      | ConstructorP (s, pat) => (case v of
                                 	 Constructor (a, b) => if s = a then match(b, pat) else NONE
                                   | _ => NONE)



(* 12 *)
fun first_match v p_lst =
    SOME (first_answer (fn x => match(v, x)) p_lst) handle NoAnswer => NONE





