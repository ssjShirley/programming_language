(* if you use this function to compare two strings (returns true if the same
   string), then you avoid some warning regarding polymorphic comparison  *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for Part 1 here *)

(* 1 *)
fun all_except_option (str: string, str_list: string list) =
    let 
    	fun remove_from_list (str_to_remove, remaining_list) =
        	case remaining_list of
           		[] => []
        	  | x :: x' => if same_string(x, str_to_remove) 
          			       then x'
                           else x :: remove_from_list(str_to_remove, x')
        val filtered_list = remove_from_list(str, str_list)
    in
        if filtered_list = str_list 
        then NONE 
        else SOME filtered_list
    end


(* 2 *)
fun get_substitutions1 (substitutions: string list list, str: string) =
    case substitutions of
        [] => []
      | x :: x' => let 
      					val result = all_except_option(str,x) 
                   in
                    	case result of
                        	NONE => get_substitutions1(x', str)
                          | SOME str_list => str_list @ get_substitutions1(x', str)
                   end


(* 3 *)
fun get_substitutions2 (substitutions: string list list, str: string) =
    let 
    	fun tail_recursive (list_of_lists: string list list, matches_so_far: string list) =
            case list_of_lists of
                [] => matches_so_far
              | x::x' => let 
              				val result = all_except_option(str,x) 
              			 in
                            case result of
                                NONE => tail_recursive(x', matches_so_far)
                              | SOME str_list => tail_recursive(x', matches_so_far @ str_list)
                         end
    in
        tail_recursive(substitutions, [])
    end


(* 4 *)
fun similar_names (substitutions: string list list, {first=f: string, middle=m: string, last=l: string}) =
    let 
    	val first_names = f :: get_substitutions2(substitutions, f);
        fun helper(first_name_list: string list) =
            case first_name_list of
                [] => []
              | x::x' => {first=x, middle=m, last=l} :: helper(x')
    in
        helper(first_names)
    end



(************************************************************************)
(* Game  *)

(* you may assume that Num is always used with valid values 2, 3, ..., 10 *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw


exception IllegalMove

(* put your solutions for Part 2 here *)

(* 5 *)
fun card_color (c:card) =
    case #1 c of
    	Clubs => Black
      | Spades => Black
      | Hearts => Red
      | Diamonds => Red




(* 6 *)
fun card_value (c:card) =
    case #2 c of
        Jack => 10
      | Queen => 10
      | King => 10
      | Ace => 11
      | Num value => value



(* 7 *)
fun remove_card (cs: card list, c: card, e) =
    let 
    	fun filter_card_list (card_list: card list) =
        	case card_list of
            	[] => []
        	  | x :: xs' => if x=c then xs'
                       	   else x :: filter_card_list(xs')
        val filtered_card_list = filter_card_list(cs)
    in
        if filtered_card_list = cs 
        then raise e 
        else filtered_card_list
    end


(* 8 *)
fun all_same_color (cards: card list) =
    case cards of
        [] => true
      | x :: [] => true
      | x :: x' :: x'' => if card_color(x) = card_color(x')
                          then all_same_color(x' :: x'')
                          else false


(* 9 *)
fun sum_cards (card_list : card list) =
    let fun sum_remaining (remaining_list: card list, sum: int) =
            case remaining_list of
                [] => sum
              | x :: xs' => sum_remaining(xs', sum + card_value(x))
    in
        sum_remaining(card_list, 0)
    end




(* 10 *)
fun score (held_cards: card list, goal: int) =
    let val sum = sum_cards(held_cards)
        val pre_score = if sum > goal 
        				   then 2 * (sum - goal)
        				   else (goal - sum)
        val same_color = all_same_color(held_cards)
    in
        if same_color 
        then pre_score div 2 
        else pre_score
    end



(* 11 *)
fun officiate (card_list: card list, move_list: move list, goal: int) =
    let fun play(held_cards: card list, moves: move list, remaining_cards: card list) =
            case moves of
                [] => score(held_cards, goal)
              | x::x' => case x of
                         	Discard card => play(remove_card(held_cards, card, IllegalMove), x', remaining_cards)
                          | Draw => case remaining_cards of
                                        [] => score(held_cards, goal)
                                      | y::y' => let 
                                      				val new_held_cards = y::held_cards 
                                      			 in
                                                    if sum_cards(new_held_cards) > goal 
                                                    then score(new_held_cards, goal)
                                                    else play(new_held_cards, x', y')
                                                 end
    in
        play([], move_list, card_list)
    end




(************************************************************************)
(* Test *)


val test1_0 = all_except_option("4",["5","4","9","10"]) = SOME ["5","9","10"];


val test2_0 = get_substitutions1([["Neo","New", "Nuovo"],["Panzer","Tank","Sherman"],["Neo", "Container"],["Epoch","Era"]],
                               "Neo")
            = ["New","Nuovo","Container"];
     
            
val test3_0 = get_substitutions1([["Neo","New", "Nuovo"],["Panzer","Tank","Sherman"],["Neo", "Container"],["Epoch","Era"]],
                               "Neo")
            = ["New","Nuovo","Container"];


val test4_0 = similar_names([
                             ["Fred","Fredrick"],
                             ["Elizabeth","Betty"],
                             ["Freddie","Fred","F"]
                            ], {first="Elizabeth", middle="W", last="Smith"}) 
            =[{first="Elizabeth",last="Smith",middle="W"},
              {first="Betty",last="Smith",middle="W"}];


val test5_0 = card_color(Hearts, Num 3) = Red;


val test6_0 = card_value(Diamonds, Queen) = 10;


exception notFound
val test7_0 = remove_card([(Clubs, Ace), (Diamonds, Num 4), (Spades, Num 5), (Clubs, Ace)], (Clubs, Ace), notFound) 
			= [(Diamonds,Num 4),(Spades,Num 5),(Clubs,Ace)];


val test8_0 = all_same_color([(Clubs, Ace), (Diamonds, Num 4), (Spades, Num 5), (Clubs, Ace)]) = false;


val test9_0 = sum_cards([(Spades, Ace), (Clubs, Num 10), (Diamonds, Num 5), (Clubs, Num 9)]) = 35;


val test10_0 = score([(Spades, Ace), (Clubs, Num 10)],17) = 4;


val test11_0 = officiate([(Clubs, Ace), (Diamonds, Ace), (Spades, Num 10), (Clubs, Num 2)], [Draw, Draw], 15) = 14;



