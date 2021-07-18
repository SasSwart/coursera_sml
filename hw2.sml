(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option(str: string, lst: string list): string list option =
  case lst of
       [] => NONE
     | head::rest => if same_string(str, head) 
                     then SOME(rest)
                     else case all_except_option(str, rest) of 
                               NONE   => NONE
                             | SOME l => SOME (head::l)

fun get_substitutions1(subs: string list list, s: string): string list = 
  case subs of 
       [] => []
     | head::rest => case all_except_option(s, head) of 
                                        NONE => [] @ get_substitutions1(rest, s)
                                      | SOME l => l @ get_substitutions1(rest, s)

fun get_substitutions2(subs: string list list, s: string): string list =
  let 
    fun aux(acc, subs, s) = 
      case subs of 
           [] => acc
         | head::rest => case all_except_option(s, head) of
                              NONE   => aux(acc, rest, s)
                            | SOME l => aux(acc@l, rest, s)
  in 
    aux([], subs, s)
  end

fun similar_names params =
  case params of
     (subs, {first=first,middle=middle,last=last}) => 
     let
       fun to_fullname(lst, middle, last) = 
         case lst of
              [] => []
            | first::rest => {first=first, middle=middle,
            last=last}::to_fullname(rest, middle, last)
     in
       to_fullname(first::get_substitutions2(subs, first), middle, last)
     end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color (Spades, _) = Black
  | card_color (Clubs, _) = Black
  | card_color _ = Red

fun card_value (_, Num x) = x
  | card_value (_, Ace) = 11
  | card_value _ = 10

fun remove_card (cs, c, e) = 
  case cs of
       [] => raise e
     | head::rest => if head = c
                     then rest
                     else head::remove_card(rest, c, e) 

fun all_same_color cards = 
  case cards of
       [] => true
     | head::[] => true
     | head::neck::rest => 
       if card_color(head) = card_color(neck)
       then all_same_color(neck::rest)
       else false

fun sum_cards(cards) =
  let
    fun aux(cards, acc) = 
     case cards of
         [] => acc
       | c::rest => aux(rest, acc + (card_value c))
  in
    aux(cards, 0)
  end

fun score (cards, goal) =
  let 
    val total = sum_cards(cards)
    val prelim = if total > goal then 3 * (total - goal) else goal - total
  in
    if all_same_color(cards) then prelim div 2 else prelim
  end

fun officiate (cards, moves, goal) = 
  let
    fun take_turn(held, _, [], goal) = score(held, goal)
      | take_turn(held, [], Draw::moves, goal) = score(held, goal)
      | take_turn(held, head::heap, Draw::moves, goal) = 
        let 
          val bigger_hand = head::held
          val smaller_heap = remove_card(cards, head, IllegalMove)
          val game_over = sum_cards(bigger_hand) > goal
        in
          if game_over
          then score(bigger_hand, goal)
          else take_turn(bigger_hand, smaller_heap, moves, goal)
        end
      | take_turn([], _, Discard(_)::moves, goal) = raise IllegalMove
      | take_turn(held, heap, Discard(c)::moves, goal)
        = take_turn(remove_card(held, c, IllegalMove), heap, moves, goal)
  in
    take_turn([], cards, moves, goal)
  end

