(* Coursera Programming Languages, Homework 3, Provided Code *)

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

fun count_wildcards p =
  g (fn _ => 1) (fn _ => 0) p

fun count_wild_and_variable_lengths p =
  g (fn _ => 1) (fn x => String.size x) p

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals(strings) = 
  List.filter (fn line => if String.size(line) > 0 then
    Char.isUpper(String.sub(line, 0)) else false) strings

fun longest_string1 strings =
  List.foldl
    (fn (left, right) => if String.size(left) > String.size(right) then left else right) 
    ""
    strings

fun longest_string2 strings =
  List.foldl
    (fn (left, right) => if String.size(left) >= String.size(right) then left else right) 
    ""
    strings

fun longest_string_helper f strings =
  List.foldl
  (fn (left, right) => if f(String.size(left), String.size(right)) then left else right)
  ""
  strings

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f lst =
  case lst of
       [] => raise NoAnswer
     | a::rest => case f a of
                           NONE => first_answer f rest
                         | SOME b => b

fun all_answers f lst =
  let
    fun aux acc [] = SOME acc
      | aux _ (NONE::_) = NONE
      | aux acc (SOME bl::rest) = (aux (acc@bl) rest)
  in
    (aux [] (map f lst))
  end


