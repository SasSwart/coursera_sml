val only_capitals_test1 = 
  only_capitals(["A", "b", "Valid", "to_be_filtered", ""]) = ["A", "Valid"]


val longest_string1_test1 = 
  longest_string1 ["sas"] = "sas"
val longest_string1_test2 = 
  longest_string1 ["", "a", "b", "ab", "sas", "swa", "rt"] = "sas"
val longest_string2_test1 = 
  longest_string2 ["", "a", "b", "ab", "sas", "swa", "rt"] = "swa" 

val longest_string3_test1 = 
  longest_string3 ["", "a", "b", "ab", "sas", "swa", "rt"] = "sas"
val longest_string4_test1 = 
  longest_string4 ["", "a", "b", "ab", "sas", "swa", "rt"] = "swa"

val longest_capitalized_test1 =
  longest_capitalized (["A", "b", "Valid", "to_be_filtered", ""]) = "Valid"

val longest_capitalized_test2 =
  longest_capitalized (["", "a", "b", "ab", "sas", "swa", "rt"]) = ""

val longest_capitalized_test3 =
  longest_capitalized (["", "a", "b", "ab", "Sas", "Swa", "rt"]) = "Sas"

val rev_string_test1 =
  rev_string "Sas Swart" = "trawS saS"

val count_wildcards_test1 = 
  count_wildcards (TupleP [Wildcard, ConstructorP ("Test", Wildcard)]) = 2

val count_wildcards_test2 = 
  count_wildcards (ConstP 5) = 0

val count_wild_and_variable_lengths_test1 = 
    count_wild_and_variable_lengths (ConstP 5) = 0

val count_wild_and_variable_lengths_test2 = 
  count_wild_and_variable_lengths Wildcard = 1

val count_wild_and_variable_lengths_test3 = 
  count_wild_and_variable_lengths (TupleP [Wildcard, Variable "Sas Swart"]) = 10
