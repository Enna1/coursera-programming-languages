(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw3.sml";

val test1a = only_capitals ["A","B","C"] = ["A","B","C"]

val test1b = only_capitals ["A","b","C"] = ["A","C"]

val test1c = only_capitals ["foo","BAR","bOO"] = ["BAR"]

val test2a = longest_string1 [] = ""

val test2b = longest_string1 ["A","bc","C"] = "bc"

val test2c = longest_string1 ["A","bc","cd"] = "bc"

val test3a = longest_string2 ["A","bc","cd"] = "cd"

val test4a = longest_string3 [] = ""

val test4b = longest_string3 ["A","bc","C"] = "bc"

val test4c = longest_string3 ["A","bc","cd"] = "bc"

val test4d = longest_string4 ["A","B","C"] = "C"

val test5a = longest_capitalized ["A","bc","C"] = "A"

val test5b = longest_capitalized [] = ""

val test5c = longest_capitalized ["a","bc","c"] = ""

val test6a = rev_string "abc" = "cba"

val test6b = rev_string "" = ""

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8a = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test8b = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME []

val test8c = all_answers (fn x => if x > 1 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME [2,3,4,5,6,7]

val test9a1 = count_wildcards Wildcard = 1

val test9a2 = count_wildcards (Variable("foo")) = 0

val test9a3 = count_wildcards UnitP = 0

val test9a4 = count_wildcards (ConstP(123)) = 0

val test9a5 = count_wildcards (TupleP [Wildcard,Variable("foo"),UnitP,ConstructorP("bar",Wildcard)]) = 2

val test9b1 = count_wild_and_variable_lengths (Variable("a")) = 1

val test9b2 = count_wild_and_variable_lengths (Wildcard) = 1

val test9b3 = count_wild_and_variable_lengths (TupleP [Wildcard,Variable("foo"),UnitP,ConstructorP("bar",Wildcard)]) = 5

val test9c1 = count_some_var ("x",Variable("x")) = 1

val test9c2 = count_some_var ("x",Wildcard) = 0

val test9c3 = count_some_var ("foo",TupleP [Wildcard,Variable("foo"),UnitP,ConstructorP("foo",Wildcard)]) = 1

val test10a = check_pat (Variable("x")) = true

val test10b = check_pat (Wildcard) = true

val test10c = check_pat (TupleP [Variable("foo"),Variable("foo")]) = false

val test10d = check_pat (TupleP [Wildcard,Variable("foo"),UnitP,ConstructorP("foo",Wildcard)]) = true

val test10e = check_pat (TupleP [Wildcard,Variable("foo"),UnitP,ConstructorP("bar",Variable("foo"))]) = false

val test11a = match (Constructor("foo",Unit),Wildcard) = SOME []

val test11b = match (Constructor("foo",Unit),Variable("foo")) = SOME [("foo", Constructor("foo",Unit))]

val test11c = match (Unit,UnitP) = SOME []

val test11d = match (Unit,UnitP) = SOME []

val test11e = match (Const(1),UnitP) = NONE

val test11f = match (Const(1),ConstP(1)) = SOME []

val test11g = match (Const(11),ConstP(1)) = NONE

val test11h = match (Tuple [],TupleP [Wildcard,Variable("foo"),UnitP,ConstructorP("foo",Wildcard)]) = NONE

val test11i = match (Tuple [Unit,Constructor("foo",Unit),Unit,Constructor("foo",Unit)],
        TupleP [Wildcard,Variable("foo"),UnitP,ConstructorP("foo",Wildcard)])
    = SOME [("foo",Constructor("foo",Unit))]

val test11j = match (Tuple [Unit,Constructor("foo",Unit),Const(1),Constructor("foo",Unit)],
        TupleP [Wildcard,Variable("foo"),UnitP,ConstructorP("foo",Wildcard)])
    = NONE

val test12a = first_match Unit [UnitP] = SOME []

val test12b = first_match (Const(1)) [UnitP] = NONE

val test12c = first_match (Const(1)) [UnitP,ConstP(1)] = SOME []

val test13a = typecheck_patterns ([], [ConstP 10])
            = SOME IntT

val test13b = typecheck_patterns ([], [ConstP 10,Variable "a"])
            = SOME IntT

val test13c = typecheck_patterns ([("SOME","option",Anything)], [ConstP 10,Variable "a",ConstructorP("SOME",Variable "x")])
            = NONE

val test13d = typecheck_patterns ([], [TupleP[Variable "a", ConstP 10, Wildcard], TupleP[Variable "b", Wildcard, ConstP 11], Wildcard])
            = SOME (TupleT[Anything,IntT,IntT])

val test13e = typecheck_patterns ([("Red","color",UnitT),("Green","color",UnitT),("Blue","color",UnitT)],
                                    [ConstructorP("Red", UnitP), Wildcard])
            = SOME (Datatype "color")

val test13f = typecheck_patterns ([("Sedan","auto", Datatype "color"),("Truck","auto",TupleT[IntT, Datatype "color"]),("SUV","auto",UnitT)],
                                [ConstructorP("Sedan", Variable "a"), ConstructorP("Truck", TupleP[Variable "b", Wildcard]), Wildcard])
            = SOME (Datatype "auto")

val test13g = typecheck_patterns ([("Empty","list",UnitT),("List","list",TupleT[Anything, Datatype "list"])],
                                [ConstructorP("Empty",UnitP),ConstructorP("List",TupleP[ConstP 10, ConstructorP("Empty",UnitP)]), Wildcard])
            = SOME (Datatype "list")

val test13h = typecheck_patterns ([("Empty","list",UnitT),("List","list",TupleT[Anything, Datatype "list"])],
                                [ConstructorP("Empty",UnitP),ConstructorP("List",TupleP[Variable "k", Wildcard])])
            = SOME (Datatype "list")

val test13i = typecheck_patterns ([("Sedan","auto", Datatype "color"),("Truck","auto",TupleT[IntT, Datatype "color"]),("SUV","auto",UnitT),("Empty","list",UnitT),("List","list",TupleT[Anything, Datatype "list"])],
                                [ConstructorP("Empty",UnitP),ConstructorP("List",TupleP[ConstructorP("Sedan", Variable "c"), Wildcard])])
            = SOME (Datatype "list")

val test13j = typecheck_patterns ([],[TupleP[Variable "x",Variable "y"],TupleP[Wildcard, Wildcard]])
            = SOME (TupleT[Anything, Anything])

val test13k = typecheck_patterns ([],[TupleP[Wildcard, Wildcard],TupleP[Wildcard, TupleP[Wildcard,Wildcard]]])
            = SOME (TupleT[Anything, TupleT[Anything, Anything]])