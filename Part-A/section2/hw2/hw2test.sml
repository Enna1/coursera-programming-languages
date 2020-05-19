(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw2.sml";

val test1 = all_except_option ("string", ["string"]) = SOME []

val test2 = all_except_option ("string", []) = NONE

val test3 = all_except_option ("string", ["foo"]) = NONE

val test4 = all_except_option ("string", ["foo", "bar"]) = NONE

val test5 = all_except_option ("string", ["foo", "bar", "baz"]) = NONE

val test6 = all_except_option ("string", ["string", "foo", "bar"]) = SOME ["foo", "bar"]

val test7 = get_substitutions1 ([], "foo") = []

val test8 = get_substitutions1 ([["foo"], ["there"]], "foo") = []

val test9 = get_substitutions1 ([["foo", "bar"], ["there"]], "foo") = ["bar"]

val test10 = get_substitutions1 ([["foo", "bar"], ["there", "foo"]], "foo") = ["bar", "there"]

val test11 = get_substitutions1 ([["Fred", "Fredrick"], ["Elizabeth", "Betty"], ["Freddie", "Fred", "F"]], "Fred") = ["Fredrick" ,"Freddie" ,"F"]

val test12 = get_substitutions1 ([["Fred", "Fredrick" ], ["Jeff", "Jeffrey" ], ["Geoff", "Jeff", "Jeffrey"]], "Jeff") = ["Jeffrey", "Geoff", "Jeffrey"]

val test13 = get_substitutions2 ([], "foo") = []

val test14 = get_substitutions2 ([["foo"], ["there"]], "foo") = []

val test15 = get_substitutions2 ([["foo", "bar"], ["there"]], "foo") = ["bar"]

val test16 = get_substitutions2 ([["foo", "bar"], ["there", "foo"]], "foo") = ["bar", "there"]

val test17 = get_substitutions2 ([["Fred", "Fredrick"], ["Elizabeth", "Betty"], ["Freddie", "Fred", "F"]], "Fred") = ["Fredrick" ,"Freddie" ,"F"]

val test18 = get_substitutions2 ([["Fred", "Fredrick" ], ["Jeff", "Jeffrey" ], ["Geoff", "Jeff", "Jeffrey"]], "Jeff") = ["Jeffrey", "Geoff", "Jeffrey"]

val test19 = similar_names ([["Fred", "Fredrick"], ["Elizabeth", "Betty"], ["Freddie", "Fred", "F"]],
                            {first="Fred", middle="W", last="Smith"}) =
	        [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	        {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test20 = similar_names ([], {first="Fred", middle="W", last="Smith"}) = [{first="Fred", last="Smith", middle="W"}]

val test21 = similar_names ([["Fred", "Fredrick"], ["Freddie", "Fred", "F", "Fredrick"]],
                            {first="Fred", middle="W", last="Smith"}) =
	        [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	        {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"},
            {first="Fredrick", last="Smith", middle="W"}]

val test22 = card_color (Clubs, Num 2) = Black

val test23 = card_color (Spades, Jack) = Black

val test24 = card_color (Hearts, Num 0) = Red

val test25 = card_color (Diamonds, Ace) = Red

val test26 = card_value (Clubs, Num 2) = 2

val test27 = card_value (Hearts, Num 0) = 0

val test28 = card_value (Diamonds, Ace) = 11

val test29 = card_value (Spades, King) = 10

val test30 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test31 = ((remove_card ([(Spades, King)], (Hearts, Ace), IllegalMove);
               false)
              handle IllegalMove => true)

val test32 = remove_card ([(Hearts, Ace), (Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Hearts, Ace)]

val test33 = remove_card ([(Hearts, Ace), (Spades, Jack), (Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Spades, Jack), (Hearts, Ace)]

val test34 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true

val test35 = all_same_color [(Hearts, Ace)] = true

val test36 = all_same_color [] = true

val test37 = all_same_color [(Hearts, Ace), (Hearts, King), (Diamonds, Queen)] = true

val test38 = all_same_color [(Spades, Ace), (Hearts, King), (Diamonds, Queen)] = false

val test39 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test40 = sum_cards [(Hearts, Ace),(Clubs, Num 2)] = 13

val test41 = sum_cards [] = 0

val test42 = sum_cards [(Hearts, King)] = 10

val test43 = sum_cards [(Hearts, King), (Diamonds, Queen)] = 20

val test44 = score ([(Hearts, Num 2), (Clubs, Num 4)], 10) = 4

val test45 = score ([(Hearts, Num 2), (Diamonds, Num 4)], 10) = 2

val test46 = score ([(Hearts, King), (Diamonds, Queen)], 10) = 15

val test47 = score ([(Hearts, King), (Diamonds, Queen)], 20) = 0

val test48 = officiate ([(Hearts, Num 2),(Clubs, Num 4)], [Draw], 15) = 6

val test49 = officiate ([(Clubs, Ace), (Spades, Ace), (Clubs, Ace), (Spades, Ace)],
                        [Draw, Draw, Draw, Draw, Draw],
                        42)
            = 3

val test50 = ((officiate([(Clubs, Jack), (Spades, Num(8))],
                         [Draw, Discard(Hearts, Jack)],
                         42);
               false)
              handle IllegalMove => true)

val test51 = score_challenge ([(Hearts, Num 2), (Clubs, Num 4)], 10) = 4

val test52 = score_challenge ([(Hearts, Num 2), (Diamonds, Num 4)], 10) = 2

val test53 = score_challenge ([(Hearts, King), (Diamonds, Queen)], 10) = 15

val test54 = score_challenge ([(Hearts, King), (Diamonds, Queen)], 20) = 0

val test55 = score_challenge ([(Hearts, Num 2), (Clubs, Num 4), (Clubs, Ace)], 10) = 3

val test56 = score_challenge ([(Hearts, Num 2), (Diamonds, Num 4), (Diamonds, Ace), (Diamonds, Ace)], 10) = 1

val test57 = officiate_challenge ([(Hearts, Num 2), (Clubs, Num 4)], [Draw], 15) = 6

val test58 = officiate_challenge ([(Clubs, Ace), (Spades, Ace), (Clubs, Ace), (Spades, Ace)],
                        [Draw, Draw, Draw, Draw, Draw],
                        42)
            = 3

val test59 = officiate_challenge ([(Clubs, Ace), (Spades, Ace), (Clubs, Ace), (Spades, Ace)],
                        [Draw, Draw, Draw, Draw, Draw],
                        2)
            = 1

val test60 = ((officiate_challenge([(Clubs, Jack), (Spades, Num(8))],
                         [Draw, Discard(Hearts, Jack)],
                         42);
               false)
              handle IllegalMove => true)

val test61 = officiate_challenge([(Clubs, Jack), (Spades, Num(8)), (Diamonds, Ace)],
                         [Draw, Discard(Clubs, Jack)],
                         42)
            = 21

val test62 = careful_player([(Clubs, Jack), (Spades, Num(8)), (Diamonds, Ace)], 42)
            = [Draw, Draw, Draw, Draw]

val test63 = careful_player([(Clubs, Jack), (Spades, Num(8)), (Diamonds, Ace)], 10)
            = [Draw]

val test64 = careful_player([(Spades, Num(8)), (Clubs, Jack), (Diamonds, Ace)], 10)
            = [Draw, Discard(Spades, Num(8)), Draw]

val test65 = careful_player([(Spades, Num(7)), (Spades, Num(1)), (Spades, Num(3)), (Diamonds, Ace)], 10)
            = [Draw, Draw, Discard(Spades, Num(1)), Draw]
