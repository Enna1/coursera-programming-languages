(* Enna1, Coursera Programming Languages, HW2 Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2


(* solutions for problem 1 here *)

(* a) all_except_option:
    - desc: takes a string and a string list. Return NONE if the string is not
            in the list, else return SOME lst where lst is identical to the
            argument list except the string is not in it.
    - note: assume the string is in the list at most once.
*)
fun all_except_option(str, str_list) =
    case str_list of
        [] => NONE
    |   hd::tl => if same_string(str, hd)
                    then SOME tl
                    else case all_except_option(str, tl) of
                            NONE => NONE
                        |   SOME ss => SOME (hd::ss)


(* b) get_substitutions1:
    - desc: takes a string list list (a list of list of strings, the substitutions)
            and a string s and returns a string list. The result has all the strings
            that are in some list in substitutions that also has s, but s itself
            should not be in the result.
    - note: assume each list in substitutions has no repeats.
*)
fun get_substitutions1(substitutions, s) =
    case substitutions of
        [] => []
    |   hd::tl => case all_except_option(s, hd) of
                    NONE => get_substitutions1(tl, s)
                |   SOME ss => ss @ get_substitutions1(tl, s)


(* c) get_substitutions2:
    - desc: like get_substitutions1 except this uses a tail-recursive local
            helper function.
*)
fun get_substitutions2(substitutions, s) =
    let fun helper(sll, acc) =
            case sll of
                [] => acc
            |   hd::tl => case all_except_option(s, hd) of
                            NONE => helper(tl, acc)
                        |   SOME ss => helper(tl, acc @ ss)
    in
        helper(substitutions, [])
    end


(* d) similar_names:
    - desc: takes a string list list of substitutions and a full name of type
            {first:string,middle:string,last:string} and returns a list of full
            names (type {first:string,middle:string,last:string} list).
            The result is all the full names you can produce by substituting
            for the first name (and only the first name) using substitutions
            and parts (b) or (c).
            The answer should begin with the original name
            (then have 0 or more other names).
    - note: do not eliminate duplicates from the answer.
*)
fun similar_names(substitutions, fullname) =
    let
        val {first=x, middle=y, last=z} = fullname
        val sub_first = get_substitutions2(substitutions, x)
        fun helper(ss) =
            case ss of
                [] => []
            |   hd::tl => {first=hd, middle=y, last=z}::helper(tl)
    in
        fullname::helper(sub_first)
    end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove


(* solutions for problem 2 here *)

(* a) card_color:
    - desc: takes a card and returns its color (spades and clubs are black,
            diamonds and hearts are red).
    - note: one case-expression is enough.
*)
fun card_color(card) =
    case card of
        (Clubs, _) => Black
    |   (Spades, _) => Black
    |   (Diamonds, _) => Red
    |   (Hearts, _) => Red


(* b) card_value:
    - desc: takes a card and returns its value (numbered cards have their
            number as the value, aces are 11, everything else is 10).
    - note: one case-expression is enough.
*)
fun card_value(card) =
    case card of
        (_, Num i) => i
    |   (_, Ace) => 11
    |   (_, _) => 10


(* c) remove_card:
    - desc: takes a list of cards cs, a card c, and an exception e.
            It returns a list that has all the elements of cs except c.
            If c is in the list more than once, remove only the first one.
            If c is not in the list, raise the exception e.
    - note: you can compare cards with =.
*)
fun remove_card(cs, c, e) =
    let fun helper(cs) =
        case cs of
            [] => raise e
        |   hd::tl => if hd = c then tl else hd::helper(tl)
    in
        helper(cs)
    end


(* d) all_same_color:
    - desc: takes a list of cards and returns true if all the cards in the list
            are the same color.
*)
fun all_same_color(cs) =
    case cs of
        [] => true
    |   _::[] => true
    |   head::(neck::rest) => card_color(head) = card_color(neck) andalso all_same_color(neck::rest)


(* e) sum_cards:
    - desc: takes a list of cards and returns the sum of their values.
            Use a locally defined helper function that is tail recursive.
*)
fun sum_cards(cs) =
    let fun helper(cs, acc) =
        case cs of
            [] => acc
        |   hd::tl => helper(tl, acc+card_value(hd))
    in  helper(cs, 0)
    end


(* f) score:
    - desc: takes a card list (the held-cards) and an int (the goal) and computes
            the score as described above.
*)
fun score(held_cards, goal) =
    let val sum = sum_cards(held_cards)
        val preliminary_score = if sum > goal then 3 * (sum - goal) else goal - sum
    in
        if all_same_color(held_cards)
        then preliminary_score div 2
        else preliminary_score
    end


(* g) officiate:
    - desc: takes a card list (the card-list) a move list (what the player “does” at each point),
            and an int (the goal) and returns the score at the end of the game
            after processing (some or all of) the moves in the move list in order.
*)
fun officiate(card_list, move_list, goal) =
    let fun helper(card_list, move_list, held_cards) =
        if sum_cards(held_cards) > goal
        then score(held_cards, goal)
        else
            case move_list of
                [] => score(held_cards, goal)
            |   Draw::tl_moves => (case card_list of
                                    [] => score(held_cards, goal)
                                |   hd_card::tl_cards => helper(tl_cards, tl_moves, hd_card::held_cards))
            |   (Discard c)::tl_moves => helper(card_list, tl_moves, remove_card(held_cards, c, IllegalMove))
    in
        helper(card_list, move_list, [])
    end


(* solutions for problem 3 (challenge) here *)

fun minlist_nonempty(xs) =
    case xs of
        x::[] => x
    |   x::xs' => Int.min(x,minlist_nonempty(xs'))


(* sum_cards_challenge
    - desc: helper function used in challenge problem. return a list of all
            possible sum value of the given cards in consideration of
            each ace can have a value of 1 or 11.
*)
fun sum_cards_challenge(cs) =
    let fun helper(cs, sum, sums) =
            case cs of
                [] => sums
            |   hd::tl => (case hd of
                            (_, Ace) => helper(tl, sum-10, (sum-10)::sums)
                        |   _ => helper(tl, sum, sums))
        val sum = sum_cards(cs)
    in
        helper(cs, sum, [sum])
    end

(* a) score_challenge:
    - desc: like the non-challenge counterparts function score except each ace
            can have a value of 1 or 11.
    - note: score_challenge should always return the least (i.e., best)
            possible score.
*)
fun score_challenge(held_cards, goal) =
    let
        fun preliminary_score(sum) =
            if sum > goal then 3 * (sum - goal) else goal - sum
        fun helper(sums, pre_scores) =
            case sums of
                [] => pre_scores
            |   hd::tl => helper(tl, preliminary_score(hd)::pre_scores)
        val sums = sum_cards_challenge(held_cards)
        val pre_scores = helper(sums, [])
        val min_pre_score = minlist_nonempty(pre_scores)
    in
        if all_same_color(held_cards) then min_pre_score div 2 else min_pre_score
    end


(* a) officiate_challenge:
    - desc: like the non-challenge counterparts function officiate except each ace
            can have a value of 1 or 11.
    - note: the game-ends-if-sum-exceeds-goal rule should apply only if there
            is no sum that is less than or equal to the goal.
*)
fun officiate_challenge(card_list, move_list, goal) =
    let fun helper(card_list, move_list, held_cards) =
        if minlist_nonempty(sum_cards_challenge(held_cards)) > goal
        then score_challenge(held_cards, goal)
        else
            case move_list of
                [] => score_challenge(held_cards, goal)
            |   hd_move::tl_moves =>
                    case hd_move of
                        Draw => (case card_list of
                                    [] => score_challenge(held_cards, goal)
                                |   hd_card::tl_cards => helper(tl_cards, tl_moves, hd_card::held_cards))
                    |   Discard c => helper(card_list, tl_moves, remove_card(held_cards, c, IllegalMove))
    in
        helper(card_list, move_list, [])
    end


(* b) careful_player:
    - desc: takes a card-list and a goal and returns a move-list such that calling
            officiate with the card-list, the goal, and the move-list
*)
fun careful_player(card_list, goal) =
    let
        fun can_reach_zero_by_discard_draw(sum_held_cards, held_cards, draw_card) =
            case held_cards of
                [] => NONE
            |   hd_hc::tl_hc =>
                    if sum_held_cards - card_value(hd_hc) + card_value(draw_card) = goal
                    then SOME hd_hc
                    else can_reach_zero_by_discard_draw(sum_held_cards, tl_hc, draw_card)

        fun helper(card_list, held_cards, move_list) =
            let val curr_score = score(held_cards, goal)
            in
                if curr_score = 0
                then move_list
                else if goal - sum_cards(held_cards) > 10
                then case card_list of
                        [] => move_list @ [Draw]
                    |   hd_cl::tl_cl => helper(tl_cl, hd_cl::held_cards, move_list @ [Draw])
                else case card_list of
                        [] => (case held_cards of
                                [] => move_list
                            |   hd_hc::tl_hc =>
                                    if score(tl_hc, goal) < curr_score
                                    then helper(card_list, tl_hc, move_list @ [Discard hd_hc])
                                    else move_list)
                    |   hd_cl::tl_cl =>
                            if sum_cards(hd_cl::held_cards) <= goal
                            then helper(tl_cl, hd_cl::held_cards, move_list @ [Draw])
                            else case held_cards of
                                    [] => move_list
                                |   hd_hc::tl_hc =>
                                        (case can_reach_zero_by_discard_draw(sum_cards(held_cards), held_cards, hd_cl) of
                                            NONE => if sum_cards(hd_cl::tl_hc) <= goal
                                                    then helper(tl_cl, hd_cl::tl_hc, move_list @ [Discard hd_hc, Draw])
                                                    else if score(tl_hc, goal) < curr_score
                                                    then helper(card_list, tl_hc, move_list @ [Discard hd_hc])
                                                    else move_list
                                        |   SOME c => move_list @ [Discard c, Draw])
            end
    in
        helper(card_list, [], [])
    end