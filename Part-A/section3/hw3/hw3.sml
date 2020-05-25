(* Enna1, Coursera Programming Languages, Homework 3 Code *)

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
        |   Variable x        => f2 x
        |   TupleP ps         => List.foldl (fn (p, i) => (r p) + i) 0 ps
        |   ConstructorP(_, p) => r p
        |   _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
         | UnitT
         | IntT
         | TupleT of typ list
         | Datatype of string

(**** you can put all your code here ****)

(* problem 1 *)
fun only_capitals ss =
    List.filter (fn s => Char.isUpper(String.sub(s, 0))) ss


(* problem 2 *)
fun longest_string1 ss =
    foldl (fn (x, y) => if String.size x > String.size y then x else y) "" ss


(* problem 3 *)
fun longest_string2 ss =
    foldl (fn (x, y) => if String.size x >= String.size y then x else y) "" ss


(* problem 4 *)
fun longest_string_helper f =
    foldl (fn (x, y) => if f(String.size x, String.size y) then x else y) ""

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)


(* problem 5 *)
val longest_capitalized = longest_string1 o only_capitals


(* problem 6 *)
val rev_string = String.implode o rev o String.explode


(* problem 7 *)
fun first_answer f xs =
    case xs of
        [] => raise NoAnswer
    |   x::xs' => case f x of
                    SOME v => v
                |   NONE => first_answer f xs'

fun all_answers f xs =
    let fun helper (xs, acc) =
        case xs of
            [] => SOME acc
        |   x::xs' => case f x of
                        NONE => NONE
                    |   SOME lst => helper(xs', acc @ lst)
    in
        helper(xs, [])
    end


(* problem 9 *)

(* a *)
val count_wildcards = g (fn () => 1) (fn _ => 0)

(* b *)
val count_wild_and_variable_lengths = g (fn () => 1) String.size

(* c *)
fun count_some_var(s, p) = g (fn () => 0) (fn x => if s = x then 1 else 0) p


(* problem 10 *)
fun check_pat p =
    let
        fun get_all_var_names p =
            case p of
                Variable x        => [x]
            |   TupleP ps         => List.foldl (fn (p, acc) => (get_all_var_names p) @ acc) [] ps
            |   ConstructorP(_,p) => get_all_var_names p
            |   _                 => []
        fun name_unique ss =
            case ss of
                [] => true
            |   s::ss' => (not (List.exists (fn x => x = s) ss')) andalso name_unique ss'
    in
        (name_unique o get_all_var_names) p
    end


(* problem 11 *)
fun match(v, p) =
    case (v, p) of
        (_, Wildcard) => SOME []
    |   (_, Variable s) => SOME [(s, v)]
    |   (Unit, UnitP) => SOME []
    |   (Const i, ConstP j) => if i = j then SOME [] else NONE
    |   (Tuple vs, TupleP ps) => if length vs = length ps
                                    then all_answers match (ListPair.zip(vs, ps))
                                    else NONE
    |   (Constructor(s2, v), ConstructorP(s1, p)) => if s1 = s2
                                                    then match (v, p)
                                                    else NONE
    |   _ => NONE


(* problem 12 *)
fun first_match v ps =
    SOME (first_answer (fn p => match(v, p)) ps)
    handle NoAnswer => NONE


(* challenge problem *)
(* ((string * string * typ) list) * (pattern list) -> typ option *)
fun typecheck_patterns (ts, ps) =
    let
        fun merge_types_nonempty ts =
            let fun helper (t1, t2) =
                case (t1, t2) of
                    (Anything, Anything) => Anything
                |   (UnitT, UnitT) => UnitT
                |   (IntT, IntT) => IntT
                |   (TupleT xs1, TupleT xs2) => if length xs1 = length xs2
                                                then TupleT (List.map helper (ListPair.zip(xs1, xs2)))
                                                else raise NoAnswer
                |   (Datatype s1, Datatype s2) => if s1 = s2 then Datatype s1 else raise NoAnswer
                |   (t, Anything) => t
                |   (Anything, t) => t
                |   (_, _) => raise NoAnswer
            in
                case ts of
                    t::[] => t
                |   t1::(t2::ts') => merge_types_nonempty (helper(t1, t2)::ts')
            end
        fun first_datatype (s, p) =
            SOME (first_answer (fn (s1, s2, t) => if s1 = s then SOME (s1, s2, t) else NONE) ts)
        fun infer_type p =
            case p of
                Wildcard => Anything
            |   Variable _ => Anything
            |   UnitP => UnitT
            |   ConstP _ => IntT
            |   TupleP ps => TupleT (foldl (fn (x,acc) => acc @ [infer_type x]) [] ps)
            |   ConstructorP (s,p) => (case first_datatype (s, p) of
                                            SOME (s1, s2, t) =>
                                                (case merge_types_nonempty (t::[infer_type p]) of
                                                    _ => Datatype s2))
    in
        SOME (merge_types_nonempty(foldl (fn (p,acc) => (infer_type p) :: acc) [] ps))
        handle NoAnswer => NONE
    end
