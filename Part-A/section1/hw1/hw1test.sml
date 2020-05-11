use "hw1.sml";

val test1 = is_older((2020, 1, 1), (2020, 1, 1)) = false

val test2 = is_older((2020, 1, 1), (2020, 5, 9)) = true

val test3 = number_in_month([], 2) = 0

val test4 = number_in_month([(2020, 1, 1), (2020, 2, 2), (2020, 3, 3), (2020, 3, 4)], 4) = 0

val test5 = number_in_month([(2020, 1, 1), (2020, 2, 2), (2020, 3, 3), (2020, 3, 4)], 3) = 2

val test6 = number_in_months([], []) = 0

val test7 = number_in_months([(2020, 1, 1)], []) = 0

val test8 = number_in_months([(2020, 1, 1), (2020, 2, 2), (2020, 3, 3), (2020, 3, 4)], [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]) = 4

val test9 = dates_in_month([], 5) = []

val test10 = dates_in_month([(2020, 1, 1), (2020, 2, 2), (2020, 3, 3), (2020, 3, 4)], 5) = []

val test11 = dates_in_month([(2020, 1, 1), (2020, 2, 2), (2020, 3, 3), (2020, 3, 4)], 3) = [(2020, 3, 3), (2020, 3, 4)]

val test12 = dates_in_months([], []) = []

val test13 = dates_in_months([(2020, 1, 1), (2020, 2, 2), (2020, 3, 3), (2020, 3, 4)], [5]) = []

val test14 = dates_in_months([(2020, 1, 1), (2020, 2, 2), (2020, 3, 3), (2020, 3, 4)], [3]) = [(2020, 3, 3), (2020, 3, 4)]

val test15 = dates_in_months([(2020, 1, 1), (2020, 2, 2), (2020, 3, 3), (2020, 3, 4)], [1, 2, 3]) = [(2020, 1, 1), (2020, 2, 2), (2020, 3, 3), (2020, 3, 4)]

val test16 = get_nth(["1", "2", "3", "4"], 1) = "1"

val test17 = get_nth(["1", "2", "3", "4"], 4) = "4"

val test18 = date_to_string((2020, 1, 1)) = "January 1, 2020"

val test19 = date_to_string((2013, 1, 20)) = "January 20, 2013"

val test20 = number_before_reaching_sum(2, [1, 2, 3]) = 1

val test21 = number_before_reaching_sum(4, [1, 2, 3]) = 2

val test22 = number_before_reaching_sum(6, [1, 2, 3]) = 2

val test23 = what_month(1) = 1

val test24 = what_month(31) = 1

val test25 = what_month(365) = 12

val test26 = month_range(2,1) = []

val test27 = month_range(1,1) = [1]

val test28 = month_range(31,32) = [1, 2]

val test29 = month_range(31,59) = [1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]

val test30 = oldest([]) = NONE

val test31 = oldest([(2020, 1, 1), (2018, 12, 31), (2020, 5, 9)]) = SOME((2018, 12, 31))

val test32 = number_in_months_challenge([], []) = 0

val test33 = number_in_months_challenge([(2020, 1, 1)], []) = 0

val test34 = number_in_months_challenge([(2020, 1, 1), (2020, 2, 2), (2020, 3, 3), (2020, 3, 4)], [1, 2, 2, 3, 3, 4, 4, 5, 6, 7, 8, 9, 10, 11, 12]) = 4

val test35 = dates_in_months_challenge([], []) = []

val test36 = dates_in_months_challenge([(2020, 1, 1), (2020, 2, 2), (2020, 3, 3), (2020, 3, 4)], [5]) = []

val test37 = dates_in_months_challenge([(2020, 1, 1), (2020, 2, 2), (2020, 3, 3), (2020, 3, 4)], [3,3,5]) = [(2020, 3, 3), (2020, 3, 4)]

val test38 = dates_in_months_challenge([(2020, 1, 1), (2020, 2, 2), (2020, 3, 3), (2020, 3, 4)], [1, 2, 3, 1, 2, 3]) = [(2020, 1, 1), (2020, 2, 2), (2020, 3, 3), (2020, 3, 4)]

val test39 = reasonable_date((0,0,0)) = false

val test40 = reasonable_date((2020,5,~1)) = false

val test41 = reasonable_date((2020,5,10)) = true

val test42 = reasonable_date((2020,2,29)) = true

val test43 = reasonable_date((2021,2,29)) = false

val test44 = reasonable_date((1,0,0)) = false

val test45 = reasonable_date((1,1,0)) = false

val test46 = reasonable_date((1,1,999)) = false

val test47 = is_older ((1,2,3),(2,3,4)) = true

val test48 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1

val test49 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test50 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test51 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test52 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test53 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test54 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

val test55 = what_month 70 = 3

val test56 = month_range (31, 34) = [1,2,2,2]

val test57 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)