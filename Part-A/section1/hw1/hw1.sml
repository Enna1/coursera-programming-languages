(* Enna1, Coursera Programming Languages, HW1 Code *)

(* is_older:
    - desc: takes two dates and evaluates to true or false. It evaluates to
            true if the first argument is a date that comes before the second
            argument. (If the two dates are the same, the result is false.)
*)
fun is_older(date1 : int * int * int, date2 : int * int * int) =
    let
        fun helper(lhs : int, rhs : int) =
            if lhs < rhs
            then true
            else false
    in
        if #1 date1 <> #1 date2
        then helper(#1 date1, #1 date2)
        else if #2 date1 <> #2 date2
        then helper(#2 date1, #2 date2)
        else helper(#3 date1, #3 date2)
    end


(* number_in_month:
    - desc: takes a list of dates and a month (i.e., an int) and returns
            how many dates in the list are in the given month.
*)
fun number_in_month(dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else
        let val tl_ans = number_in_month(tl dates, month)
        in
            if #2 (hd dates) = month
            then 1 + tl_ans
            else tl_ans
        end


(* number_in_months:
    - desc: takes a list of dates and a list of months (i.e., an int list)
            and returns the number of dates in the list of dates that are
            in any of the months in the list of months.
    - precondition: the list of months has no number repeated
*)
fun number_in_months(dates : (int * int * int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)


(* dates_in_month:
    - desc: takes a list of dates and a month (i.e., an int) and returns a
            list holding the dates from the argument list of dates that are
            in the month. The returned list should contain dates in the order
            they were originally given.
*)
fun dates_in_month(dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else
        let val tl_ans = dates_in_month(tl dates, month)
        in
            if #2 (hd dates) = month
            then (hd dates)::tl_ans
            else tl_ans
        end


(* dates_in_months:
    - desc: takes a list of dates and a list of months (i.e., an int list)
            and returns a list holding the dates from the argument list of
            dates that are in any of the months in the list of months.
    - precondition: the list of months has no number repeated
*)
fun dates_in_months(dates : (int * int * int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)


(* get_nth:
    - desc: takes a list of strings and an int n and returns the nth element of the
            list where the head of the list is 1st. It's is okay to apply hd or tl
            to the empty list in this case.
*)
fun get_nth(strings : string list, n : int) =
    if n = 1
    then hd strings
    else get_nth(tl strings, n - 1)


(* date_to_string:
    - desc: takes a date and returns a string of the form January 20, 2013 (for example)
*)
fun date_to_string(date : int * int * int) =
    let val month_names = ["January ", "February ", "March ", "April ", "May ", "June ",
            "July ", "August ", "September ", "October ", "November ", "December "]
    in get_nth(month_names, #2 date) ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end


(* number_before_reaching_sum:
    - desc: takes an int called sum, which you can assume is positive,
            and an int list, which you can assume contains all positive numbers,
            and returns an int. You should return an int n such that the first
            n elements of the list add to less than sum, but the first n + 1
            elements of the list add to sum or more.
*)
fun number_before_reaching_sum(sum : int, positive_nums : int list) =
    if hd positive_nums >= sum
    then 0
    else 1 + number_before_reaching_sum(sum - (hd positive_nums), tl positive_nums)


(* what_month:
    - desc: takes a day of year (i.e., an int between 1 and 365) and returns
            what month that day is in (1 for January, 2 for February, etc.).
*)
fun what_month(day : int) =
    let val month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in number_before_reaching_sum(day, month_days) + 1
    end


(* month_range:
    - desc: takes two days of the year day1 and day2 and returns an int list
            [m1,m2,...,mn],
            where m1 is the month of day1, m2 is the month of day1+1, ...,
            and mn is the month of day day2.
            Note the result will have length day2 - day1 + 1 or length 0 if day1>day2.
*)
fun month_range(day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1+1, day2)


(* oldest:
    - desc: takes a list of dates and evaluates to an (int*int*int) option.
            It evaluates to NONE if the list has no dates and
            SOME d if the date d is the oldest date in the list.
*)
fun oldest(dates : (int * int * int) list) =
    if null dates
    then NONE
    else
        let
            fun oldest_nonempty(dates: (int * int * int) list) =
                if null (tl dates)
                then hd dates
                else
                    let val tl_ans = oldest_nonempty(tl dates)
                    in
                        if is_older(hd dates, tl_ans)
                        then hd dates
                        else tl_ans
                    end
        in
            SOME (oldest_nonempty(dates))
        end


(* remove_duplicates:
    - desc: takes a list of ints and returns the list without duplicates.
            helper function used in number_in_months_challenge and
            dates_in_months_challenge
*)
fun remove_duplicates(xs : int list) =
    let
        fun is_in(x : int, xs : int list) =
            if null xs
            then false
            else x = hd xs orelse is_in(x, tl xs)
    in
        if null xs
        then []
        else
            let val tl_ans = remove_duplicates(tl xs)
            in
                if is_in(hd xs, tl xs)
                then tl_ans
                else hd xs :: tl_ans
            end
    end


(* number_in_months_challenge:
    - desc: takes a list of dates and a list of months (i.e., an int list)
            and returns the number of dates in the list of dates that are
            in any of the months in the list of months.
    - note: the list of months has number repeated
*)
fun number_in_months_challenge(dates : (int * int * int) list, months : int list) =
    let val months_unique = remove_duplicates(months)
    in number_in_months(dates, months_unique)
    end


(* dates_in_months_challenge:
    - desc: takes a list of dates and a list of months (i.e., an int list)
            and returns a list holding the dates from the argument list of
            dates that are in any of the months in the list of months.
    - note: the list of months has number repeated
*)
fun dates_in_months_challenge(dates : (int * int * int) list, months : int list) =
    let val months_unique = remove_duplicates(months)
    in dates_in_months(dates, months_unique)
    end


(* reasonable_date:
    - desc: takes a date and determines if it describes a real date in the common era.
            A “real date” has a positive year (year 0 did not exist), a month
            between 1 and 12, and a day appropriate for the month.
            Solutions should properly handle leap years. Leap years are years that are
            either divisible by 400 or divisible by 4 but not divisible by 100.
*)
fun reasonable_date(date : int * int * int) =
    let
        fun get_days_count(year : int, month : int) =
            let
                val common_year_month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
                fun get_nth(xs : int list, n : int) =
                    if n = 1
                    then hd xs
                    else get_nth(tl xs, n - 1)
            in
                if month = 2 andalso (year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0))
                then 29
                else get_nth(common_year_month_days, month)
            end
    in
        if #1 date <= 0
        then false
        else if #2 date <=0 orelse #2 date> 12
        then false
        else #3 date >0 andalso #3 date <= get_days_count(#1 date, #2 date)
    end
