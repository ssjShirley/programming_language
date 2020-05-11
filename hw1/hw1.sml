(*  Assignment #1 *)

type DATE = (int * int * int)
exception InvalidParameter

(* This file is where your solutions go *)


(* 1. Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if the first argument is a date that comes before the second argument. (If the two dates are the same, the result is false.) *)
fun is_older(d1: DATE, d2: DATE): bool =   
    if (#1 d1) < (#1 d2)
    	then true
    else if (#1 d1) > (#1 d2)
    	then false
    else 
    	if (#2 d1) < (#2 d2)
    	then true
    	else if (#2 d1) > (#2 d2)
     	then false
     	else
     		if (#3 d1) < (#3 d2)
    		then true
    		else false
  
  
(* 2. Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns how many dates in the list are in the given month. *)
fun number_in_month(dl: DATE list, m: int): int = 
	if null dl
	then 0
	else if (#2(hd dl) = m) 
		then 1 + number_in_month(tl dl, m)
		else number_in_month(tl dl, m)


(* 3. Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list) and returns the number of dates in the list of dates that are in any of the months in the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the previous problem. *)
fun number_in_months(dl: DATE list, ml: int list): int = 
	if null dl
	then 0
	else if null ml
		then 0
		else number_in_month(dl, hd ml) + number_in_months(dl, tl ml)
		

(* 4. Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a list holding the dates from the argument list of dates that are in the month. The returned list should contain dates in the order they were originally given. *)
fun dates_in_month (dl: DATE list, m: int): DATE list =
    if null dl 
    then []
    else if (#2 (hd dl)) = m 
    	then hd dl :: dates_in_month(tl dl, m)
    	else dates_in_month(tl dl, m)



(* 5. Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list) and returns a list holding the dates from the argument list of dates that are in any of the months in the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the previous problem and ML’s list-append operator (@). *)
fun dates_in_months (dl: DATE list, ml: int list): DATE list =
    if null ml 
    then []
    else dates_in_month(dl, hd ml) @ dates_in_months(dl, tl ml)



(* 6. Write a function get_nth that takes a list of strings and an int n and returns the n-th element of the list where the head of the list is 1st . Raise the exception InvalidParameter if n is zero or larger than the length of the list. *)
fun get_nth (items : string list, n : int): string =
    if (n < 1) orelse (null items)
    then raise InvalidParameter
    else if n = 1
    	then hd items
    	else get_nth(tl items, n - 1)


(* 7. Write a function date_to_string that takes a date and returns a string of the form January 20, 2013 (for example). Use the operator ˆ for concatenating strings and the library function Int.toString for converting an int to a string. For producing the month part, do not use a bunch of conditionals. Instead, use a list holding 12 strings and your answer to the previous prob- lem. For consistency, put a comma following the day and use capitalized English month names: January, February, March, April, May, June, July, August, September, October, November, December. *)
fun date_to_string (date: DATE): string =
    let 
    	val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in 
    	get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end


(* 8. Write a function number_before_reaching_sum that takes an int called sum, which you can assume is positive, and an int list, which you can assume contains all positive numbers, and returns an int. You should return an int n such that the first n elements of the list add to less than sum, but the first n + 1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in value; it is okay for an exception to occur if this is not the case. *)
fun number_before_reaching_sum (sum: int, nums: int list): int =
    if hd nums >= sum 
    then 0
    else 1 + number_before_reaching_sum(sum - hd nums, tl nums)



(* 9. Write a function what_month that takes a day of year (i.e., an int between 1 and 365) and returns what month that day is in (1 for January, 2 for February, etc.). Use a list holding 12 integers and your answer to the previous problem. *)
fun what_month (d: int): int =
    let 
    	val month_sums = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in 
    	number_before_reaching_sum(d, month_sums) + 1
    end



(* 10. Write a function month_range that takes two days of the year day1 and day2 and returns an int list [m1,m2,...,mn]wherem1isthemonthofday1,m2isthemonthofday1+1,...,andmnis the month of day day2. Note the result will have length day2 − day1 + 1 or length 0 if day1 > day2. *)
fun month_range (d1: int, d2: int): int list =
    if d1 > d2 
    then []
    else what_month(d1) :: month_range(d1 + 1, d2)


(* 11. Write a function oldest that takes a list of dates and evaluates to a DATE option. It evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list. *)
fun oldest(dl: DATE list) =
    if null dl 
    then NONE
    else
        let fun oldest_helper (dl: DATE list) =
            if null (tl dl) 
            then hd dl
            else
               let val tail_oldest = oldest_helper(tl dl)
               in
                   if is_older(hd dl, tail_oldest) 
                   then hd dl
                   else tail_oldest
               end
        in
            SOME (oldest_helper(dl))
        end


(* 12. Write a function reasonable_date that takes a date and determines if it describes a real date in the common era. A “real date” has a positive year (year 0 did not exist), a month between 1 and 12, and a day appropriate for the month. Solutions should properly handle leap years. Leap years are years that are either divisible by 400 or divisible by 4 but not divisible by 100. (Do not worry about days possibly lost in the conversion to the Gregorian calendar in the Late 1500s.) *)
fun reasonable_date (d: DATE): bool =
    let 
    	val year = #1 d
    	val month = #2 d
    	val day = #3 d
    	fun get_days(m: int, y: int): int =
    		if m = 2 andalso (y mod 400 = 0 orelse y mod 4 = 0 andalso not (y mod 100 = 0))
    		then 29
    		else
        		let val month_sums = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
            		fun helper(cur_month: int, months: int list) =
                		if cur_month = 1 then hd months
                		else helper(cur_month-1, tl months)
        		in
           			 helper(m, month_sums)
        		end
    in
    	if year > 0 andalso month > 0 andalso month < 13 andalso day > 0 andalso day < get_days(month, year)+1
        then true
        else false
    end






















