type date = (int * int * int)

fun year(d: date): int = #1 d
fun month(d: date): int = #2 d
fun day(d: date): int = #3 d

fun is_older(date1: date, date2: date) =
  #1 date1 < #1 date2 orelse
  (#1 date1 = #1 date2 andalso
    #2 date1 < #2 date2 orelse
    (#2 date1 = #2 date2 andalso
      #3 date1 < #3 date2
    )
  )

fun number_in_month(dates, num) =
  if null dates then 0 else
  let
    val num_in_head = if month(hd dates) = num then 1 else 0
  in
    num_in_head + number_in_month(tl dates, num)
  end

fun number_in_months(dates: date list, numbers: int list) =
  if null numbers then 0 else
  number_in_month(dates, hd numbers) + number_in_months(dates, tl numbers)

fun dates_in_month(dates: date list, num: int): date list =
  if dates = [] then [] else
  let
    val tail_dates_in_month = dates_in_month(tl dates, num)
  in
    if month(hd dates) = num 
    then (hd dates)::tail_dates_in_month
    else tail_dates_in_month
  end

fun dates_in_months(dates: date list, numbers: int list): date list =
  if numbers = [] then [] else
  dates_in_month(dates, hd numbers)@dates_in_months(dates, tl numbers)

fun get_nth(strings: string list, n: int): string = 
  if n = 1 
  then hd strings
  else get_nth(tl strings, n-1)

val months = [
  "January", "February", 
  "March", "April",
  "May", "June", 
  "July", "August",
  "September", "October",
  "November", "December"]

fun date_to_string(date1: date): string = 
  get_nth(months, month(date1)) 
  ^ " " 
  ^ Int.toString(day(date1)) 
  ^ ", " 
  ^ Int.toString(year(date1))

fun number_before_reaching_sum(sum: int, parts: int list): int = 
  if sum <= 0
  then 0
  else 
    if sum - (hd parts) <= 0
    then 0
    else 1 + number_before_reaching_sum(sum - (hd parts), tl parts)

val months_day_count = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

fun what_month(day_of_year: int): int =
  number_before_reaching_sum(day_of_year, months_day_count) + 1

fun month_range(day1: int, day2: int): int list =
  if day2 < day1
  then []
  else what_month(day1)::month_range(day1+1,day2)

fun oldest(dates: date list): date option = 
  if null dates
  then NONE
  else let 
    fun oldest_nonempty(dates : date list): date =
      let 
        val date1 = hd dates
        val rest = (tl dates)
        val oldest_from_rest = oldest(rest)
      in
        if isSome(oldest_from_rest) 
        then if is_older(date1, valOf(oldest_from_rest))
             then date1
             else valOf(oldest_from_rest)
        else date1
      end
  in
    SOME (oldest_nonempty(dates))
  end
