(* ----------------------------------------------------------------------- *)
(* here we have to do the work of date-comparisons of the apache-log-stuff *)
(* ----------------------------------------------------------------------- *)

(*
   An entry of the following form
   "[18/Nov/2007:06:38:17 +0100]"

   will be returned by the logfile-lexer as:
   "18/Nov/2007:06:38:17 +0100"

   So we check the date-string without thr '[' and ']'.

   "18/Nov/2007:06:38:17 +0100"
    00000000001111111111222222  10
    01234567890123456789012345   1

*)

type datecomp = D1_before_D2 | D1_equals_D2 | D1_after_D2


let unixepoche_of_datestring datestring =
  External.Strptime.unixtime_of_string datestring "%d/%h/%Y:%H:%M:%S %z"


let compare d1 d2 =
  let diff = unixepoche_of_datestring d1 - unixepoche_of_datestring d2 in
  if diff < 0 then D1_before_D2 else if diff = 0 then D1_equals_D2 else D1_after_D2

