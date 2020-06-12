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



let compare d1 d2 = ()

