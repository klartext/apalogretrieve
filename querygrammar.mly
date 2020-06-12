/*
 written by Oliver Bandel (Urheber / Copyright)

 Free distributable under the terms of the GNU GPLv2.
*/

 

%{
(*
exception Undefined_value of string
*)

open Querytypes
open Logentry
(*
let string_unquote s = String.sub s 1 (String.length s -2)
*)

let make_comp sel const cmp r =
  cmp (sel r) const



(*
<spaltenname> LIKE  <wert>
        <wert> kann dabei Jokerzeichen enthalten :
                %   für beliebig viele unbek. Zeichen
                _   für ein  Zeichen



   s/_/\./g;
   s/%/\.\*/g;
*)
(** This function creates a matcher/filter for the like-part of a WHERE-clause. *)
let like_test contents like_re =
  let re_underscore = Str.regexp "_" in
  let re_percent = Str.regexp "%" in

  let re_string = Str.global_replace re_percent ".*" (Str.global_replace re_underscore "." like_re) in
  (*
  print_endline re_string;
  *)

  Str.string_match (Str.regexp ( "^" ^ re_string ^ "$")) contents 0
    (* partial application! *)
  
  


(** compares int-strings like int's,
    but when a value can't be converted to an int,
    it assumes the value -1 for it's conversion!
(This behavior might be changed later.)
*)
(*
let savicmp x y =
  compare ( try int_of_string x with _ -> -1) ( try int_of_string y with _ -> -1)
*)

let savi_equal     x y =  ( try int_of_string x with _ -> -1)  =  ( try int_of_string y with _ -> -1)
let savi_not_equal x y =  not (( try int_of_string x with _ -> -1)  =  ( try int_of_string y with _ -> -1))
let savi_smaller   x y =  ( try int_of_string x with _ -> -1)  <  ( try int_of_string y with _ -> -1)
let savi_greater   x y =  ( try int_of_string x with _ -> -1)  >  ( try int_of_string y with _ -> -1)

(*
  how to use make_comp:
let acomp_eq_77 = make_comp Logentry.host "lj511571.crawl.yahoo.net"  (fun x y -> x = y )

*)

module L = Logentry



(*
  Query:
    * Select-List
    * filename: string
    * condition: condtype option
    * sort: ORDER-BY values

*)



%}



%right NOT
%left OR
%left AND

%token AND OR NOT LPAR RPAR
%token EQUALS SMALLER GREATER NOT_EQUAL LIKE
%token SEMICOLON COMMA ASTERISK
%token SELECT FROM WHERE ORDER BY
%token ENTRY_HOST ENTRY_LNAME ENTRY_USER ENTRY_DATE ENTRY_REQUEST ENTRY_STATUS ENTRY_SIZE ENTRY_REFERRER ENTRY_CLIENT

%token ALIGN LATEX HTML DEFAULT
%token FORMAT

%token QUIT_COMMAND
%token OUTPUT


%token <string> STRING
%token <string> INT
%token <string> NAME

%type <Querytypes.query_result> main
%start main





%%
main: select SEMICOLON         { Query $1 }
     | QUIT_COMMAND SEMICOLON  { Quit }
     | format_cmd SEMICOLON    { $1 }
     | OUTPUT EQUALS STRING    { Output $3 }
     ;


format_cmd: FORMAT EQUALS formatlist { Format ($3) }
     ;

formatlist: ALIGN   { Aligned }
     |      LATEX   { LaTeX }
     |      HTML    { Html }
     |              { Comma_sep }
     |      DEFAULT { Comma_sep }
     ;



select: SELECT sel FROM file { { sel = $2; file = $4; cond = None; sort = [] } }
     | SELECT sel FROM file WHERE cond  { { sel = $2; file = $4; cond = Some $6; sort = [] } }
     | SELECT sel FROM file WHERE cond ORDER BY grp { { sel = $2; file = $4; cond = Some $6; sort = $9 } }
     | SELECT sel FROM file ORDER BY grp { { sel = $2; file = $4; cond = None; sort = $7 } }
     ;


file: STRING { $1 }

sel: elem_list { $1 }
     ;

grp: elem_list { $1 }
     ;



elem_list: elem { [ $1 ] }
     |         elem_list COMMA elem { $3 :: $1 }
     ;


elem: string_elem { $1 }
     | int_elem   { $1 }
     ;

string_elem:  ENTRY_HOST     { Host }
     | ENTRY_LNAME    { Lname }
     | ENTRY_USER     { User   }
     | ENTRY_DATE     { Date }
     | ENTRY_REQUEST      { Cmd }
     | ENTRY_REFERRER { Referrer }
     | ENTRY_CLIENT   { Client }
     | ASTERISK   { All }
     ;


int_elem: ENTRY_STATUS  { Retcode } 
     |    ENTRY_SIZE     { Size }
     ;




cond:  cond_simple { $1 }
     | LPAR cond RPAR { $2 }
     | NOT cond { let not_cond r = not ($2 r) in not_cond }
     | cond AND cond { let and_cond c1 c2 r = (c1 r) && (c2 r) in and_cond $1 $3 }
     | cond OR cond { let or_cond c1 c2 r = (c1 r) || (c2 r) in or_cond $1 $3 }
     ;

cond_simple:  elem_select_s EQUALS const_s  { make_comp $1 $3 ( = ) }
     | elem_select_s NOT_EQUAL const_s { make_comp $1 $3 ( <> ) }
     | elem_select_s SMALLER const_s { make_comp $1 $3 ( < ) }
     | elem_select_s GREATER const_s { make_comp $1 $3 ( > ) }
     | elem_select_s LIKE const_s { make_comp $1 $3 ( like_test ) (* ?????? AENDERN! ?????? *) }

     | elem_select_i EQUALS const_i  { make_comp $1 $3 ( savi_equal ) }
     | elem_select_i NOT_EQUAL const_i { make_comp $1 $3 ( savi_not_equal ) }
     | elem_select_i SMALLER const_i { make_comp $1 $3 ( savi_smaller ) }
     | elem_select_i GREATER const_i { make_comp $1 $3 ( savi_greater ) }
     ;


elem_select_s: ENTRY_HOST { L.host }
     | ENTRY_DATE       { L.date }
     | ENTRY_REQUEST        { L.request }
     | ENTRY_REFERRER   { L.referrer }
     | ENTRY_CLIENT     { L.client }
     | ENTRY_LNAME      { L.lname }
     | ENTRY_USER       { L.user }
     ;

elem_select_i: ENTRY_STATUS    { L.status }
     | ENTRY_SIZE       { L.size }
     ;

/*
comp: EQUALS     {}
     | SMALLER   {}
     | GREATER   {}
     ;

*/


const_s: STRING  { $1 } 
     ;



const_i: INT     { $1 }
     ;




/*
host   : string
m1       : string
m2       : string
date    : string
request    : string
status  : int
size   : int
referrer : string
client  : string
*/





%%
