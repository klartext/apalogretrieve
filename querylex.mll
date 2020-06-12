{

(*

   written by Oliver Bandel. => Urheber / COpyright Oliver Bandel

   This is free software and it can be redistributed under the
   General Public License (GPL) version 2.

*)


open Querygrammar

exception Scanner_error
let string_unquote s = String.sub s 1 (String.length s -2) (* remove first and last char *)
}


rule token = parse
    | ['s''S']['e''E']['l''L']['e''E']['c''C']['t''T'] { SELECT }
    | ['f''F']['r''R']['o''O']['m''M']                 { FROM }
    | ['w''W']['h''H']['e''E']['r''R']['e''E']         { WHERE }
    | ['g''G']['r''R']['o''O']['u''U']['p''P']         { ORDER (* THIS is wrong; but until 1.0 will be here *) }
    | ['o''O']['r''R']['d''D']['e''E']['r''R']         { ORDER }
    | ['b''B']['y''Y']                                 { BY    }
    | ['l''L']['i''I']['k''K']['e''E']                 { LIKE }

    | ['a''A']['n''N']['d''D']       { AND }
    | ['o''O']['r''R']               { OR }
    | ['n''N']['o''O']['t''T']       { NOT }



    | ['f''F']['o''O']['r''R']['m''M']['a''A']['t''T'] { FORMAT }
    | ['a''A']['l''L']['i''I']['g''G']['n''N']         { ALIGN }
    | ['l''L']['a''A']['t''T']['e''E']['x''X']         { LATEX }
    | ['h''H']['t''T']['m''M']['l''L']                 { HTML }
    | ['d''D']['e''E']['f''F']['a''A']['u''U']['l''L']['t''T'] { DEFAULT }



    | ['o''O']['u''U']['t''T']['p''P']['u''U']['t''T']  { OUTPUT }



    | "="                            { EQUALS }
    | "!="                           { NOT_EQUAL }
    | "<"                            { SMALLER }
    | ">"                            { GREATER }

    | "host"                         { ENTRY_HOST }
    | "lname"                        { ENTRY_LNAME }
    | "user"                         { ENTRY_USER }
    | "date"                         { ENTRY_DATE }
    | "request"                      { ENTRY_REQUEST }
    | "status"                       { ENTRY_STATUS }
    | "size"                         { ENTRY_SIZE }
    | "referrer"                     { ENTRY_REFERRER }
    | "client"                       { ENTRY_CLIENT }

    | "quit"                         { QUIT_COMMAND }

    | ";"                            { SEMICOLON }
    | ","                            { COMMA }
    | "*"                            { ASTERISK }


    | "("                            { LPAR }
    | ")"                            { RPAR }
    | ['"'] [^'"']* ['"']  as string { STRING (string_unquote string) }
    | ['0'-'9']+ as value { INT (value) }
    | ['a'-'z' 'A'-'Z' '_' '-' '+' '.' '0'-'9' ]+ as word { NAME (word) }
    | [' '  '\t' '\n' ] { token lexbuf }
    | ['-']             { STRING "-" }
    | eof    { raise End_of_file  }
    | _      { raise Scanner_error }


{
 (* nothing *)


 (*


 *)
}
