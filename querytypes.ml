
(*
   written by Oliver Bandel. => Urheber / COpyright Oliver Bandel

   This is free software and it can be redistributed under the
   General Public License (GPL) version 2.
*)


(** This module contains typedefinitions for the query-handling. *)




(** This record stores the result of the Query-Parser.
    It stores the selection of entities by the SELECT-statement,
    the selected logfile-name 
    and the
    Condition (Filter) (from the WHERE-Clause) if there is one.
*)
type outform_t = Comma_sep | Aligned | LaTeX | Html

type query_t = { sel: Logentry.entry_type_t list;
                 file: string;
                 cond: (Logentry.entry_t -> bool) option;
                 sort: Logentry.entry_type_t list;
               }


(**
   This sumtype is intended to tell us what the result of the Query-parser is.
   The ReadEvalPrintLoop can give back a query,
   or tells us that we have to quit or that an error has occured.
*)
type query_result = Query of query_t | Format of outform_t | Output of string | Quit | Error

