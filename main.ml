(* ------------------ *)
 let version = "0.9.8"
(* ------------------ *)

(*
   written by Oliver Bandel. => Urheber / COpyright Oliver Bandel

   This is free software and it can be redistributed under the
   General Public License (GPL) version 2.
*)


(** The main-module contains the REPL-loop and the query-function. *)

open Querytypes


module LE = Logentry


let formatting = ref Comma_sep (* Default-value   *)
let outputfile = ref None    (* None means stdout *)
let colsep = 3         (* number of blanks to seperate the columns *)





(** This function reads the user's query from stdin. *)
let query() =

(* Abfrage-Query parsen :) *)
  begin
    try
    let lexbuf = Lexing.from_channel stdin in
      let query_result = Querygrammar.main Querylex.token lexbuf in
      query_result
    with   End_of_file -> (*prerr_endline "EOF found!"; *) Quit
       | Parsing.Parse_error -> prerr_endline "Parse Error!"; Error
  end


(* ------------------------------------------------------------------------------------ *)
(* get the sting-widths (lengths) for each entry of each sublist of the stringlist-list *)
(* ------------------------------------------------------------------------------------ *)
let get_widths li =
 let width = List.length ( (List.hd li )) in
 let maxvals = Stdlib.Array.make width 0 in

 let rec trav li idx = match li with
     []   -> ()
   | hd::tl -> maxvals.(idx) <- max maxvals.(idx) (String.length hd);
         trav tl (idx+1)
 in
   List.iter ( fun entrylist -> trav entrylist 0 ) li;
   maxvals   (* return value is the maxvals-array *)



(* convert a string to latex-friendly strings: Quoting-Things *)
(* ------------------------------------------------------------ *)
let string_to_latexstring str =
  let buf = Buffer.create 1000 in
  for idx = 0 to String.length str - 1
  do
  match str.[idx] with
    | '_' -> Buffer.add_string buf "\\_"
    | '&' -> Buffer.add_string buf "\\&"
    | '$' -> Buffer.add_string buf "\\$"
    | '#' -> Buffer.add_string buf "\\#"
    | '{' -> Buffer.add_string buf "\\{"
    | '}' -> Buffer.add_string buf "\\}"
    | '%' -> Buffer.add_string buf "\\%"
    | '~' -> Buffer.add_string buf "\\textasciitilde"

    | c   -> Buffer.add_char buf c
  done;
  Buffer.contents buf





(** REPL-loop *)
(* ========== *)
let rec loop() =
  print_string "# "; flush stdout; (* print a prompt for the user *)
  match query()          (* read user-input *)
  with   Error -> prerr_endline "ERRRRRRROR!";
          prerr_endline "try it again! ";
          loop()

     | Quit    -> prerr_endline "Beehren Sie uns auch beim naechsten mal wieder."; exit 0
     | Output file -> outputfile := Some file;
            loop()
            (*
            begin
              match file with
              None -> ()
              | Some f -> 
                    print_endline ("Not implemented!  should print to file " ^ f)
            end
            *)
     | Format form -> begin
              match form with
                Comma_sep -> formatting := Comma_sep; prerr_endline "format set to DEFAULT (comma-space seperated)"
              | Aligned   -> formatting := Aligned; prerr_endline "format set to ALIGNED (three whitespaces)"
              | LaTeX   -> formatting := LaTeX; prerr_endline "LaTeX-Table-format is not implemented so far, sorry!"
              | Html    -> formatting := Html; prerr_endline "format set to HTMl (html-table)"

            end;
            loop()

     | Query query -> (* from the successful query we got this result *)

         let filter = match query.cond with
           None    -> fun anything -> true
         | Some filt -> filt
         in

         begin
         try
           (* Reading logfile with WHERE-filter; only filtered entries in result *)
           (* ------------------------------------------------------------------ *)
           let filtered_entrylist = Readcombinedlog.readlog query.file filter in


           (* here we sort the GROUP-BY stuff *)
           (* =============================== *)

           (* --------------------------------------------------------------------------------------------- *)
           (* substitute query-Types by the strings they stand for                      *)
           (* --------------------------------------------------------------------------------------------- *)
           (* Here we also add the GROUPing-data: a pair will be constructed, which contains the      *)
           (* elements that will be used for the sort.   Because these elements possibly are not inside   *)
           (* the query itself, they must be added to have them as sort-keys inside the datastructure.    *)
           (* To use a pair here, seperates the query from the GROUP-/sorting-stuff.            *)
           (* The only reason why this is added, is, to have the possibility of sorting.          *)
           (* The sort will be possible, even if the sort-keys (e.g. "referrer") are not inside the query   *)
           (* itself.                                             *)
           (* --------------------------------------------------------------------------------------------- *)
           let sorted_filtered_stringlist_pair =
            List.map (fun entry -> (LE.getval_by_typelist entry query.sel,
                        LE.getval_by_typelist entry query.sort) ) filtered_entrylist in

           (* SORT (GROUP BY) the result by the GROUP-BY-selection *)
           (* ---------------------------------------------------- *)
           (* sorting the list by the sort-items *)
           let sorted_stuff = List.sort (fun el1 el2 -> compare (snd el1) (snd el2)) sorted_filtered_stringlist_pair in

           (* now we peel of the data from the sorted list *)
           let sorted_data = List.map (fun (comp1, comp2) -> comp1 ) sorted_stuff in



           (* print out the result *)
           (* ==================== *)
           (* only the first component will be printed, because it contains the data *)
           (* SELECT OUTCHANNEL! here!  *)
           let outchannel = begin match !outputfile with None   -> stdout | Some f -> open_out f end in

           begin
           (* depending on the selected format for the output, we put out *)
           match !formatting with
             Comma_sep -> List.iter (fun p -> Printf.fprintf outchannel "%s\n" (String.concat ", " p ) ) sorted_data
             | Aligned   -> 
                    let maxlength_arr = get_widths sorted_data in

                    (* this function prints one entry-line *)
                    (* ----------------------------------- *)
                    let rec trav li idx lenarr =
                      match li with
                        []   -> ()
                      | hd::tl ->
                            output_string outchannel hd; (* the text itself *)
                            (* the needed blanks: colsep spaces more as column-seperator *)
                            let spaces = String.make (lenarr.(idx) - String.length hd + colsep) ' '  in
                            output_string outchannel spaces;
                            trav tl (idx+1) lenarr (* next entry of the list *)
                    in
                      (* and here it will be applied *)
                      List.iter ( fun li -> trav li 0 maxlength_arr; output_char outchannel '\n' ) sorted_data

   (* =======>   ***************************************************** here: String-lengths! *)

             | LaTeX   -> output_string outchannel "should print with   LaTeX-Table - it is possibly wrong.\n";

                    (*"\begin{tabular}{ll}"*)
                    (* "|l|l|l|" *)
                    let n = List.length (List.hd sorted_data) in
                    let tabdef = (Bytes.make (2 * n + 1) '|' ) in
                    for idx = 1 to  n
                    do
                    Bytes.set tabdef (idx*2-1) 'l'
                    done;
                    output_string outchannel ( "\\begin{tabular}{" ^ (Bytes.to_string tabdef) ^ "}\\hline\n");

                    List.iter (fun p -> let ltxstrlist = List.map string_to_latexstring p in (* LaTeX-Quotings! *)
                              Printf.fprintf outchannel "%s\\\\\\hline\n" (String.concat "&" ltxstrlist)
                        ) sorted_data;
                    output_string outchannel "\\end{tabular}\n";
                    output_string outchannel "LaTeX-Table is possibly wrong, because of missing LateX-Quotations!\n"

             | Html    -> output_string outchannel "<table border=\"1\">\n";
                    List.iter (fun p -> Printf.fprintf outchannel "  <tr>\n  <td>%s</td>\n  </tr>\n" (String.concat "</td><td>" p)) sorted_data;
                    output_string outchannel "</table>\n"
           end;
           flush outchannel;
           if !outputfile <> None then close_out outchannel

         with Sys_error msg -> Printf.printf "Error: %s\n" msg (* possibly logfile does not exist *)
         end;
         loop()



(* MAIN-program *)
(* ============ *)
let _ =
  prerr_string  "apalogretrieve ";
  prerr_endline version;
  loop () (* calling the Read-Eval-Print-Loop (REPL) *)


