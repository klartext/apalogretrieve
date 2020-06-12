type entry_t = {
  host : string;
  lname : string;
  user : string;
  date : string;
  request : string;
  status : string;
  size : string;
  referrer : string;
  client : string;
}
type entry_type_t =
    Host
  | Lname
  | User
  | Date
  | Cmd
  | Retcode
  | Size
  | Referrer
  | Client
  | All
val create :
  host:string ->
  lname:string ->
  user:string ->
  date:string ->
  request:string ->
  status:string -> size:string -> referrer:string -> client:string -> entry_t
val host : entry_t -> string
val lname : entry_t -> string
val user : entry_t -> string
val date : entry_t -> string
val request : entry_t -> string
val status : entry_t -> string
val size : entry_t -> string
val referrer : entry_t -> string
val client : entry_t -> string
val name_of_entrytype : entry_type_t -> string
val getval_by_typelist : entry_t -> entry_type_t list -> string list
val get_lengths_by_typelist : entry_t -> entry_type_t list -> int list
