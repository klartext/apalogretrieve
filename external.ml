

module Strptime =
  struct
    external unixtime_of_string: string -> string -> int = "unixtime_of_string"
    (* raises exception Failure on parsing error *)
  end


