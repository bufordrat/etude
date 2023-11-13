val alternatives :('a -> 'b) -> 'a list -> 'b option

module FCM :sig
  module type CONFPATHS =
    sig
      val path_vars : string list
      val paths : string list
      val fallback : string
    end
  val get_config : (module CONFPATHS) -> string
end

val get_config : path_vars:string list ->
                 paths:string list ->
                 fallback:string ->
                 string
  
val get_config_opt : path_vars:string list ->
                     paths:string list ->
                     string option

val get_option : vars:string list ->
                 fallback:string ->
                 string

val get_option_opt : vars:string list ->
                     string option
