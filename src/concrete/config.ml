module Standard = struct
  let alternatives f lst =
    let open Option in
    asum (List.map (catch f) lst)

  let read_paths paths = alternatives Prelude.readfile paths

  let read_vars vars = alternatives Unix.getenv vars

  let get_config ~path_vars ~paths ~fallback =
    let open Option in
    read_vars path_vars <|> read_paths paths <|> pure fallback
    |> get
end
include Standard

module FCM = struct
  module type CONFPATHS = sig
    val path_vars : string list
    val paths : string list
    val fallback : string
  end

  let get_config (module ConfPaths : CONFPATHS) =
    Standard.get_config
      ~path_vars:ConfPaths.path_vars
      ~paths:ConfPaths.paths
      ~fallback:ConfPaths.fallback
end
