let alternatives lookup lst =
  let open Option in
  asum (List.map (catch lookup) lst)

module Full = struct
  module Simple = struct
    let read_paths paths =
      alternatives Prelude.readfile paths
    let read_vars vars =
      let each_var var =
        var |> Unix.getenv |> Prelude.readfile
      in
      alternatives each_var vars 
    let get_config_opt ~path_vars ~paths =
      let open Option in
      read_vars path_vars <|> read_paths paths
    let get_config ~path_vars ~paths ~fallback =
      let open Option in
      get_config_opt ~path_vars ~paths <|> pure fallback
      |> get
  end
  module FCM = struct
    module type CONFPATHS = sig
      val path_vars : string list
      val paths : string list
      val fallback : string
    end
    let get_config (module ConfPaths : CONFPATHS) =
      Simple.get_config
        ~path_vars:ConfPaths.path_vars
        ~paths:ConfPaths.paths
        ~fallback:ConfPaths.fallback
  end
end

module FCM = Full.FCM
let get_config = Full.Simple.get_config
let get_config_opt = Full.Simple.get_config_opt

module SingleOption = struct
  module Simple = struct
    let get_option_opt ~vars =
      alternatives Unix.getenv vars
    let get_option ~vars ~fallback =
      let open Option in
      get_option_opt ~vars
      <|> pure fallback
      |> get
  end
end

let get_option = SingleOption.Simple.get_option
let get_option_opt = SingleOption.Simple.get_option_opt
