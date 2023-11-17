let alternatives lookup lst =
  let open Option in
  asum (List.map lookup lst)

module Path = struct
  module Simple = struct

    let check_exists path =
      if Sys.file_exists path
      then Some path
      else None

    let check_varpath_exists var =
      let open Option in
      let* path = catch Sys.getenv var in
      catch Prelude.readfile path

    let check_paths paths =
      alternatives check_exists paths

    let check_vars vars =
      alternatives check_varpath_exists vars

    let get_config ~cmdline_opt ~path_vars ~paths =
      let open Option in
      (cmdline_opt >>= check_exists)
      <|> check_vars path_vars
      <|> check_paths paths

    let get_config_fallback
          ~cmdline_opt
          ~path_vars
          ~paths
          ~fallback =
      let open Option in
      let from_system =
        get_config ~cmdline_opt ~path_vars ~paths 
      in default fallback from_system

  end

  module FCM = struct

    module type CONFPATHS = sig
      val cmdline_opt : string option
      val path_vars : string list
      val paths : string list
    end

    module type FALLBACK = sig
      include CONFPATHS
      val fallback : string
    end

    let get_config (module ConfPaths : CONFPATHS) =
      Simple.get_config
        ~cmdline_opt:ConfPaths.cmdline_opt
        ~path_vars:ConfPaths.path_vars
        ~paths:ConfPaths.paths

    let get_config_fallback (module Fallback : FALLBACK) =
      let open Option in
      let from_system =
        Simple.get_config
          ~cmdline_opt:Fallback.cmdline_opt
          ~path_vars:Fallback.path_vars
          ~paths:Fallback.paths
      in default Fallback.fallback from_system

  end
end
