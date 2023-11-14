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

module Parse = struct

  module FCM = struct
    module type CONFPATHS = sig
      type t
      type error
      include Path.FCM.CONFPATHS
      val parse : string -> (t, error) result
      val error_msg : string -> (t, error) result
    end

    let get_config (module C : CONFPATHS) =
      let open Option in
      let* filepath =
        Path.Simple.get_config
          ~cmdline_opt:C.cmdline_opt
          ~path_vars:C.path_vars
          ~paths:C.paths
      in
      let* contents =
        catch Prelude.readfile filepath
      in
      assert false
      
      (* let parsed = 
       *   match contents_opt with
       *   | Some contents ->
       *      C.parse contents
       *   | None ->
       *      C.error_msg "bad filepath"
       * in
       * 
       * assert false *)

      
  end

end


(* let alternatives_both lookup lst =
 *   let open Option in
 *   let each_path path =
 *     let* data = catch lookup path
 *     in pure (data, path)
 *   in asum (List.map each_path lst)
 * 
 * let alternatives_data lookup lst =
 *   Option.map fst (alternatives_both lookup lst)
 * 
 * let alternatives_path lookup lst =
 *   Option.map snd (alternatives_both lookup lst)
 * 
 * module Full = struct
 *   module Simple = struct
 *     let read_paths paths =
 *       alternatives_path Prelude.readfile paths
 *     let read_vars vars =
 *       let each_var var =
 *         var |> Unix.getenv |> Prelude.readfile
 *       in
 *       alternatives_path each_var vars 
 *     let get_config_opt ~path_vars ~paths =
 *       let open Option in
 *       read_vars path_vars <|> read_paths paths
 *     let get_config ~path_vars ~paths ~fallback =
 *       let open Option in
 *       get_config_opt ~path_vars ~paths <|> pure fallback
 *       |> get
 *   end
 * 
 * end
 * 
 * module FCM = Full.FCM
 * let get_config = Full.Simple.get_config
 * let get_config_opt = Full.Simple.get_config_opt
 * 
 * module SingleOption = struct
 *   module Simple = struct
 *     let get_option_opt ~vars =
 *       alternatives_data Unix.getenv vars
 *     let get_option ~vars ~fallback =
 *       let open Option in
 *       get_option_opt ~vars
 *       <|> pure fallback
 *       |> get
 *   end
 * end
 * 
 * let get_option = SingleOption.Simple.get_option
 * let get_option_opt = SingleOption.Simple.get_option_opt *)
