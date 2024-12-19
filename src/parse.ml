
(* module Stream (Remainder : REMAINDER) = struct
 *   type 'parse_result t =
 *     { output : 'parse_result;
 *       backtrack : char list option;
 *       remainder : char Remainder.t }
 * end *)

(* module S' = Stream (Seq) *)

module type REMAINDER = sig
  type 'a t
  val force : 'a t -> 'a * 'a t
end

(* module STREAMLINES (R : REMAINDER) = struct
 *   type parsed_line = { line : char list;
 *                        ending: string option;
 *                        remainder : string R.t; }
 *   type t =
 *     | Line of parsed_line
 *     | EndOfInput
 *   
 *   let next r =
 *     let open Prelude in
 *     let forced, remainder = R.force r in
 *     match String.split ~sep:"\n" forced with
 *     | "" -> EndOfInput
 *     | [str] -> Line { line = str;
 *                       ending = None;
 *                       remainder = remainder }
 *     | str : strs -> 
 * end *)

(* module S : STREAMLINES with type input = string = struct
 *   type input = string
 *   type line =
 *     | Line of char list * string option * string option
 *     | EndOfInput
 *   let next input =
 *     let open Prelude in
 *     match String.split ~sep:"\n" input with
 *     | [] -> EndOfInput
 *     | [str] -> Line (String.explode str, None, None)
 *     | str :: strs -> Line (String.explode str, Some "\n", Some strs)
 * end *)
