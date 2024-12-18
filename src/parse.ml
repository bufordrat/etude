module type REMAINDER = sig
  type 'a t
end

module Stream (Remainder : REMAINDER) = struct
  type 'parse_result t =
    { output : 'parse_result;
      backtrack : char list option;
      remainder : char Remainder.t }
end

module S' = Stream (Seq)

module type STREAMLINES = sig
  type input
  type t =
    | Line of char list * char list option
    | EndOfInput
  val next : input -> t
end

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
