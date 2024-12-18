module type REMAINDER = sig
  type 'a t
end

module Stream (Remainder : REMAINDER) = struct
  type 'parse_result t =
    { output : 'parse_result;
      backtrack : char list option;
      remainder : char Remainder.t }
end

module S = Stream (Seq)
