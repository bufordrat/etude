module type REMAINDER = sig
  type 'a t
end

module Stream (Remainder : REMAINDER) = struct
  type 'parse_result t =
    { head : 'parse_result option;
      backtrack : char list option;
      remainder : char Remainder.t }
end

module S = Stream (Seq)
