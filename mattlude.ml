(* mattlude
 * mattlude.ml
 * Copyright (c) 2021 Matt Teichman. All rights reserved.
 * Distributed under the ISC license, see terms at the end of the file.
 * Mattlude Version 1.0
 *)

open Prelude

(** [version] is the library version metadata alist. *)
(* let version = V.data *)

module type FUNCTOR = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end
   
module type APPLICATIVE = sig
  include FUNCTOR
  val product : 'a t -> 'b t -> ('a * 'b) t
end

module type MONAD = sig
  type 'a t
  val pure : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

                 
module FunctorGoodies (F : FUNCTOR) = struct
  let (let+) x f = F.map f x
  let (>>|) = (let+)
  let (<&>) = (let+)
  let (>|=) = (let+)
  let (<$>) = F.map
end
                  
module ApplicativeGoodies (A : APPLICATIVE) = struct
  include FunctorGoodies (A)
  let (and+) = A.product
  let apply af ax =
    A.map
      (fun (f, x) -> f x)
      (A.product af ax)
  let (<*>) = apply
end

module MonadGoodies (M : MONAD) = struct
  let pure = M.pure
  let bind = M.bind
  let (>>=) = bind
  let (let*) = bind
  let (>=>) mf mg x = mf x >>= mg
  let (<=<) mf mg x = mg x >>= mf
  let (>>) mx my = mx >>= fun _ -> my
end
                               
module Monad2App (M : MONAD) = struct
  include MonadGoodies (M)

  (* reduction of applicative interface to monadic interface *)
  let map f mx = let* x = mx in
                 pure (f x)
  let product ax ay = let* x = ax in
                      let* y = ay in
                      pure (x,y)

  module I = struct
    type 'a t = 'a M.t
    let map = map
    let product = product
  end

  include ApplicativeGoodies (I)
  let ( <* ) ax ay = pure k <*> ax <*> ay
  let ( *> ) ax ay = pure (flip k) <*> ax <*> ay
end

(* helper functions for optional values *)
module Option = struct

  (* unwraps the Somes; throws the None-s out *)
  include Option
  let rec cat_options = function
    | [] -> []
    | Some x :: xs -> x :: cat_options xs
    | None :: xs -> cat_options xs

  (* for auto-generating monad and applicative stuff *)
  module OptionMonad = struct
    type 'a t = 'a option
    let pure = Option.some
    let bind = Option.(>>=)
  end

  include Monad2App (OptionMonad)
end
let cat_options = Option.cat_options

module type ERROR = sig
  type t
end
                
(* module functor for building a Result module with cool extra stuff in it;
   takes a module containing the error type as an input *)
module ResultF (E : ERROR) = struct
  include Result

  (* for auto-generating monad and applicative stuff *)
  module ResultMonad = struct
    type 'a t = ('a, E.t) result
    let pure = Result.ok
    let bind = Result.(>>=)
  end

  include Monad2App (ResultMonad)
end

module type MONOID = sig
  type 'a t
  val empty : 'a t
  val (++) : 'a t -> 'a t -> 'a t
end

module type FOLDABLE = sig
  include MONOID
  val foldl : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
end

module type TOKEN = sig
  include FOLDABLE
  type tok
  type stream = tok t
  val hd : 'tok t -> 'tok
  val tl : 'tok t -> 'tok t
  val tail : 'tok t -> ('tok t) option
  val head : 'tok t -> 'tok option
  val cons : 'tok -> 'tok t -> 'tok t
end

module ParserF (T : TOKEN) = struct
  module PResult = ResultF (String)

  module ParserMonad = struct
    type 'output t =
      T.stream -> (('output * T.stream), string) result
    let pure x = fun stream -> PResult.ok (x, stream)
    let bind prsr k = let open PResult in 
                      fun input ->
                      let* (result1, remainder1) = prsr input in
                      (k result1) remainder1
  end
  include Monad2App (ParserMonad)

  let alternative prsr1 prsr2 input =
    let is_ok = function
      | Ok _ -> true
      | _ -> false
    in
    if is_ok (prsr1 input)
    then prsr1 input
    else prsr2 input
        
  let alternative prsr1 prsr2 input =
    match prsr1 input with
    | Error _ -> prsr2 input
    | _ -> prsr1 input
         
  let (<|>) = alternative

  let run_parser prsr input = prsr input
            
  module KleisliArrows = struct

    let succeed input = PResult.ok input

    let fail _ = PResult.error "error: pfail"

    let choice prsrs = List.foldl (<|>) fail prsrs

    let optional prsr = prsr *> pure () <|> pure ()
                     
    let satisfy pred = function
      | r when r = T.empty -> PResult.error "end of file"
      | toks -> if pred (T.hd toks)
                then PResult.ok (T.hd toks, T.tl toks)
                else PResult.error "error: satisfy"

    let eof = function
      | r when r = T.empty -> PResult.ok ((), T.empty)
      | _ -> PResult.error "error: eof"

    let token tok = satisfy (fun x -> x = tok)

    let rec many prsr input =
      match prsr input with
      | Ok _ -> (pure T.cons <*> prsr <*> many prsr) input
      | Error _ -> (pure T.empty) input

    let many1 prsr = pure T.cons <*> prsr <*> many prsr

    let sep_by1 prsr sepPrsr =
      let+ initial = many (prsr <* sepPrsr)
      and+ final = prsr
      in let open T in
         initial ++ (cons final empty)
                     
  end
  include KleisliArrows
end


module StringParserF = struct
  module PResult = ResultF (String)

  module ParserMonad = struct
    type 'output t =
      string -> (('output * string), string) result
    let pure x = fun stream -> PResult.ok (x, stream)
    let bind prsr k = let open PResult in 
                      fun input ->
                      let* (result1, remainder1) = prsr input in
                      (k result1) remainder1
  end
  include Monad2App (ParserMonad)

  let alternative prsr1 prsr2 input =
    match prsr1 input with
    | Error _ -> prsr2 input
    | _ -> prsr1 input
  let (<|>) = alternative
            
  module KleisliArrows = struct

    let succeed input = PResult.ok input

    let fail _ = PResult.error "error: pfail"

    let choice prsrs = List.foldl (<|>) fail prsrs

    let optional prsr = prsr *> pure () <|> pure ()
                     
    let satisfy pred = let open String in function
      | "" -> PResult.error "end of file"
      | str ->
         let head = str.[0] in
         let tail = sub str 1 (length str - 1) in
         if pred head
         then PResult.ok (head, tail)
         else PResult.error "error: satisfy"

    let munch1 pred input =
      let open String in
      let rec span pred = function
        | "" -> ("", "")
        | str ->
           let head = sub str 0 1 in
           let recurse = sub str 1 (length str - 1) |> span pred in
           if pred str.[0]
           then head ^ fst recurse, snd recurse
           else "", str
      in
      match span pred input with
      | ("",_) -> PResult.error "error: span"
      | _ -> PResult.ok (span pred input)

    let eof = function
      | "" -> PResult.ok ((), "")
      | _ -> PResult.error "error: eof"
    
    let char c = satisfy (fun x -> x = c)

    let string str = 
      let concat_char strP chr =
        let+ str = strP
        and+ chr = char chr in
        str ^ String.make 1 chr
      in
      String.foldl concat_char (pure "") str
               
    let parse_string prsr str =
      match prsr str with
      | Ok (output, []) -> Ok output
      | Error _ as e -> e
      | _ -> Error "partial parse"
                
    let rec many prsr input =
      match prsr input with
      | Ok _ -> (pure cons <*> prsr <*> many prsr) input
      | Error _ -> (pure []) input

    let many1 prsr = pure cons <*> prsr <*> many prsr

    let sep_by1 prsr sepPrsr =
      let+ initial = many (prsr <* sepPrsr)
      and+ final = prsr
      in initial @ [final]

    let skip_spaces1 =
      let is_space chr =
        String.mem chr "\r\n\t "
      in
      pure () <* munch1 is_space

    let skip_spaces = skip_spaces1 <|> pure ()

  end
  include KleisliArrows
end

                     
module Example = struct

  module Lex = struct         
    type lexeme =
      | LParen
      | RParen
      | Plus
      | Minus
      | Times
      | Div
      | Num of int
      | Space

    let char_to_binop = function
      | '+' -> Plus
      | '-' -> Minus
      | '*' -> Times
      | '/' -> Div
      | _ -> assert false

    let is_plus = function Plus -> true | _ -> false 
    let is_minus = function Minus -> true | _ -> false 
    let is_times = function Times -> true | _ -> false 
    let is_div = function Div -> true | _ -> false 
    let is_lparen = function LParen -> true | _ -> false
    let is_rparen = function RParen -> true | _ -> false
    let is_space = function Space -> true | _ -> false
    let is_num = function Num _ -> true | _ -> false

    module Lexer = StringParserF 

    let lexP =
      let open Lexer in
      let lparenP = pure LParen <* satisfy (eq '(') in
      let rparenP = pure RParen <* satisfy (eq ')') in
      let opP = 
        let is_op_chr chr = String.mem chr "+*/-" in
        let+ op_chr = satisfy is_op_chr
        in char_to_binop op_chr
      in
      let numP =
        let mk_num str = Num (int_of_string str) in
        let+ numstring = munch1 (Char.Decimal.is)
        in mk_num numstring
      in
      let spaceP = pure Space <* skip_spaces1 in
      choice [ lparenP; rparenP; opP; numP; spaceP ]
                                     
    let lex str =
      match Lexer.many1 lexP str with
      | Ok (lst, "") -> Ok lst
      | Ok (_, _) -> Error "lexing error"
      | Error e -> Error e
  end

  module Parse = struct

    module ListTok =
      struct
        include List
        type tok = Lex.lexeme
        type stream = tok List.t
        let (++) = (@)
        let empty = []
      end
    
    type binop =
      | Plus of (exp * exp)
      | Minus of (exp * exp)
      | Times of (exp * exp)
      | Div of (exp * exp)

    and num = Num of int

    and exp =
      | Num_exp of num
      | Op_exp of binop

    let mk_plus exp1 exp2 = Plus (exp1, exp2) 
    let mk_minus exp1 exp2 = Minus (exp1, exp2)
    let mk_times exp1 exp2 = Times (exp1, exp2)
    let mk_div exp1 exp2 = Div (exp1, exp2)
    let mk_num n = Num n
    let mk_numexp n = Num_exp n
    let mk_opexp o = Op_exp o
                           
    module Parser = ParserF (ListTok)

    let skip_spaces =
      let open Parser in
      optional (satisfy Lex.is_space)
                  
    let numP =
      let open Parser in
      let make_num = function
        | Lex.Num n -> Num n
        | _ -> assert false
      in
      let+ lexeme = satisfy Lex.is_num
      in make_num lexeme
       
    let rec binopP input =
      let open Parser in
      let open Lex in
      let op prsr pred =
          pure prsr
          <* (satisfy is_lparen)
          <* skip_spaces
          <*> expP
          <* skip_spaces
          <* (satisfy pred)
          <* skip_spaces
          <*> expP
          <* skip_spaces
          <* (satisfy is_rparen)
      in
      choice [
          op mk_plus is_plus ;
          op mk_minus is_minus ;
          op mk_times is_times ;
          op mk_div is_div ;
        ] @@ input
      
    and expP input =
      let open Parser in
      choice [
          pure mk_numexp <*> numP ;
          pure mk_opexp <*> binopP ;
        ] @@ input
  end
end




                

(*
 * Copyright (c) 2021 Matt Teichman
 * 
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
