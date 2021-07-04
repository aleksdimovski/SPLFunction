(***************************************************)
(*                                                 *)
(*                 DTDomain.ml                     *)
(*                                                 *)
(*             Aleksandar Dimovski                 *)
(*          Mother Teresa Uni, Skopje              *)
(*                   2021		                   *)
(*                                                 *)
(***************************************************)

open AbstractSyntax
open Apron
open Partition
open ItoA
open DecisionTree
open Domain
open Functions
open Ordinals

module type DTDomainRF =
sig

  module R : RANKING_FUNCTION
  module P : PARTITION

  type t

  val bot : ?domain:P.t -> Environment.t -> Environment.t -> var list -> var list -> t
  val top : ?domain:P.t -> Environment.t -> Environment.t -> var list -> var list -> t
  val zero : ?domain:P.t -> Environment.t -> Environment.t -> var list -> var list -> t
  
  val isLeq : kind -> t -> t -> bool
  val join : kind -> t -> t -> t
  val meet : kind -> t -> t -> t
  val widen : ?jokers:int -> t -> t -> t

  val filter : ?underapprox:bool -> t -> bExp -> t
  val bwdAssign : ?underapprox:bool -> t -> aExp * aExp -> t

  val config_filter : t -> bExp -> t  

  val compress : t -> t
  val sorting_tree : t -> t
  val print : Format.formatter -> t -> unit
  val print_assert : Format.formatter -> t -> unit
  val print_graphviz_dot : Format.formatter -> t -> unit
  val defined: t -> bool

end
