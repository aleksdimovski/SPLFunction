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

module type SketchDomainP =
sig

  module P : PARTITION

  type t

  val bot : ?domain:P.t -> Environment.t -> Environment.t -> var list -> var list -> t
  val top : ?domain:P.t -> Environment.t -> Environment.t -> var list -> var list -> t
  
  val isLeq : t -> t -> bool
  val join : t -> t -> t
  val meet : t -> t -> t
  val widen : t -> t -> t

  val fwdAssign : t -> aExp * aExp -> t
  val filter : t -> bExp -> t
  val bwdAssign : t -> aExp * aExp -> t

  val config_filter : t -> bExp -> t  
  val config_filter_constraint : t -> bExp -> P.t -> t
  val general_filter : t -> bExp -> t

  val compress : t -> t
  val sorting_tree : t -> t
  val print : Format.formatter -> t -> unit
  val print_assert : Format.formatter -> t -> unit
  val print_graphviz_dot : Format.formatter -> t -> unit

end
