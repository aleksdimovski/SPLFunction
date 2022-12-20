(***************************************************)
(*                                                 *)
(*                   Tuplerf.ml                    *)
(*                                                 *)
(*             Aleksandar Dimovski                 *)
(*          Mother Teresa Uni, Skopje              *)
(*                   2021		                   *)
(*                                                 *)
(***************************************************)

open AbstractSyntax
open Apron
open DecisionTree
open Constraints
open Numerical 
open Partition
open Domain
open Functions

(** Signature for a tuple of partitions. *)
module type TUPLERF = sig
  module R : RANKING_FUNCTION

  type t
  val elems : t -> R.t list
  val env : t -> Environment.t
  val vars : t -> var list  
  val configs : t -> (string * int) list list

  val bot :  Environment.t -> var list -> (string * int) list list -> t
  val inner : Environment.t -> var list -> (string * int) list list -> R.t list -> t
  val top : Environment.t -> var list -> (string * int) list list -> t
  val zero : Environment.t -> var list -> (string * int) list list -> t  

  val isLeq : kind -> t -> t -> bool
  val join : kind -> t -> t -> t
  val meet : kind -> t -> t -> t
  val widen : ?jokers:int -> t -> t -> t  
  val reset : t -> bExp -> t
  val resetmask : t -> t -> bExp -> t
  val dual_widen : t -> t -> t  

  val bwdAssign : ?underapprox:bool -> t -> aExp * aExp -> t
  val filter : ?underapprox:bool -> t -> bExp -> t
  val config_filter : Environment.t -> t -> bExp -> t  
  val config_filter_not : Environment.t -> t -> t -> t   
  val defined : t -> bool list
  
  val label : string -> property -> t -> t

  val print : Format.formatter -> t -> unit
  val print_assert : Format.formatter -> t -> unit

end
