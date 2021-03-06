(***************************************************)
(*                                                 *)
(*             MakeDTDomain.ml                    *)
(*                                                 *)
(*             Aleksandar Dimovski                 *)
(*          Mother Teresa Uni, Skopje              *)
(*                   2018 - 2019                   *)
(*                                                 *)
(***************************************************)

open AbstractSyntax
open Apron
open DTDomainRF
open ItoA
open Partition
open Constraints
open DecisionTree
open Domain
open Functions
open Ordinals



let tracebwd = ref false
let retrybwd = ref 5
let fmt = ref Format.std_formatter

(** The ranking functions abstract domain is an abstract domain functor T. 
    It is parameterized by an auxiliary abstract domain for linear constraints 
    C, and an auxiliary abstract domains for functions F, both parameterized by 
    an auxiliary numerical abstract domain B. *)
module MakeDTDomainRF (R: RANKING_FUNCTION)(P: PARTITION) : DTDomainRF =
struct

  module R = R 
  module P = P	(* auxiliary numerical abstract domain *)
  module C = P.C	(* auxiliary linear constraints abstract domain *)
  module N = P.N  (* numerical domain *)

  module CMap = Map.Make(
    struct
      type t = C.t
      let compare = C.compare
    end)

  module L =
  struct
    type t = C.t * C.t
    let compare (c1,nc1) (c2,nc2) =
      if (C.isLeq nc1 c1) then
        if (C.isLeq nc2 c2) then 
          C.compare c1 c2 
        else C.compare c1 nc2
      else if (C.isLeq nc2 c2) 
      then C.compare nc1 c2 
      else C.compare nc1 nc2
  end

  module LSet = Set.Make(L)

  (** The abstract domain manipulates numerical properties of program families. 
      These are represented by decision trees, where the decision nodes are 
      labeled by Boolean features and linear constraints over the numerical features, and the leaf 
      nodes are labeled by linear constraints over the program variables. The decision 
      nodes recursively partition the space of possible values of the features and the linear constraints at the leaves provide 
	  the corresponding invariants. *)
  type tree = Leaf of R.t | Node of L.t * tree * tree

  (** An element of the decision tree abstract domain. *)
  type t = {
    domain : P.t option;	(* current reachable configurations - feature states *)
    tree : tree;			(* current tree *)
    env_vars : Environment.t;	(* current APRON environment for variables*)
	env_feats : Environment.t;	(* current APRON environment for features *)
    vars : var list;			(* current list of program variables *)
    feats : var list		(* current list of feature variables *)	
  }


  (** The current decision tree. *)
  let tree t = t.tree

  (** Prints the current decision tree. *)
  let print_tree vars feats fmt t =
    let rec aux ind fmt t =
      match t with
      | Leaf p ->  Format.fprintf fmt "\n%sLEAF %a" ind R.print p
      | Node ((c,_),l,r) -> Format.fprintf fmt "\n%sNODE %a%a%a" ind 
                              (C.print feats) c (aux (ind ^ "  ")) l (aux (ind ^ "  ")) r
    in aux "" fmt t

  (**
     Prints a tree in graphviz 'dot' format for visualization. 
     http://www.graphviz.org/content/dot-language
  *)
  let print_graphviz_dot fmt t = 
    let vars = t.vars in
	let feats = t.feats in
    let nodeId = ref 0 in
    let nextNodeId () =
      let id = !nodeId in
      nodeId := id + 1;
      Printf.sprintf "node%d" id
    in
    let rec aux id fmt t =
      match t with
      | Leaf p -> Format.fprintf fmt "%s[shape=box,label=\"%a\"]" id R.print p
      | Node ((c,_),l,r) -> 
        let leftId = nextNodeId () in
        let hiddenId = nextNodeId () in
        let rightId = nextNodeId () in
        Format.fprintf fmt "%s[shape=box,style=rounded,label=\"%a\"] ; %s [label=\"\",width=.1,style=invis] ; %s -- %s ; %s -- %s [style=invis] ; %s -- %s [style=dashed] {rank=same %s -- %s -- %s [style=invis]} ; %a; %a" 
            id
            (C.print vars) c
            hiddenId 
            id leftId 
            id hiddenId 
            id rightId 
            leftId hiddenId rightId 
            (aux leftId) l
            (aux rightId) r
    in Format.fprintf fmt "graph G { %a }" (aux (nextNodeId ())) t.tree


  (** Collects the linear constraints labeling the current decision tree. *)
  let tree_labels t =
    let ls = ref LSet.empty in
    let rec aux t =
      match t with
      | Leaf _ -> ()
      | Node (c,l,r) -> aux l; aux r; ls := LSet.add c !ls
    in aux t; !ls


  (* map function for decision tree*)
  let tree_map p_leaf t: t = 
    let domain = t.domain in 
    let env_vars = t.env_vars in
    let env_feats = t.env_feats in	
    let vars = t.vars in 
	let feats = t.feats in 
    let rec aux (tree:tree) : tree = match tree with
      | Leaf p -> p_leaf p
      | Node (c,l,r) -> Node (c, aux l, aux r)
    in { domain = domain; tree = aux t.tree; 
		env_vars = env_vars;
	  	env_feats = env_feats;
      	vars = vars;
	  	feats = feats }

  (** Sorts (and normalizes the constraints within) a decision tree `t`. 

      Let x_1,...,x_k be program variables. We consider all linear 
      constraints in a decision tree to have the following normal form:
      m_1*x_1 + ... + m_k*x_k + q >= 0
      where m_1,...,m_k,q are integer coefficients. Moreover, in order to 
      ensure a canonical representation of the linear constraints, we require
      gcd(|m_1|,...,|m_k|,|q|) = 1
      We then impose a total order on the linear constraints. In particular, 
      we define such order to be the lexicographic order on the coefficients 
      m_1,...,m_k and constant q of the linear constraints. *)
  let rec sort_tree t =
    let rec swap_tree t =
      match t with
      | Node((c,nc),l,r) ->
        let sl = swap_tree l in
        let sr = swap_tree r in
        if (C.isLeq nc c)
        then (* t is normalized *)
          (match sl, sr with
           | Node((c1,nc1),l1,r1), Node((c2,nc2),l2,r2) 
             when (C.isEq c1 c2) (* c1 = c2 *) ->
             if (C.isLeq c c1)
             then (* c <= c1 = c2 *)
               if (C.isEq c c1)
               then (* c = c1 = c2 *) Node((c,nc),l1,r2)
               else (* c < c1 = c2 *) Node((c,nc),sl,sr)
             else (* c > c1 = c2 *)
             if (C.similar c c1) 
             then Node((c1,nc1),l1,Node((c,nc),r1,r2)) 
             else
               let rt = (c,nc) in 
               Node((c1,nc1),Node(rt,l1,l2),Node(rt,r1,r2))
           | Node((c1,nc1),l1,r1), Node((c2,nc2),l2,r2) 
             when (C.isLeq c1 c2) (* c1 < c2 *) ->
             if (C.isLeq c c1)
             then (* c <= c1 < c2 *)
               if (C.isEq c c1)
               then (* c = c1 < c2 *) Node((c,nc),l1,sr)
               else (* c < c1 < c2 *) Node((c,nc),sl,sr)
             else (* c > c1 < c2 *)
             if (C.isLeq c c2)
             then (* c1 < c <= c2 *)
               if (C.isEq c c2)
               then (* c1 < c = c2 *)
                 if (C.similar c c1) 
                 then Node((c1,nc1),l1,Node((c,nc),r1,r2)) 
                 else
                   let rt = (c,nc) in
                   let rt1 = (c1,nc1) in
                   Node(rt1,Node(rt,l1,r2),Node(rt,r1,r2))
               else (* c1 < c < c2 *)
               if (C.similar c2 c) && (C.similar c c1) 
               then Node((c1,nc1),l1,Node((c,nc),r1,sr)) 
               else
                 let rt = (c,nc) in
                 let rt1 = (c1,nc1) in
                 Node(rt1,Node(rt,l1,sr),Node(rt,r1,sr))
             else (* c1 < c2 < c *)
             if (C.similar c c2) && (C.similar c2 c1) 
             then Node((c1,nc1),l1,Node((c,nc),r1,r2))
             else
               let rt = (c,nc) in
               let rt2 = (c2,nc2) in 
               Node((c1,nc1),
                    Node(rt2,Node(rt,l1,l2),Node(rt,l1,r2)),
                    Node(rt2,Node(rt,r1,l2),Node(rt,r1,r2))
                   )
           | Node((c1,nc1),l1,r1), Node((c2,nc2),l2,r2) 
             when (C.isLeq c2 c1) (* c1 > c2 *) ->
             if (C.isLeq c c2)
             then (* c <= c2 < c1 *)
               if (C.isEq c c2)
               then (* c = c2 < c1 *) Node((c,nc),sl,r2)
               else (* c < c2 < c1 *) Node((c,nc),sl,sr)
             else (* c > c2 < c1 *)
             if (C.isLeq c c1)
             then (* c2 < c <= c1 *)
               if (C.isEq c c1)
               then (* c2 < c = c1 *)
                 if (C.similar c c2) 
                 then Node((c,nc),l1,r2) 
                 else
                   let rt = (c,nc) in
                   let rt2 = (c2,nc2) in
                   Node(rt2,Node(rt,l1,l2),Node(rt,l1,r2))
               else (* c2 < c < c1 *)
               if (C.similar c1 c) && (C.similar c c2) 
               then Node((c,nc),l1,r2) 
               else
                 let rt = (c,nc) in 
                 let rt2 = (c2,nc2) in 
                 Node(rt2,Node(rt,sl,l2),Node(rt,sl,r2))
             else (* c2 < c1 < c *)
             if (C.similar c c1) && (C.similar c1 c2) 
             then Node((c1,nc1),l1,Node((c,nc),r1,r2))
             else
               let rt = (c,nc) in
               let rt1 = (c1,nc1) in  
               Node((c2,nc2),
                    Node(rt1,Node(rt,l1,l2),Node(rt,r1,l2)),
                    Node(rt1,Node(rt,l1,r2),Node(rt,r1,r2))
                   )
           | Node((c1,nc1),l1,r1), _ ->
             if (C.isLeq c c1)
             then (* c <= c1 *)
               if (C.isEq c c1)
               then (* c = c1 *) Node((c,nc),l1,sr)
               else (* c < c1 *) Node((c,nc),sl,sr)
             else (* c > c1 *)
             if (C.similar c c1) 
             then Node((c1,nc1),l1,Node((c,nc),r1,sr)) 
             else
               let rt = (c,nc) in 
               Node((c1,nc1),Node(rt,l1,sr),Node(rt,r1,sr))
           | _, Node((c2,nc2),l2,r2) ->
             if (C.isLeq c c2)
             then (* c <= c2 *)
               if (C.isEq c c2)
               then (* c = c2 *) Node((c,nc),sl,r2)
               else (* c < c2 *) Node((c,nc),sl,sr)
             else (* c > c2 *)
             if (C.similar c c2) 
             then Node((c,nc),sl,r2) 
             else
               let rt = (c,nc) in 
               Node((c2,nc2),Node(rt,sl,l2),Node(rt,sl,r2))
           | _ -> Node((c,nc),sl,sr) (* same *))
        else (* t is not normalized *)
          (match sl,sr with
           | Node((c1,nc1),l1,r1), Node((c2,nc2),l2,r2) 
             when (C.isEq c1 c2) (* c1 = c2 *) ->
             if (C.isLeq nc c1)
             then (* nc <= c1 = c2 *)
               if (C.isEq nc c1)
               then (* nc = c1 = c2 *) Node((nc,c),l2,r1)
               else (* nc < c1 = c2 *) Node((nc,c),sr,sl)
             else (* nc > c1 = c2 *)
             if (C.similar nc c1) 
             then Node((c1,nc1),l2,Node((nc,c),r2,r1)) 
             else
               let rt = (nc,c) in
               let rt1 = (c1,nc1) in
               Node(rt1,Node(rt,l2,l1),Node(rt,r2,r1))
           | Node((c1,nc1),l1,r1), Node((c2,nc2),l2,r2) 
             when (C.isLeq c1 c2) (* c1 < c2 *) ->
             if (C.isLeq nc c1)
             then (* nc <= c1 < c2 *)
               if (C.isEq nc c1)
               then (* nc = c1 < c2 *) Node((nc,c),sr,r1)
               else (* nc < c1 < c2 *) Node((nc,c),sr,sl)
             else (* nc > c1 < c2 *)
             if (C.isLeq nc c2)
             then (* c1 < nc <= c2 *)
               if (C.isEq nc c2)
               then (* c1 < nc = c2 *)
                 if (C.similar nc c1) 
                 then Node((nc,c),l2,r1) 
                 else
                   let rt = (nc,c) in 
                   let rt1 = (c1,nc1) in	
                   Node(rt1,Node(rt,l2,l1),Node(rt,l2,r1))
               else (* c1 < nc < c2 *)
               if (C.similar c2 nc) && (C.similar nc c1) 
               then Node((nc,c),l2,r1) 
               else
                 let rt = (nc,c) in 
                 let rt1 = (c1,nc1) in
                 Node(rt1,Node(rt,sr,l1),Node(rt,sr,r1))
             else (* c1 < c2 < nc *)
             if (C.similar nc c2) && (C.similar c2 c1) 
             then Node((c2,nc2),l2,Node((nc,c),r2,r1))
             else
               let rt = (nc,c) in
               let rt2 = (c2,nc2) in 
               Node((c1,nc1),
                    Node(rt2,Node(rt,l2,l1),Node(rt,r2,l1)),
                    Node(rt2,Node(rt,l2,r1),Node(rt,r2,r1))
                   )
           | Node((c1,nc1),l1,r1), Node((c2,nc2),l2,r2) 
             when (C.isLeq c2 c1) (* c1 > c2 *) ->
             if (C.isLeq nc c2)
             then (* nc <= c2 < c1 *)
               if (C.isEq nc c2)
               then (* nc = c2 < c1 *) Node((nc,c),l2,sl)
               else (* nc < c2 < c1 *) Node((nc,c),sr,sl)
             else (* nc > c2 < c1 *)
             if (C.isLeq nc c1)
             then (* c2 < nc <= c1 *)
               if (C.isEq nc c1)
               then (* c2 < nc = c1 *)
                 if (C.similar nc c2) 
                 then Node((c2,nc2),l2,Node((nc,c),r2,r1)) 
                 else
                   let rt = (nc,c) in
                   let rt2 = (c2,nc2) in
                   Node(rt2,Node(rt,l2,r1),Node(rt,r2,r1))
               else (* c2 < nc < c1 *)
               if (C.similar c1 nc) && (C.similar nc c2) 
               then Node((c2,nc2),l2,Node((nc,c),r2,sl)) 
               else
                 let rt = (nc,c) in
                 let rt2 = (c2,nc2) in 
                 Node(rt2,Node(rt,l2,sl),Node(rt,r2,sl))
             else (* c2 < c1 < nc *)
             if (C.similar nc c1) && (C.similar c1 c2) 
             then Node((c2,nc2),l2,Node((nc,c),r2,r1))
             else
               let rt = (nc,c) in
               let rt1 = (c1,nc1) in 
               Node((c2,nc2),
                    Node(rt1,Node(rt,l2,l1),Node(rt,l2,r1)),
                    Node(rt1,Node(rt,r2,l1),Node(rt,r2,r1))
                   )
           | Node((c1,nc1),l1,r1), _ ->
             if (C.isLeq nc c1)
             then (* nc <= c1 *)
               if (C.isEq nc c1)
               then (* nc = c1 *) Node((nc,c),sr,r1)
               else (* nc < c1 *) Node((nc,c),sr,sl)
             else (* nc > c1 *)
             if (C.similar nc c1) then Node((nc,c),sr,r1) 
             else
               let rt = (nc,c) in 		
               Node((c1,nc1),Node(rt,sr,l1),Node(rt,sr,r1))
           | _, Node((c2,nc2),l2,r2) ->
             if (C.isLeq nc c2)
             then (* nc <= c2 *)
               if (C.isEq nc c2)
               then (* nc = c2 *) Node((nc,c),l2,sl)
               else (* nc < c2 *) Node((nc,c),sr,sl)
             else (* nc > c2 *)
             if (C.similar nc c2)
             then Node((c2,nc2),l2,Node((nc,c),r2,sl)) 
             else
               let rt = (nc,c) in 	
               Node((c2,nc2),Node(rt,l2,sl),Node(rt,r2,sl))
           | _ -> Node((nc,c),sr,sl) (* it stays the same *))
      | _ -> t
    in
    let st = swap_tree t in (* root(st) is the smallest constraint in t *)
    match st with
    | Node(c,l,r) ->
      let sl = sort_tree l in
      let sr = sort_tree r in
      Node(c,sl,sr)
    | _ -> st


  let sorting_tree t: t = 
    let domain = t.domain in 
	let tree = t.tree in 
    let env_vars = t.env_vars in
    let env_feats = t.env_feats in	
    let vars = t.vars in 
	let feats = t.feats in 
    { domain = domain; 
		tree = sort_tree tree;
		env_vars = env_vars;
	  	env_feats = env_feats;
      	vars = vars;
	  	feats = feats }

  (** The bottom element of the abstract domain, i.e., a decision tree with a single `bottom` leaf. *)
  let bot ?domain ev ef vs fs = 
    { domain = domain; tree = Leaf (R.bot ev vs); env_vars = ev; env_feats = ef; vars = vs; feats =fs }


  (** The top element of the abstract domain. The totally unknown function, i.e., a decision tree with a single `top` leaf. *)
  let top ?domain ev ef vs fs = 
    { domain = domain; tree = Leaf (R.top ev vs); env_vars = ev; env_feats = ef; vars = vs; feats =fs }

  let zero ?domain ev ef vs fs = 
    { domain = domain; tree = Leaf (R.zero ev vs); env_vars = ev; env_feats = ef; vars = vs; feats =fs }

  (** BINARY OPERATORS *)

  let tree_unification_aux t1 t2 env_vars env_feats vars feats cs = 
    let rec aux (t1,t2) cs =
      match t1,t2 with
      | Leaf _,Leaf _ -> (t1,t2)
      | Node ((c1,nc1),l1,r1),Node((c2,nc2),l2,r2) when (C.isEq c1 c2) (* c1 = c2 *) ->
        let (ul1,ul2) = aux (l1,l2) (c1::cs) in
        let (ur1,ur2) = aux (r1,r2) (nc1::cs) in
        (Node((c1,nc1),ul1,ur1),Node((c2,nc2),ul2,ur2))
      | Node ((c1,nc1),l1,r1),Node((c2,nc2),l2,r2) when (C.isLeq c1 c2) (* c1 < c2 *) ->
        let bcs = P.inner env_feats feats cs in
        let bc1 = P.inner env_feats feats [c1] in
        if (P.isLeq bcs bc1) then (* c1 is redundant *) 
          aux (l1,t2) cs
        else (* c1 is not redundant *)
          let bnc1 = P.inner env_feats feats [nc1] in
          if (P.isLeq bcs bnc1) then (* nc1 is redundant *) 
            aux (r1,t2) cs
          else (* nc1 is not redundant *)
            let (ul1,ul2) = aux (l1,t2) (c1::cs) in
            let (ur1,ur2) = aux (r1,t2) (nc1::cs) in
            (Node((c1,nc1),ul1,ur1),Node((c1,nc1),ul2,ur2))
      | Node ((c1,nc1),l1,r1),Node((c2,nc2),l2,r2) 
        when (C.isLeq c2 c1) (* c1 > c2 *) ->
        let bcs = P.inner env_feats feats cs in
        let bc2 = P.inner env_feats feats [c2] in
        if (P.isLeq bcs bc2)
        then (* c2 is redundant *) aux (t1,l2) cs
        else (* c2 is not redundant *)
          let bnc2 = P.inner env_feats feats [nc2] in
          if (P.isLeq bcs bnc2)
          then (* nc2 is redundant *) aux (t1,r2) cs
          else (* nc2 is not redundant *)
            let (ul1,ul2) = aux (t1,l2) (c2::cs) in
            let (ur1,ur2) = aux (t1,r2) (nc2::cs) in
            (Node((c2,nc2),ul1,ur1),Node((c2,nc2),ul2,ur2))
      | Node ((c1,nc1),l1,r1),_ ->
        let bcs = P.inner env_feats feats cs in
        let bc1 = P.inner env_feats feats [c1] in
        if (P.isLeq bcs bc1)
        then (* c1 is redundant *) aux (l1,t2) cs
        else (* c1 is not redundant *)
          let bnc1 = P.inner env_feats feats [nc1] in
          if (P.isLeq bcs bnc1)
          then (* nc1 is redundant *) aux (r1,t2) cs
          else (* nc1 is not redundant *)
            let (ul1,ul2) = aux (l1,t2) (c1::cs) in
            let (ur1,ur2) = aux (r1,t2) (nc1::cs) in
            (Node((c1,nc1),ul1,ur1),Node((c1,nc1),ul2,ur2))
      | _,Node((c2,nc2),l2,r2) ->
        let bcs = P.inner env_feats feats cs in
        let bc2 = P.inner env_feats feats [c2] in
        if (P.isLeq bcs bc2)
        then (* c2 is redundant *) aux (t1,l2) cs
        else (* c2 is not redundant *)
          let bnc2 = P.inner env_feats feats [nc2] in
          if (P.isLeq bcs bnc2)
          then (* nc2 is redundant *) aux (t1,r2) cs
          else (* nc2 is not redundant *)
            let (ul1,ul2) = aux (t1,l2) (c2::cs) in
            let (ur1,ur2) = aux (t1,r2) (nc2::cs) in
            (Node((c2,nc2),ul1,ur1),Node((c2,nc2),ul2,ur2))
    in aux (t1,t2) cs

  (** The decision tree orderings and binary operators rely on tree 
      unification to find a common labeling for the decision trees. Given two 
      decision trees t1 and t2 the unification accumulates into a set `cs` 
      the linear constraints encountered along the paths of the decision 
      trees, possibly adding decision nodes or removing constraints that are 
      redundant or whose negation is redundant with respect to `cs`. 

      The implementation assumes that t1 and t2 are sorted and normalized. *)	
  let tree_unification t1 t2 env_vars env_feats vars feats = 
    tree_unification_aux t1 t2 env_vars env_feats vars feats [] 

  (** Given two decision trees t1 and t2, the ordering accumulates 
      into a set `cs` the linear constraints encountered along the paths of 
      the decision tree up to the leaf nodes, which are compared by means of 
      the leaf node ordering. 

      The implementation assumes that t1 and t2 are defined over the same 
      reachable states, the same APRON envorinment and the same list of 
      program variables. *)



let isLeq k t1 t2 =
    let domain = t1.domain in 
    let env_vars = t1.env_vars in
    let env_feats = t1.env_feats in	
    let vars = t1.vars in 
	let feats = t1.feats in 
    let rec aux (t1,t2) cs = match t1,t2 with
      | Leaf p1, Leaf p2 ->
        let p = match domain with 
          | None -> P.inner env_feats feats cs 
          | Some domain -> 
            P.inner env_feats feats (cs@(P.constraints domain)) in
        (*Format.fprintf !fmt "\n isLEQ %a\n" P.print p;*)
		if (P.isBot p) then true
        else
          (R.isLeq k p1 p2 (* forall leafs x: P1(x) <= P2(x) *))
      | Node ((c1,nc1),l1,r1), Node((c2,nc2),l2,r2) when (C.isEq c1 c2) ->
        (aux (l1,l2) (c1::cs)) && (aux (r1,r2) (nc1::cs))
      | _ -> raise (Invalid_argument "isLeq:")
    in aux (tree_unification t1.tree t2.tree env_vars env_feats vars feats) []

	(**)

  	let join k t1 t2 =
    let domain = t1.domain in 
    let env_vars = t1.env_vars in
    let env_feats = t1.env_feats in	
    let vars = t1.vars in 
	let feats = t1.feats in 
    let rec aux (t1,t2) cs = match t1,t2 with
      | Leaf p1, Leaf p2 ->
        let p = match domain with 
          | None -> P.inner env_feats feats cs 
          | Some domain -> P.inner env_feats feats (cs@(P.constraints domain)) in
            (*P.meet (P.inner env_feats feats cs) domain in
		Format.fprintf !fmt "\n isJOIN old %a\n" P.print p;*)
        if (P.isBot p) then Leaf (R.bot env_vars vars)
        else
          Leaf (R.join k p1 p2 (* forall leafs x: join (P1(x), P2(x)) *))
      | Node ((c1,nc1),l1,r1), Node((c2,nc2),l2,r2) when (C.isEq c1 c2) ->
        let l = aux (l1,l2) (c1::cs) in 
		let r = aux (r1,r2) (nc1::cs) in 
		Node ((c1,nc1),l,r)
      | _ -> raise (Invalid_argument "isJoin:")
    in 
	let (t1,t2) = tree_unification t1.tree t2.tree env_vars env_feats vars feats in 
	(*let rec aux2 t cs =
      match t with
      | Leaf p ->
        let b = P.inner env_feats feats cs in
        if P.isBot b then () else Format.fprintf !fmt "[%a HERE? %a] " P.print b P.print p
      | Node((c,nc),l,r) -> aux2 r (nc::cs); aux2 l (c::cs)
    in aux2 t1 []; aux2 t2 []; *)
	(*Format.fprintf !fmt "\n%a JOIN %a" DTDomain.print !fmt t1; DTDomain.print !fmt t2; *)
	let t = aux (t1,t2) [] in 
	{ domain = domain;
      tree = t;
      env_vars = env_vars;
	  env_feats = env_feats;
      vars = vars;
	  feats = feats }	
	

  (**)
	
  	let meet k t1 t2 =
    let domain = t1.domain in 
    let env_vars = t1.env_vars in
    let env_feats = t1.env_feats in	
    let vars = t1.vars in 
	let feats = t1.feats in 
    let rec aux (t1,t2) cs = match t1,t2 with
      | Leaf p1, Leaf p2 ->
        let p = match domain with 
          | None -> P.inner env_feats feats cs 
          | Some domain -> 
            P.inner env_feats feats (cs@(P.constraints domain)) in
        if (P.isBot p) then Leaf (R.bot env_vars vars)
        else
          Leaf (R.meet k p1 p2 (* forall leafs x: join (P1(x), P2(x)) *))
      | Node ((c1,nc1),l1,r1), Node((c2,nc2),l2,r2) when (C.isEq c1 c2) ->
        let l = aux (l1,l2) (c1::cs) in 
		let r = aux (r1,r2) (nc1::cs) in 
		Node ((c1,nc1),l,r)
      | _ -> raise (Invalid_argument "isMeet:")
    in let t = aux (tree_unification t1.tree t2.tree env_vars env_feats vars feats) [] in 
	{ domain = domain;
      tree = t;
      env_vars = env_vars;
	  env_feats = env_feats;
      vars = vars;
	  feats = feats }	

  (**)
	
  	let widen ?(jokers=0) t1 t2 =
    let domain = t1.domain in 
    let env_vars = t1.env_vars in
    let env_feats = t1.env_feats in	
    let vars = t1.vars in 
	let feats = t1.feats in 
    let rec aux (t1,t2) cs = match t1,t2 with
      | Leaf p1, Leaf p2 ->
        let p = match domain with 
          | None -> P.inner env_feats feats cs 
          | Some domain -> 
            P.inner env_feats feats (cs@(P.constraints domain)) in
        if (P.isBot p) then Leaf (R.bot env_vars vars)
        else
          Leaf (R.widen ~jokers:jokers p1 p2 (* forall leafs x: join (P1(x), P2(x)) *))
      | Node ((c1,nc1),l1,r1), Node((c2,nc2),l2,r2) when (C.isEq c1 c2) ->
        let l = aux (l1,l2) (c1::cs) in 
		let r = aux (r1,r2) (nc1::cs) in 
		Node ((c1,nc1),l,r)
      | _ -> raise (Invalid_argument "isWiden:")
    in let t = aux (tree_unification t1.tree t2.tree env_vars env_feats vars feats) [] in 
	{ domain = domain;
      tree = t;
      env_vars = env_vars;
	  env_feats = env_feats;
      vars = vars;
	  feats = feats }	  	

  (**)

  let bwdAssign ?(underapprox=false) t e = 
    let domain = t.domain in 
    let env_vars = t.env_vars in
    let env_feats = t.env_feats in	
    let vars = t.vars in 
	let feats = t.feats in 
    let rec aux t cs = match t with
      | Leaf p ->
        let p' = match domain with 
          | None -> P.inner env_feats feats cs 
          | Some domain -> 
            P.inner env_feats feats (cs@(P.constraints domain)) in
        if (P.isBot p') then Leaf (R.bot env_vars vars)
        else
          Leaf (R.bwdAssign ~underapprox:underapprox p e)
      | Node ((c1,nc1),l1,r1) ->
        let l = aux l1 (c1::cs) in
		let r = aux r1 (nc1::cs) in
		Node ((c1,nc1),l,r)
      | _ -> raise (Invalid_argument "bwdAssign:")
    in { domain = domain;
      tree = aux t.tree [];
      env_vars = env_vars;
	  env_feats = env_feats;
      vars = vars;
	  feats = feats }

  (**)
    
  let rec filter ?(underapprox=false) t e =
    let domain = t.domain in 
    let env_vars = t.env_vars in
    let env_feats = t.env_feats in	
    let vars = t.vars in 
	let feats = t.feats in 
    let rec aux t cs = match t with
      | Leaf p ->
        let p' = match domain with 
          | None -> P.inner env_feats feats cs 
          | Some domain -> 
            P.inner env_feats feats (cs@(P.constraints domain)) in
        if (P.isBot p') then Leaf (R.bot env_vars vars)
        else
          Leaf (R.filter ~underapprox:underapprox p e (* forall leafs x: widen ( P1(x), P2(x)) *))
      | Node ((c1,nc1),l1,r1) ->
        let l = aux l1 (c1::cs) in
		let r = aux r1 (nc1::cs) in
		Node ((c1,nc1),l,r)
      | _ -> raise (Invalid_argument "fwdAssign:")
    in { domain = domain;
      tree = aux t.tree [];
      env_vars = env_vars;
	  env_feats = env_feats;
      vars = vars;
	  feats = feats }


  let rec config_filter t e =
    let domain = t.domain in 
    let env_vars = t.env_vars in
    let env_feats = t.env_feats in	
    let vars = t.vars in 
	let feats = t.feats in 
	let rec aux t bs (*constraints in expression e*) cs (*constraints collected up to the node*) =
	  let bcs = P.inner env_feats feats cs in 
      match bs with
      | [] ->
        (match t with
         | Leaf p -> Leaf p
         | Node((c,nc),l,r) ->
           let bc = P.inner env_feats feats [c] in  (* Environmet features *)
           if (P.isLeq bcs bc)
           then (* c is redundant *) aux l bs cs
           else (* c is not redundant *)
             (* if (B.isBot (B.meet bc bcs))
                then (* c is conflicting *) aux r bs cs
                else *)
             let l = aux l bs (c::cs) in
             let r = aux r bs (nc::cs) in
			 Node((c,nc),l,r))
      | (x,nx)::xs ->
	    (*C.print feats !fmt x; Format.fprintf !fmt "\n config-filter %b\n" (Lincons1.get_typ x = Lincons1.EQ); *)
        let bx = P.inner env_feats feats [x] in
        if (P.isLeq bcs bx)
        then (* x is redundant *) aux t xs cs
        else (* x is not redundant *)
        if (P.isBot (P.meet bx bcs))
        then (* x is conflicting *) Leaf (R.bot env_vars vars) (* This introduces a NIL leaf to the tree *)
        else
        if (C.isLeq nx x) || (Lincons1.get_typ x = Lincons1.EQ)
        then (* x is normalized *)
          (match t with
           | Node ((c,nc),l,r) when (C.isEq c x) (* c = x *) ->
             let l = aux l xs (c::cs) in
			 Node((c,nc),l,Leaf (R.bot env_vars vars))
           | Node ((c,nc),l,r) when (C.isLeq c x) (* c < x *) ->
             let bc = P.inner env_vars vars [c] in
             if (P.isLeq bcs bc)
             then (* c is redundant *) aux l bs cs
             else (* c is not redundant *)
               (* if (B.isBot (B.meet bc bcs))
                  then (* c is conflicting *) aux r bs cs
                  else *)
               let l = aux l bs (c::cs) in
               let r = aux r bs (nc::cs) in
			   Node((c,nc),l,r)
           | _ ->
             let l = aux t xs (x::cs) in
				Node((x,nx),l,Leaf (R.bot env_vars vars)) )
        else (* x is not normalized *)
          (match t with
           | Node ((c,nc),l,r) when (C.isEq c nx) (* c = nx *) ->
             let r = aux r xs (nc::cs) in
			 Node((c,nc),Leaf (R.bot env_vars vars),r)
           | Node ((c,nc),l,r) when (C.isLeq c nx) (* c < nx *) ->
             let bc = P.inner env_vars vars [c] in
             if (P.isLeq bcs bc)
             then (* c is redundant *) aux l bs cs
             else (* c is not redundant *)
               (* if (B.isBot (B.meet bc bcs))
                  then (* c is conflicting *) aux r bs cs
                  else *)
               let l = aux l bs (c::cs) in
               let r = aux r bs (nc::cs) in
			   Node((c,nc),l,r)
           | _ ->
             let r = aux t xs (x::cs) in
			 Node((nx,x),Leaf (R.bot env_vars vars),r) )
    in
    match e with
    | A_TRUE | A_MAYBE -> { domain = domain; tree = t.tree; env_vars = env_vars; env_feats = env_feats; vars = vars; feats = feats }
    | A_FALSE -> { domain = domain; tree = Leaf (R.bot env_vars vars); env_vars = env_vars; env_feats = env_feats; vars = vars; feats = feats }
	| A_bvar v -> 	let cons = Lincons1.make (Linexpr1.make env_feats) Lincons1.EQ in
  					Lincons1.set_array cons [| ((Coeff.s_of_int 1), (Var.of_string v.varId)) |] (Some (Coeff.s_of_int (-1))); 
					let neg_cons = Lincons1.make (Linexpr1.make env_feats) Lincons1.EQ in
  					Lincons1.set_array neg_cons [| ((Coeff.s_of_int 1), (Var.of_string v.varId)) |] (Some (Coeff.s_of_int 0)); 
					(*C.print feats !fmt cons; C.print feats !fmt neg_cons; Format.fprintf !fmt "\n config-filter b-var %d\n" cst;*)
					let bs = if (!AbstractSyntax.cst=1) then [(cons,neg_cons)] else [(neg_cons,cons)] in AbstractSyntax.cst:=1; 
					{ domain = domain; tree = aux t.tree bs []; env_vars = env_vars; env_feats = env_feats; vars = vars; feats = feats }
    | A_bunary (o,e) ->
      (match o with
       | A_NOT -> let (e, _) = negBExp e in config_filter t e )
    | A_bbinary (o,(e1,_),(e2,_)) ->
      let t1 = config_filter t e1 and t2 = config_filter t e2 in
      (match o with
       | A_AND -> meet APPROXIMATION t1 t2
       | A_OR -> join APPROXIMATION t1 t2)
    | A_rbinary (_,_,_) ->
      let bp = match domain with
        | None -> P.inner env_feats feats []
        | Some domain -> P.inner env_feats feats (P.constraints domain)
      in
	  (*let pp = P.filter bp e in Format.fprintf !fmt "\n here filter %a %a" P.print pp bExp_print_aux e;*)
      let bs = List.map (fun c -> let nc = C.negate c in (c,nc)) (P.constraints (P.filter bp e)) in
      let bs = List.sort L.compare bs in
      { domain = domain; tree = aux t.tree bs []; env_vars = env_vars; env_feats = env_feats; vars = vars; feats = feats }
 

  let rec print_tree fmt env_feats feats t =
    let rec aux t cs =
      match t with
      | Leaf p ->
        let b = P.inner env_feats feats cs in
        if P.isBot b then () else Format.fprintf fmt "[%a ? %a] " P.print b R.print p
      | Node((c,nc),l,r) -> aux r (nc::cs); aux l (c::cs)
    in aux t []

  (**)

  let compress t =
    let domain = t.domain in
    let env_vars = t.env_vars in
    let env_feats = t.env_feats in	
    let vars = t.vars in 
	let feats = t.feats in 
    let rec aux t cs =
      match t with
      | Leaf _ -> t
      | Node((c,nc),l,r) ->
        let l = aux l (c::cs) in
        let r = aux r (nc::cs) in
		(*print_string "in compress, before match \n";
		print_string " node: "; C.print feats !fmt c; print_string " left: "; print_tree !fmt env_feats feats l; print_string " right: "; print_tree !fmt env_feats feats r; *)
        match l,r with
        | Leaf f1,Leaf f2 when (R.isBot f1) && (R.isBot f2) -> Leaf f1
        | Leaf f1,Leaf f2 when (R.isLeq APPROXIMATION f1 f2) && (R.isLeq APPROXIMATION f2 f1) -> (*print_string "in compress, eq leaves \n"; P.print !fmt f1; P.print !fmt f2;*) Leaf f1		
        | Leaf f1,Leaf f2 ->
		  (*print_string "in compress, leaves \n"; P.print !fmt f1; P.print !fmt f2; *)
          let b1 = match domain with | None -> P.inner env_feats feats (c::cs) | Some domain -> P.inner env_feats feats ((c::cs)@(P.constraints domain)) in
          if (P.isBot b1) then Leaf f2 else
            let b2 = match domain with | None -> P.inner env_feats feats (nc::cs) | Some domain -> P.inner env_feats feats ((nc::cs)@(P.constraints domain)) in
            if (P.isBot b2) then Leaf f1 else Node((c,nc),l,r)
        (*| Leaf f1,Node((c2,nc2),Leaf f2,r2) when (P.isBot f1) && (P.isBot f2) -> aux (Node((c2,nc2),Leaf f1,r2)) cs
        | Leaf f1,Node((c2,nc2),Leaf f2,r2) when (P.isLeq f1 f2) && (P.isLeq f2 f1) -> aux (Node((c2,nc2),Leaf f1,r2)) cs *)
          (* e.g., NODE( y >= 2, LEAF 3y+2, NODE( y >= 1, LEAF 5, LEAF 1 )) *)
        | Leaf f1,Node((c2,nc2),Leaf f2,r2) when (R.isLeq APPROXIMATION f1 f2) && (R.isLeq APPROXIMATION f2 f1) && (C.isLeq c c2) -> Node((c2,nc2),Leaf f1,r2)
		| Node((c1,nc1),l1,Leaf f1),Leaf f2 when (R.isLeq APPROXIMATION f1 f2) && (R.isLeq APPROXIMATION f2 f1) && (C.isLeq c1 c) -> Node((c1,nc1),l1,Leaf f1)
        | Node((c1,nc1),Leaf f1,Leaf f2),Node((c2,nc2),Leaf f3,Leaf f4) when (C.isEq c1 c2) && (R.isLeq APPROXIMATION f1 f3) && (R.isLeq APPROXIMATION f2 f4) && (R.isLeq APPROXIMATION f3 f1) && (R.isLeq APPROXIMATION f4 f2) -> Node((c1,nc1),Leaf f1,Leaf f2)	
        | _ -> Node((c,nc),l,r)
    in { domain = domain; tree = aux t.tree [];       
	  env_vars = env_vars;
	  env_feats = env_feats;
      vars = vars;
	  feats = feats }


  let rec print fmt t =
    let domain = t.domain in
    let env_vars = t.env_vars in
    let env_feats = t.env_feats in	
    let vars = t.vars in 
	let feats = t.feats in 
    let print_domain fmt domain =
      match domain with
      | None -> ()
      | Some domain -> P.print fmt domain
    in
    let rec aux t cs =
      match t with
      | Leaf p ->
        let b = match domain with | None -> P.inner env_feats feats cs | Some domain -> P.inner env_feats feats (cs@(P.constraints domain)) in
        if P.isBot b then () else Format.fprintf fmt "[%a ?? %a] " P.print b R.print p
      | Node((c,nc),l,r) -> aux r (nc::cs); aux l (c::cs)
    (* in aux t.tree []; Format.fprintf fmt "\nDOMAIN = {%a}%a\n" print_domain domain (print_tree vars) t.tree *)
    (* Format.fprintf fmt "\nDOMAIN = {%a}%a\n" print_domain domain (print_tree vars) t.tree; *)
    in aux t.tree []


  let print_assert fmt t =
    let domain = t.domain in
    let env_vars = t.env_vars in
    let env_feats = t.env_feats in	
    let vars = t.vars in 
	let feats = t.feats in 
    let rec aux t cs =
      match t with
      | Leaf p ->
        let b = match domain with | None -> P.inner env_feats feats cs | Some domain -> P.inner env_feats feats (cs@(P.constraints domain)) in
        if P.isBot b then () else (if (R.isBot p) then Format.fprintf fmt "CORRECT: %a ? ; " P.print b else (Format.fprintf fmt "ERROR: %a ? %a;" P.print b R.print p))
      | Node((c,nc),l,r) -> aux r (nc::cs); aux l (c::cs)
    in 
	Format.fprintf fmt "{ "; 
	aux t.tree [];
	Format.fprintf fmt " }"



  let rec defined t =
    let domain = t.domain in
    let env_vars = t.env_vars in
    let env_feats = t.env_feats in	
    let vars = t.vars in 
	let feats = t.feats in 
	let result = ref true in 
    let rec aux t cs =
      match t with
      | Leaf p ->
        let b = match domain with | None -> P.inner env_feats feats cs | Some domain -> P.inner env_feats feats (cs@(P.constraints domain)) in
        if P.isBot b then () else (if (not (R.defined p)) then result:=false;)
      | Node((c,nc),l,r) -> 
	  	aux r (nc::cs);
		aux l (c::cs)
    (* in aux t.tree []; Format.fprintf fmt "\nDOMAIN = {%a}%a\n" print_domain domain (print_tree vars) t.tree *)
    (* Format.fprintf fmt "\nDOMAIN = {%a}%a\n" print_domain domain (print_tree vars) t.tree; *)
    in aux t.tree []; !result


end

(** Decision Tree-based domain parameterized by the boxes numerical abstract domain. *)
 module DTBRF = MakeDTDomainRF(TSAB)(Numerical.B)
module DTBORF = MakeDTDomainRF(TSOB)(Numerical.B)
(** Decision Tree-based domain parameterized by the octagons abstract domain. *)
module DTORF = MakeDTDomainRF(TSAO)(Numerical.O)
module DTOORF = MakeDTDomainRF(TSOO)(Numerical.O)
(** Decision Tree-based domain parameterized by the polyhedra abstract domain. *)
module DTPRF = MakeDTDomainRF(TSAP)(Numerical.P)
module DTPORF = MakeDTDomainRF(TSOP)(Numerical.P)
