(***************************************************)
(*                                                 *)
(*                        Main                     *)
(*                                                 *)
(*             Aleksandar Dimovski                 *)
(*          Mother Teresa Uni, Skopje              *)
(*                   2021		                   *)
(*                                                 *)
(***************************************************)

open AbstractSyntax
open ItoA
open InvMap
open Apron
open Domain
open Functions
open Iterator
open DTDomainP
open Tuplerf
open DTDomainRF

module DTreeTerminationIterator (B: DTDomainP) (D: DTDomainRF) =
struct

  module D = D
  module R = D.R 
  module S = D.P  

  module B = B
  module P = B.P

  let fwdInvMap = ref InvMap.empty

  let addFwdInv l (a:B.t) = fwdInvMap := InvMap.add l a !fwdInvMap

  let fwdMap_print fmt m = InvMap.iter (fun l a -> 
      Format.fprintf fmt "%a: %a\n" label_print l B.print a) m

  let bwdInvMap = ref InvMap.empty

  let addBwdInv l (a:D.t) = bwdInvMap := InvMap.add l a !bwdInvMap

  let bwdMap_print fmt m = InvMap.iter (fun l a -> 
      Format.fprintf fmt "%a: %a\n" label_print l D.print a) m  

  (* Forward Iterator *)

  let rec fwdStm funcs env_vars env_feats vars feats p s =
    match s with
    | A_label _ -> p
    | A_return -> B.bot env_vars env_feats vars feats
    | A_assign ((l,_),(e,_)) -> B.fwdAssign p (l,e)
    | A_assert (b,_) -> B.filter p b
    | A_if ((b,ba),s1,s2) ->
      let p1' = fwdBlk funcs env_vars env_feats vars feats (B.filter p b) s1 in
      let p2 = B.filter p (fst (negBExp (b,ba))) in
      let p2' = fwdBlk funcs env_vars env_feats vars feats p2 s2 in
      B.join p1' p2'
    | A_ifdef ((b,ba),s1,s2) ->
	  let p1 = B.config_filter p b in 
      let p1' = fwdBlk funcs env_vars env_feats vars feats p1 s1 in	  
	  let b_neg = fst (negBExp (b,ba)) in 
      let p2 = B.config_filter p b_neg in	  
      let p2' = fwdBlk funcs env_vars env_feats vars feats p2 s2 in
      B.join p1' p2'		  
    | A_while (l,(b,ba),s) ->
      let rec aux i p2 n =
        let i' = B.join p p2 in
        if !tracefwd && not !minimal then begin
          Format.fprintf !fmt "### %a:%i ###:\n" label_print l n;
          Format.fprintf !fmt "p1: %a\n" B.print p;
          Format.fprintf !fmt "i: %a\n" B.print i;
          Format.fprintf !fmt "p2: %a\n" B.print p2;
          Format.fprintf !fmt "i': %a\n" B.print i'
        end;
        if B.isLeq i' i then i else
          let i'' = if n <= !joinfwd then i' else 
              B.widen i (B.join i i') in
          if !tracefwd && not !minimal then
            Format.fprintf !fmt "i'': %a\n" B.print i'';
          aux i'' (fwdBlk funcs env_vars env_feats vars feats (B.filter i'' b) s) (n+1)
      in
      let i = B.bot env_vars env_feats vars feats in
      let p2 = fwdBlk funcs env_vars env_feats vars feats (B.filter i b) s in
      let p = aux i p2 1 in
      addFwdInv l p; B.filter p (fst (negBExp (b,ba)))
    | A_call (f,ss) ->
      let f = StringMap.find f funcs in
      let p = List.fold_left (fun ap (s,_) -> 
          fwdStm funcs env_vars env_feats vars feats p s) p ss in
      fwdBlk funcs env_vars env_feats vars feats p f.funcBody
    | A_recall (f,ss) -> B.top env_vars env_feats vars feats (* TODO *)

  and fwdBlk funcs env_vars env_feats vars feats (p:B.t) (b:block) : B.t =
    let result_print l p =
      Format.fprintf !fmt "### %a ###: %a\n" label_print l B.print p
    in
    match b with
    | A_empty l ->
      if !tracefwd && not !minimal then result_print l p;
      addFwdInv l p; p
    | A_block (l,(s,_),b) ->
      if !tracefwd && not !minimal then result_print l p;
      addFwdInv l p; 
      fwdBlk funcs env_vars env_feats vars feats (fwdStm funcs env_vars env_feats vars feats p s) b

  (* Backward Iterator + Recursion *)


  let rec bwdStm funcs env_vars env_feats vars feats (p,r,flag) s =
    match s with
    | A_label _ -> (p,r,flag)
    | A_return -> p, r, flag
    | A_assign ((l,_),(e,_)) ->
      D.bwdAssign p (l, e), r, flag
    | A_assert (b,_) ->
      D.filter p b, r, flag
    | A_if ((b,ba),s1,s2) ->
      let (p1, _, flag1) = bwdBlk funcs env_vars env_feats vars feats (p, r, flag) s1 in
      let p1 = D.filter p1 b in					
      let (p2, _, flag2) = bwdBlk funcs env_vars env_feats vars feats (p, r, flag) s2 in
      let p2 = D.filter p2 (fst (negBExp (b, ba))) in
      if !tracebwd && not !minimal then begin
        Format.fprintf !fmt "p1: %a\n" D.print p1;
        Format.fprintf !fmt "p2: %a\n" D.print p2
      end;
      (D.join APPROXIMATION p1 p2, r, flag1 || flag2)
    | A_ifdef ((b,ba),s1,s2) ->
      let (p1', _, flag1') = bwdBlk funcs env_vars env_feats vars feats (p, r, flag) s1 in
	  let p1 = D.config_filter p1' b in 	  
      let (p2', _, flag2') = bwdBlk funcs env_vars env_feats vars feats (p, r, flag) s2 in
	  let b_neg = fst (negBExp (b,ba)) in 
      let p2 = D.config_filter p2' b_neg in
	  (*let pp = D.join COMPUTATIONAL p1' p2' in 
	  Format.fprintf !fmt "#ifdef p1': %a\n" D.print p1';
      Format.fprintf !fmt "#ifdef p2': %a\n" D.print p2';
      Format.fprintf !fmt "#ifdef pp: %a\n" D.print pp;	  *)
      (D.join COMPUTATIONAL p1 p2, r, flag1' || flag2')	  
    | A_while (l, (b, ba), s) ->
      (*let a = InvMap.find l !fwdInvMap in
      let dm = if !refine then Some a else None in  *)
      let p1 = D.filter p (fst (negBExp (b, ba))) in
      let rec aux (i, _, _) (p2, _, flag2) n =
        if !abort then raise Abort else
          let i' = D.join APPROXIMATION p1 p2 in
          if !tracebwd && not !minimal then begin
            Format.fprintf !fmt "### %a:%i ###:\n" label_print l n;
            Format.fprintf !fmt "p1: %a\n" D.print p1;
            Format.fprintf !fmt "i: %a\n" D.print i;
            Format.fprintf !fmt "p2: %a\n" D.print p2;
            Format.fprintf !fmt "i': %a\n" D.print i'
          end;
          let jokers = max 0 (!retrybwd * (!Ordinals.max + 1) - n + !joinbwd) in
          if (D.isLeq COMPUTATIONAL i' i) then
            if (D.isLeq APPROXIMATION i' i) then begin
              if !tracebwd && not !minimal then begin
                Format.fprintf !fmt "### %a:FIXPOINT ###:\n" label_print l;
                Format.fprintf !fmt "i: %a\n" D.print i
              end;
              (i, r, flag2)
            end else begin
              let i'' =
                if n <= !joinbwd then
                  i'
                else
                  D.widen ~jokers:jokers i i'
              in
              if !tracebwd && not !minimal then
                Format.fprintf !fmt "i'': %a\n" D.print i'';
              let (p2, _, flag2) = 
                bwdBlk funcs env_vars env_feats vars feats (i'', r, flag2) s in
              let p2' = D.filter p2 b in
              aux (i'', r, flag2) (p2', r, flag2) (n + 1)
            end
          else
            let i'' =
              if n <= !joinbwd then
                i'
              else
                D.widen ~jokers:jokers i
                  (D.join COMPUTATIONAL i i')
            in
            if !tracebwd && not !minimal then
              Format.fprintf !fmt "i'': %a\n" D.print i'';
            let (p2, _, flag2) = 
              bwdBlk funcs env_vars env_feats vars feats (i'', r, flag2) s in
            let p2' = D.filter p2 b in
            aux (i'', r, flag2) (p2', r, flag2) (n + 1)
      in
      let i = (D.bot env_vars env_feats vars feats, r, flag) in
      let (p2, _, flag2) = bwdBlk funcs env_vars env_feats vars feats i s in
      let p2' = D.filter p2 b in
      let (p, r, flag) = aux i (p2', r, flag2) 1 in
      addBwdInv l p;
      (p, r, flag)
    | A_call (f, ss) ->
      let f = StringMap.find f funcs in
      let p = bwdRec funcs env_vars env_feats vars feats p f.funcBody in
      List.fold_left (fun (ap, ar, aflag) (s, _) ->
          bwdStm funcs env_vars env_feats vars feats (ap, ar, aflag) s
        ) (p, r, flag) ss
    | A_recall (f, ss) ->
      List.fold_left (fun (ap, ar, aflag) (s, _) ->
             bwdStm funcs env_vars env_feats vars feats (ap, ar, aflag) s) (D.join APPROXIMATION p r, r, true) ss

  and bwdBlk funcs env_vars env_feats vars feats (p,r,flag) (b:block) : D.t * D.t * bool =
    let result_print l p =
      Format.fprintf !fmt "### %a ###:\n%a@." label_print l D.print p
    in
    match b with
    | A_empty l ->
      (*let a = InvMap.find l !fwdInvMap in *)
      (*let p = if !refine then D.refine p a else p in*)
      if !tracebwd && not !minimal then result_print l p;
      addBwdInv l p; (p,r,flag)
    | A_block (l,(s,_),b) ->
      stop := Sys.time ();
      if ((!stop -. !start) > !timeout)
      then raise Timeout
      else
        let (b,rb,flagb) = bwdBlk funcs env_vars env_feats vars feats (p,r,flag) b in
        (*let a = InvMap.find l !fwdInvMap in  *)
        let (p,r,flag) = if !refine then 
            bwdStm funcs env_vars env_feats vars feats (b,rb,flagb) s 
          else bwdStm funcs env_vars env_feats vars feats (b,rb,flagb) s in
        (*let p = if !refine then D.refine p a else p in *)
        if !tracebwd && not !minimal then result_print l p;
        addBwdInv l p; (p,r,flag)

  and bwdRec funcs env_vars env_feats vars feats (p:D.t) (b:block) : D.t = 
    let (res, _, _) = bwdBlk funcs env_vars env_feats vars feats (p,D.bot env_vars env_feats vars feats,false) b  in
    res
	
  (* NOTE: unsound *)
  (* and bwdRec funcs env vars (p:D.t) (b:block) : D.t = *) 
    (* let rec aux r n = *)
    (*   let (r',_,_) = bwdBlk funcs env vars (r,r,true) b in *)
    (*   if !tracebwd && not !minimal then begin *)
    (*     Format.fprintf !fmt "r: %a@." D.print r; *)
    (*     Format.fprintf !fmt "r': %a@." D.print r' *)
    (*   end; *)
    (*   if (D.isLeq COMPUTATIONAL r' r) *)
    (*   then *)
    (*     if (D.isLeq APPROXIMATION r' r) *)
    (*     then *)
    (*       let r = r in *)
    (*       if !tracebwd && not !minimal then begin *)
    (*         Format.fprintf !fmt "### REC-FIXPOINT ###:@."; *)
    (*         Format.fprintf !fmt "r: %a@." D.print r *)
    (*       end; *)
    (*       r *)
    (*     else *)
    (*       let r'' = if n <= !joinbwd then r' else D.widen r r' in *)
    (*       if !tracebwd && not !minimal then *)
    (*         Format.fprintf !fmt "r'': %a@." D.print r''; *)
    (*       aux r'' (n+1) *)
    (*   else *)
    (*     let r'' = if n <= !joinbwd then r' else *) 
    (*         D.widen r (D.join COMPUTATIONAL r r') in *)
    (*     if !tracebwd && not !minimal then *)
    (*       Format.fprintf !fmt "r'': %a@." D.print r''; *)
    (*     aux r'' (n+1) *)
    (* in *)
    (* let (i,_,flag) = bwdBlk funcs env vars (p,D.bot env vars,false) b in *)
    (* if flag then aux i 1 *)
    (* else i *)

  (* Analyzer *)

  let rec initStm env_vars env_feats vars feats s =
    match s with
    | A_if (_,s1,s2) -> initBlk env_vars env_feats vars feats s1; initBlk env_vars env_feats vars feats s2
    | A_while (l,_,s) -> 
      addBwdInv l (D.bot env_vars env_feats vars feats); initBlk env_vars env_feats vars feats s
    | _ -> ()

  and	initBlk env_vars env_feats vars feats b =
    match b with
    | A_empty l -> addBwdInv l (D.bot env_vars env_feats vars feats)
    | A_block (l,(s,_),b) -> addBwdInv l (D.bot env_vars env_feats vars feats); 
      initStm env_vars env_feats vars feats s; initBlk env_vars env_feats vars feats b


let rec process list = 
	if List.length list = 0 then [[]]
	else match list with
		| [] -> []
		| hd :: tl -> 
			let tmp = ref [] in
			let dom = hd.featDom in
			for i = 0 to (List.length dom)-1 do
				let tmp1 = List.map (fun l -> (hd.featName,List.nth dom i) :: l) (process tl) in 
				tmp := !tmp @ tmp1
			done;
			!tmp;;

let rec process_cons feats_list env_feats feats ll = 
		match feats_list with
		| [] -> ll
		| hd :: tl ->
			let tmp = ref [] in
			let typ = hd.featTyp in
			if (typ <> A_BOOL) then (
				let dom = hd.featDom in
				let cons1 = Lincons1.make (Linexpr1.make env_feats) Lincons1.SUPEQ in
  				Lincons1.set_array cons1 [| ((Coeff.s_of_int 1), (Var.of_string hd.featId)) |] (Some (Coeff.s_of_int (-(List.nth dom 0))));
				let cons2 = Lincons1.make (Linexpr1.make env_feats) Lincons1.SUPEQ in
  				Lincons1.set_array cons2 [| ((Coeff.s_of_int (-1)), (Var.of_string hd.featId)) |] (Some (Coeff.s_of_int (List.nth dom ((List.length dom)-1))));				
				tmp := cons1::cons2::!tmp
			); 
			process_cons tl env_feats feats (!tmp @ ll)


  let analyze (vars,stmts,funcs) main =
    let rec aux xs env = match xs with
      | [] -> env
      | x::xs -> 
        aux xs (Environment.add env [|(Var.of_string x.varId)|] [||])
    in
    let f = StringMap.find main funcs in
    let v1 = snd (List.split (StringMap.bindings vars)) in
    let v2 = snd (List.split (StringMap.bindings f.funcVars)) in
    let vars = List.append v1 v2 in
    let env_vars = aux vars (Environment.make [||] [||]) in
    let s = f.funcBody in
    (*initBlk env vars stmts; initBlk env vars s; *)
    (* TODO: handle functions calls *)
    (* Forward Analysis *)
    if !tracefwd && not !minimal then
      Format.fprintf !fmt "\nForward Analysis Trace:\n";
    let startfwd = Sys.time () in
	
	let feats = ref [] in
	let feats_feat = ref [] in
	let env_feats = ref (Environment.make [||] [||]) in 
	StringMap.iter (fun key value -> feats_feat := value :: !feats_feat; feats := {varId = value.featId; varName = value.featName; varTyp = value.featTyp} :: !feats; env_feats :=  Environment.add !env_feats [|(Var.of_string value.featId)|] [||]) !ItoA.features; 
	let constraints_list = process_cons !feats_feat !env_feats !feats [] in 
	let part_listP = P.inner !env_feats !feats constraints_list in 
	let part_listS = S.inner !env_feats !feats constraints_list in 
	
    let state = fwdBlk funcs env_vars !env_feats vars !feats (fwdBlk funcs env_vars !env_feats vars !feats (B.top ~domain:part_listP env_vars !env_feats vars !feats) stmts) s in
    let stopfwd = Sys.time () in
    if not !minimal then
      begin
        if !timefwd then
          Format.fprintf !fmt "\nForward Analysis (Time: %f s):\n" (stopfwd-.startfwd)
        else
          Format.fprintf !fmt "\nForward Analysis:\n";
        fwdMap_print !fmt !fwdInvMap;
      end;  
    (* Backward Analysis *)
    if !tracebwd && not !minimal then
      Format.fprintf !fmt "\nBackward Analysis Trace:\n";
    start := Sys.time ();
    let startbwd = Sys.time () in
    let i = bwdRec funcs env_vars !env_feats vars !feats (bwdRec funcs env_vars !env_feats vars !feats (D.zero ~domain:part_listS env_vars !env_feats vars !feats) s) stmts in
    let stopbwd = Sys.time () in
    if not !minimal then begin
      if !timebwd then
        Format.fprintf !fmt "\nBackward Analysis (Time: %f s):\n" (stopbwd-.startbwd)
      else
        Format.fprintf !fmt "\nBackward Analysis:\n";
      bwdMap_print !fmt !bwdInvMap;
    end;
    let b = D.defined i in 
	Format.fprintf !fmt "defined {%b}\n " b;
	Format.fprintf !fmt "Ranking function is: \n%a\n" D.print (InvMap.find 1 !bwdInvMap); 
	b
	(*if (b) then Format.fprintf !fmt "Ranking function is: \n%a\n" D.print (InvMap.find 1 !bwdInvMap); 
	  b  *)


end
