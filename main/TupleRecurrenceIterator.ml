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
open Tuplep
open Tuplerf

module TupleRecurrenceIterator (B: TUPLEP) (D: TUPLERF) =
struct

  module D = D

  module B = B

  let fwdInvMap = ref InvMap.empty

  let addFwdInv l (a:B.t) = fwdInvMap := InvMap.add l a !fwdInvMap

  let fwdMap_print fmt m = InvMap.iter (fun l a -> 
      Format.fprintf fmt "%a: %a\n" label_print l B.print a) m

  let bwdInvMap = ref InvMap.empty

  let addBwdInv l (a:D.t) = bwdInvMap := InvMap.add l a !bwdInvMap

  let bwdMap_print fmt m = InvMap.iter (fun l a -> 
      Format.fprintf fmt "%a: %a\n" label_print l D.print a) m  


  (* Backward Iterator + Recursion *)


  let rec bwdStm property funcs env env_feat vars configs p s =
    match s with
    | A_label (l,_) -> D.label l property p 
    | A_return -> D.bot env vars configs
    | A_assign ((l,_),(e,_)) -> D.bwdAssign p (l, e)
    | A_assert (b,_) -> D.filter p b
    | A_if ((b,ba),s1,s2) ->
      let p1 = bwdBlk property funcs env env_feat vars configs p s1 in
      let p1 = D.filter p1 b in					
      let p2 = bwdBlk property funcs env env_feat vars configs p s2 in
      let p2 = D.filter p2 (fst (negBExp (b, ba))) in
      (D.join APPROXIMATION p1 p2)
    | A_ifdef ((b,ba),s1,s2) ->
      let p1 = D.config_filter env_feat p b in 
      let p1' = bwdBlk property funcs env env_feat vars configs p1 s1 in
      let p2 = D.config_filter_not env_feat p p1 in
      let p2' = bwdBlk property funcs env env_feat vars configs p2 s2 in
	  (*let pp = D.join COMPUTATIONAL p1' p2' in 
	  Format.fprintf !fmt "#ifdef p1': %a\n" D.print p1';
      Format.fprintf !fmt "#ifdef p2': %a\n" D.print p2';
      Format.fprintf !fmt "#ifdef pp: %a\n" D.print pp;	  *)
      (D.join COMPUTATIONAL p1' p2')	  
    | A_while (l, (b, ba), s) ->
      (* let a = InvMap.find l !fwdInvMap in
      let dm = if !refine then Some a else None in  *)
      let p1 = D.filter p (fst (negBExp (b, ba))) in
      let rec aux m o =
        let rec auxaux i p2 n =
          let i' = D.resetmask m (D.join APPROXIMATION p1 p2) (fst (StringMap.find "" property)) in
          if !tracebwd && not !minimal then begin
            Format.fprintf !fmt "### %a-INNER:%i ###:\n" label_print l n;
            Format.fprintf !fmt "p1: %a\n" D.print p1;
            Format.fprintf !fmt "i: %a\n" D.print i;
            Format.fprintf !fmt "p2: %a\n" D.print p2;
            Format.fprintf !fmt "i': %a\n" D.print i';
          end;
          if (D.isLeq COMPUTATIONAL i' i)
          then
            if (D.isLeq APPROXIMATION i' i)
            then
              let i = i in
              if !tracebwd && not !minimal then
                Format.fprintf !fmt "### %a-INNER:FIXPOINT ###:\n" label_print l;
              if !tracebwd && not !minimal then
                Format.fprintf !fmt "i: %a\n" D.print i;
              i
            else
              let i'' = if n <= !joinbwd then i' else D.widen i i' in
              if !tracebwd && not !minimal then
                Format.fprintf !fmt "i'': %a\n" D.print i'';
              auxaux i'' (D.filter (bwdBlk property funcs env env_feat vars configs i'' s) b) (n+1)
          else
            let i'' = if n <= !joinbwd then i' else D.widen i (D.join COMPUTATIONAL i i') in
            if !tracebwd && not !minimal then
              Format.fprintf !fmt "i'': %a\n" D.print i'';
            auxaux i'' (D.filter (bwdBlk property funcs env env_feat vars configs  i'' s) b) (n+1)
        in

        if !tracebwd && not !minimal then
          Format.fprintf !fmt "### %a-OUTER:%i ###:\n" label_print l o;
        if !tracebwd && not !minimal then
          Format.fprintf !fmt "m: %a\n" D.print m;

        let p2 = D.filter (D.bot env vars configs) b in
        let p = auxaux (D.bot env vars configs) p2 1 in					
        let p = D.join APPROXIMATION p1 (D.filter (bwdBlk property funcs env env_feat vars configs  p s) b) in

        if !tracebwd && not !minimal then
          Format.fprintf !fmt "p: %a\n" D.print p;

        let m' = D.meet APPROXIMATION m p in

        if !tracebwd && not !minimal then
          Format.fprintf !fmt "m': %a\n" D.print m';

        if (D.isLeq COMPUTATIONAL m' m) && (D.isLeq COMPUTATIONAL m m')
        then
          let p = p in
          if !tracebwd && not !minimal then begin
            Format.fprintf !fmt "### %a-OUTER:FIXPOINT ###:\n" label_print l;
            Format.fprintf !fmt "m: %a\n" D.print m;
          end;
          p
        else
          let m'' = if o <= !meetbwd then m' else D.dual_widen m m' in
          if !tracebwd && not !minimal then
            Format.fprintf !fmt "m'': %a\n" D.print m'';
          aux m'' (o+1)
      in
      let p = aux (D.top env vars configs) 1 in
      addBwdInv l p; p
    | A_call (f,ss) -> raise (Invalid_argument "bwdStm:A_call")
    | A_recall (f,ss) -> raise (Invalid_argument "bwdStm:A_recall")

  and bwdBlk property funcs env env_feat vars configs p (b:block) : D.t =
    let result_print l p =
      Format.fprintf !fmt "### %a ###:\n%a@." label_print l D.print p
    in
    match b with
    | A_empty l ->
      let p = D.reset p (fst (StringMap.find "" property)) in
      if !tracebwd && not !minimal then result_print l p;
      addBwdInv l p; p
    | A_block (l,(s,_),b) ->
      stop := Sys.time ();
      if ((!stop -. !start) > !timeout)
      then raise Timeout
      else
        let b = bwdBlk property funcs env env_feat vars configs p b in
        (*let a = InvMap.find l !fwdInvMap in  *)
        let p = if !refine then 
            bwdStm property funcs env env_feat vars configs b s 
          else bwdStm property funcs env env_feat vars configs b s in
        let p = D.reset p (fst (StringMap.find "" property)) in
        (*let p = if !refine then D.refine p a else p in *)
        if !tracebwd && not !minimal then result_print l p;
        addBwdInv l p; p

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

  let rec initStm env env_feat vars configs s =
    match s with
    | A_if (_,s1,s2) -> initBlk env env_feat vars configs s1; initBlk env env_feat vars configs s2
    | A_while (l,_,s) -> 
      addBwdInv l (D.bot env vars configs); initBlk env env_feat vars configs s
    | _ -> ()

  and	initBlk env env_feat vars configs b =
    match b with
    | A_empty l -> addBwdInv l (D.bot env vars configs)
    | A_block (l,(s,_),b) -> addBwdInv l (D.bot env vars configs); 
      initStm env env_feat vars configs s; initBlk env env_feat vars configs b


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


  let analyze property (vars,stmts,funcs) main =
    let rec aux xs env = match xs with
      | [] -> env
      | x::xs -> 
        aux xs (Environment.add env [|(Var.of_string x.varId)|] [||])
    in
    let f = StringMap.find main funcs in
    let v1 = snd (List.split (StringMap.bindings vars)) in
    let v2 = snd (List.split (StringMap.bindings f.funcVars)) in
    let vars = List.append v1 v2 in
    let env = aux vars (Environment.make [||] [||]) in
    let s = f.funcBody in
    (*initBlk env vars stmts; initBlk env vars s; *)
    (* TODO: handle functions calls *)
    (* Forward Analysis *)
    if !tracefwd && not !minimal then
      Format.fprintf !fmt "\nForward Analysis Trace:\n";
    let startfwd = Sys.time () in
	
	let list_feat = ref [] in
	let env_feat = ref (Environment.make [||] [||]) in 
	StringMap.iter (fun key value -> list_feat := value :: !list_feat; env_feat :=  Environment.add !env_feat [|(Var.of_string value.featId)|] [||]) !ItoA.features; 
	let list_configs = process !list_feat in 	
	
    (* let _ = fwdBlk funcs env !env_feat vars list_configs (fwdBlk funcs env !env_feat vars list_configs (B.top env vars list_configs) stmts) s in
    let stopfwd = Sys.time () in
    if not !minimal then
      begin
        if !timefwd then
          Format.fprintf !fmt "\nForward Analysis (Time: %f s):\n" (stopfwd-.startfwd)
        else
          Format.fprintf !fmt "\nForward Analysis:\n";
        fwdMap_print !fmt !fwdInvMap;
      end;  *)
    (* Backward Analysis *)
    if !tracebwd && not !minimal then
      Format.fprintf !fmt "\nBackward Analysis Trace:\n";
    start := Sys.time ();
    let startbwd = Sys.time () in
    let i = bwdBlk property funcs env !env_feat vars list_configs (bwdBlk property funcs env !env_feat vars list_configs (D.bot env vars list_configs) s) stmts in
    let stopbwd = Sys.time () in
    if not !minimal then begin
      if !timebwd then
        Format.fprintf !fmt "\nBackward Analysis (Time: %f s):\n" (stopbwd-.startbwd)
      else
        Format.fprintf !fmt "\nBackward Analysis:\n";
      bwdMap_print !fmt !bwdInvMap;
    end;
    let b = D.defined i in 
	List.iter2 (fun el key ->  List.iter (fun (key,v) -> Format.fprintf !fmt "%s{%d} " key v) el; Format.fprintf !fmt "is terminating  %b.\n" key) list_configs b;
	Format.fprintf !fmt "Ranking function is: \n%a\n" D.print (InvMap.find 1 !bwdInvMap); 
	let bb = ref true in List.iter (fun key -> if (not key) then bb:=false) b;  
	!bb
	(*if (b) then Format.fprintf !fmt "Ranking function is: \n%a\n" D.print (InvMap.find 1 !bwdInvMap); 
	  b  *)


end
