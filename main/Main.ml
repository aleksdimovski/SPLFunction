(***************************************************)
(*                                                 *)
(*                        Main                     *)
(*                                                 *)
(*             Aleksandar Dimovski                 *)
(*          Mother Teresa Uni, Skopje              *)
(*                   2021		                   *)
(*                                                 *)
(***************************************************)

(* parsing *)
open AbstractSyntax
open ItoA

let analysis = ref "single"
let domain = ref "boxes"
let filename = ref ""
let fmt = ref Format.std_formatter
let main = ref "main"
let minimal = ref false
let ordinals = ref false
let precondition = ref "true"
let time = ref true
let noinline = ref false
let property = ref "" 
let istree = ref false 

let parseFile filename =
  let f = open_in filename in
  let lex = Lexing.from_channel f in
  try
    lex.Lexing.lex_curr_p <- { lex.Lexing.lex_curr_p
                               with Lexing.pos_fname = filename; };
    let r = Parser.file Lexer.start lex in
    close_in f; r
  with
  | Parser.Error ->
    Printf.eprintf "Parse Error (Invalid Syntax) near %s\n"
      (IntermediateSyntax.position_tostring lex.Lexing.lex_start_p);
    failwith "Parse Error"
  | Failure e ->
    if e == "lexing: empty token" then 
      begin
        Printf.eprintf "Parse Error (Invalid Token) near %s\n" (IntermediateSyntax.position_tostring lex.Lexing.lex_start_p);
        failwith "Parse Error"
      end 
    else
      failwith e

let parsePropertyString str =
  let lex = Lexing.from_string str in
  try
    PropertyParser.file PropertyLexer.start lex 
  with
  | PropertyParser.Error ->
    Printf.eprintf "Parse Error (Invalid Syntax) near %s\n" (IntermediateSyntax.position_tostring lex.Lexing.lex_start_p);
    failwith "Parse Error"
  | Failure e ->
    if e == "lexing: empty token" then 
      begin
        Printf.eprintf "Parse Error (Invalid Token) near %s\n" (IntermediateSyntax.position_tostring lex.Lexing.lex_start_p);
        failwith "Parse Error"
      end 
    else
      failwith e


let parseProperty filename =
  let f = open_in filename in
  let lex = Lexing.from_channel f in
  try
    lex.Lexing.lex_curr_p <- { lex.Lexing.lex_curr_p
                               with Lexing.pos_fname = filename; };
    let r = PropertyParser.file PropertyLexer.start lex in
    close_in f; r
  with
  | PropertyParser.Error -> Printf.eprintf "Parse Error (Invalid Syntax) near %s\n" (IntermediateSyntax.position_tostring lex.Lexing.lex_start_p);
    failwith "Parse Error"
  | Failure e ->
    if e == "lexing: empty token" then 
      begin
        Printf.eprintf "Parse Error (Invalid Token) near %s\n" (IntermediateSyntax.position_tostring lex.Lexing.lex_start_p);
        failwith "Parse Error"
      end 
    else
      failwith e

let parse_args () =
  let rec doit args =
    match args with
    (* General arguments -------------------------------------------*)
    | "-domain"::x::r -> (* abstract domain: boxes|octagons|polyhedra *)
      domain := x; doit r
    | "-timeout"::x::r -> (* analysis timeout *)
      Iterator.timeout := float_of_string x; doit r
    | "-joinfwd"::x::r -> (* widening delay in forward analysis *)
      Iterator.joinfwd := int_of_string x; doit r
    | "-joinbwd"::x::r -> (* widening delay in backward analysis *)
      Iterator.joinbwd := int_of_string x; doit r
    | "-main"::x::r -> (* analyzer entry point *) main := x; doit r
    | "-meetbwd"::x::r -> (* dual widening delay in backward analysis *)
      Iterator.meetbwd := int_of_string x; doit r
    | "-minimal"::r -> (* analysis result only *)
      minimal := true; Iterator.minimal := true; doit r
    | "-ordinals"::x::r -> (* ordinal-valued ranking functions *)
      ordinals := true; Ordinals.max := int_of_string x; doit r
    | "-refine"::r -> (* refine in backward analysis *)
      Iterator.refine := true; doit r
    | "-retrybwd"::x::r ->
      Iterator.retrybwd := int_of_string x;
      DecisionTree.retrybwd := int_of_string x;
      doit r
    | "-tracebwd"::r -> (* backward analysis trace *)
      Iterator.tracebwd := true;
      DecisionTree.tracebwd := true;
      CFGInterpreter.trace := true;
      CFGInterpreter.trace_states := true;
      doit r
    | "-tracefwd"::r -> (* forward analysis trace *)
      Iterator.tracefwd := true; doit r
    (* Single analysis -------------------------------*)
    | "-single"::r -> (* single analysis *)
      analysis := "single"; doit r
    | "-single-guarantee"::x::r -> (* guarantee analysis *)
      analysis := "single-guarantee"; property := x; doit r         
    | "-single-recurrence"::x::r -> (* guarantee analysis *)
      analysis := "single-recurrence"; property := x; doit r           
    (* Tuple analysis -------------------------------*)
    | "-tuple"::r -> (* tuple analysis *)
      analysis := "tuple"; doit r
    | "-tuple-guarantee"::x::r -> (* guarantee analysis *)
      analysis := "tuple-guarantee"; property := x; doit r      
    | "-tree-guarantee"::x::r -> (* guarantee analysis *)
      analysis := "tree-guarantee"; property := x; doit r    
    | "-tree-recurrence"::x::r -> (* guarantee analysis *)
      analysis := "tree-recurrence"; property := x; doit r             
    | "-tuple-recurrence"::x::r -> (* guarantee analysis *)
      analysis := "tuple-recurrence"; property := x; doit r            
    | "-tree"::r -> (* dection tree analysis *)
      analysis := "tree"; doit r
    | "-sketch"::r -> (* Sketch analysis *)
      analysis := "sketch"; doit r	  
    | "-sketchtuple"::r -> (* Sketch analysis with tuples*)
      analysis := "sketchtuple"; doit r		  
    | "-time"::r -> (* track analysis time *)
      time := true; doit r
    | "-timebwd"::r -> (* track backward analysis time *)
      Iterator.timebwd := true; doit r
    | "-timefwd"::r -> (* track forward analysis time *)
      Iterator.timefwd := true; doit r
    | "-precondition"::c::r -> (* optional precondition that holds 
                                  at the start of the program, default = true *)
      precondition := c; doit r 
   | "-noinline"::r -> (* don't inline function calls, only for CFG based analysis *)
      noinline := true; doit r
    | x::r -> filename := x; doit r
    | [] -> ()
  in
  doit (List.tl (Array.to_list Sys.argv))

(* do all *)

module SingleTerminationBoxes =
  SingleTerminationIterator.SingleTerminationIterator(DecisionTree.TSAB)
module SingleTerminationBoxesOrdinals =
  SingleTerminationIterator.SingleTerminationIterator(DecisionTree.TSOB)
module SingleTerminationOctagons =
  SingleTerminationIterator.SingleTerminationIterator(DecisionTree.TSAO)
module SingleTerminationOctagonsOrdinals =
  SingleTerminationIterator.SingleTerminationIterator(DecisionTree.TSOO)
module SingleTerminationPolyhedra =
  SingleTerminationIterator.SingleTerminationIterator(DecisionTree.TSAP)
module SingleTerminationPolyhedraOrdinals =
  SingleTerminationIterator.SingleTerminationIterator(DecisionTree.TSOP)

module SingleGuaranteeBoxes =
  SingleGuaranteeIterator.SingleGuaranteeIterator(DecisionTree.TSAB)
module SingleGuaranteeOctagons =
  SingleGuaranteeIterator.SingleGuaranteeIterator(DecisionTree.TSAO)
module SingleGuaranteePolyhedra =
  SingleGuaranteeIterator.SingleGuaranteeIterator(DecisionTree.TSAP)

module SingleRecurrenceBoxes =
  SingleRecurrenceIterator.SingleRecurrenceIterator(DecisionTree.TSAB)
module SingleRecurrenceOctagons =
  SingleRecurrenceIterator.SingleRecurrenceIterator(DecisionTree.TSAO)
module SingleRecurrencePolyhedra =
  SingleRecurrenceIterator.SingleRecurrenceIterator(DecisionTree.TSAP)

module TupleGuaranteeBoxes =
  TupleGuaranteeIterator.TupleGuaranteeIterator(Maketuplep.TB)(Maketuplerf.TRB)
module TupleGuaranteeOctagons =
  TupleGuaranteeIterator.TupleGuaranteeIterator(Maketuplep.TO)(Maketuplerf.TRO)
module TupleGuaranteePolyhedra =
  TupleGuaranteeIterator.TupleGuaranteeIterator(Maketuplep.TP)(Maketuplerf.TRP)

module TupleRecurrenceBoxes =
  TupleRecurrenceIterator.TupleRecurrenceIterator(Maketuplep.TB)(Maketuplerf.TRB)
module TupleRecurrenceOctagons =
  TupleRecurrenceIterator.TupleRecurrenceIterator(Maketuplep.TO)(Maketuplerf.TRO)
module TupleRecurrencePolyhedra =
  TupleRecurrenceIterator.TupleRecurrenceIterator(Maketuplep.TP)(Maketuplerf.TRP)

module TupleTerminationBoxes =
  TupleTerminationIterator.TupleTerminationIterator(Maketuplep.TB)(Maketuplerf.TRB)
module TupleTerminationBoxesOrdinals =
  TupleTerminationIterator.TupleTerminationIterator(Maketuplep.TB)(Maketuplerf.TROB)
module TupleTerminationOctagons =
  TupleTerminationIterator.TupleTerminationIterator(Maketuplep.TO)(Maketuplerf.TRO)
module TupleTerminationOctagonsOrdinals =
  TupleTerminationIterator.TupleTerminationIterator(Maketuplep.TO)(Maketuplerf.TROO)
module TupleTerminationPolyhedra =
  TupleTerminationIterator.TupleTerminationIterator(Maketuplep.TP)(Maketuplerf.TRP)
module TupleTerminationPolyhedraOrdinals =
  TupleTerminationIterator.TupleTerminationIterator(Maketuplep.TP)(Maketuplerf.TROP)


module DTreeTerminationBoxes =
  DTreeTerminationIterator.DTreeTerminationIterator(MakeDTDomainP.DTB)(MakeDTDomainRF.DTBRF)
module DTreeTerminationBoxesOrdinals =
  DTreeTerminationIterator.DTreeTerminationIterator(MakeDTDomainP.DTB)(MakeDTDomainRF.DTBORF)
module DTreeTerminationOctagons =
  DTreeTerminationIterator.DTreeTerminationIterator(MakeDTDomainP.DTO)(MakeDTDomainRF.DTORF)
module DTreeTerminationOctagonsOrdinals =
  DTreeTerminationIterator.DTreeTerminationIterator(MakeDTDomainP.DTO)(MakeDTDomainRF.DTOORF)
module DTreeTerminationPolyhedra =
  DTreeTerminationIterator.DTreeTerminationIterator(MakeDTDomainP.DTP)(MakeDTDomainRF.DTPRF)
module DTreeTerminationPolyhedraOrdinals =
  DTreeTerminationIterator.DTreeTerminationIterator(MakeDTDomainP.DTP)(MakeDTDomainRF.DTPORF)



module SketchTerminationBoxes =
  SketchTerminationIterator.SketchTerminationIterator(MakeSketchDomainP.SketchB)(MakeSketchDomainRF.SketchBRF)
module SketchTerminationBoxesOrdinals =
  SketchTerminationIterator.SketchTerminationIterator(MakeSketchDomainP.SketchB)(MakeSketchDomainRF.SketchBORF)
module SketchTerminationOctagons =
  SketchTerminationIterator.SketchTerminationIterator(MakeSketchDomainP.SketchO)(MakeSketchDomainRF.SketchORF)
module SketchTerminationOctagonsOrdinals =
  SketchTerminationIterator.SketchTerminationIterator(MakeSketchDomainP.SketchO)(MakeSketchDomainRF.SketchOORF)
module SketchTerminationPolyhedra =
  SketchTerminationIterator.SketchTerminationIterator(MakeSketchDomainP.SketchP)(MakeSketchDomainRF.SketchPRF)
module SketchTerminationPolyhedraOrdinals =
  SketchTerminationIterator.SketchTerminationIterator(MakeSketchDomainP.SketchP)(MakeSketchDomainRF.SketchPORF)


module SketchTupleTerminationBoxes =
  SketchTupleTerminationIterator.SketchTupleTerminationIterator(Maketuplep.TB)(Maketuplerf.TRB)
module SketchTupleTerminationBoxesOrdinals =
  SketchTupleTerminationIterator.SketchTupleTerminationIterator(Maketuplep.TB)(Maketuplerf.TROB)
module SketchTupleTerminationOctagons =
  SketchTupleTerminationIterator.SketchTupleTerminationIterator(Maketuplep.TO)(Maketuplerf.TRO)
module SketchTupleTerminationOctagonsOrdinals =
  SketchTupleTerminationIterator.SketchTupleTerminationIterator(Maketuplep.TO)(Maketuplerf.TROO)
module SketchTupleTerminationPolyhedra =
  SketchTupleTerminationIterator.SketchTupleTerminationIterator(Maketuplep.TP)(Maketuplerf.TRP)
module SketchTupleTerminationPolyhedraOrdinals =
  SketchTupleTerminationIterator.SketchTupleTerminationIterator(Maketuplep.TP)(Maketuplerf.TROP)



let result = ref false

let run_analysis analysis_function program () =
  try 
    let start = Sys.time () in
    let terminating = analysis_function program !main in
    let stop = Sys.time () in
    Format.fprintf !fmt "Analysis Result: ";
    let result = if terminating then "TRUE" else "UNKNOWN" in
    Format.fprintf !fmt "%s\n" result;
    if !time then
      if (!istree) then Format.fprintf !fmt "Time: %f s\n" ((stop-.start)/.4.0) else Format.fprintf !fmt "Time: %f s\n" (stop-.start);
    Format.fprintf !fmt "\nDone.\n"
  with
  | Iterator.Timeout ->
    Format.fprintf !fmt "\nThe Analysis Timed Out!\n";
    Format.fprintf !fmt "\nDone.\n"

let single () =
  if !filename = "" then raise (Invalid_argument "No Source File Specified");
  let sources = parseFile !filename in
  let (program,_) = ItoA.prog_itoa sources in
  if not !minimal then
    begin
      Format.fprintf !fmt "\nAbstract Syntax:\n";
      AbstractSyntax.prog_print !fmt program;
    end;
  let analysis_function =
    match !domain with
    | "boxes" -> if !ordinals then SingleTerminationBoxesOrdinals.analyze else SingleTerminationBoxes.analyze
    | "octagons" -> if !ordinals then SingleTerminationOctagonsOrdinals.analyze else SingleTerminationOctagons.analyze
    | "polyhedra" -> if !ordinals then SingleTerminationPolyhedraOrdinals.analyze else SingleTerminationPolyhedra.analyze
    | _ -> raise (Invalid_argument "Unknown Abstract Domain")
  in run_analysis analysis_function program ()


let single_guar () =
  if !filename = "" then raise (Invalid_argument "No Source File Specified");
  if !property = "" then raise (Invalid_argument "No Property File Specified");
  let sources = parseFile !filename in
  let property = parseProperty !property in
  let (program, property) =
    ItoA.prog_itoa ~property:(!main,property) sources in
  let property =
    match property with
    | None -> raise (Invalid_argument "Unknown Property")
    | Some property -> property
  in
  if not !minimal then
    begin
      Format.fprintf !fmt "\nAbstract Syntax:\n";
      AbstractSyntax.prog_print !fmt program;
      Format.fprintf !fmt "\nProperty: ";
      AbstractSyntax.property_print !fmt property;
    end;
  let analysis_function =
    match !domain with
    | "boxes" -> if !ordinals then SingleGuaranteeBoxes.analyze else SingleGuaranteeBoxes.analyze
    | "octagons" -> if !ordinals then SingleGuaranteeOctagons.analyze else SingleGuaranteeOctagons.analyze
    | "polyhedra" -> if !ordinals then SingleGuaranteePolyhedra.analyze else SingleGuaranteePolyhedra.analyze
    | _ -> raise (Invalid_argument "Unknown Abstract Domain")
  in run_analysis (analysis_function property) program ()

let single_recur () =
  if !filename = "" then raise (Invalid_argument "No Source File Specified");
  if !property = "" then raise (Invalid_argument "No Property File Specified");
  let sources = parseFile !filename in
  let property = parseProperty !property in
  let (program, property) =
    ItoA.prog_itoa ~property:(!main,property) sources in
  let property =
    match property with
    | None -> raise (Invalid_argument "Unknown Property")
    | Some property -> property
  in
  if not !minimal then
    begin
      Format.fprintf !fmt "\nAbstract Syntax:\n";
      AbstractSyntax.prog_print !fmt program;
      Format.fprintf !fmt "\nProperty: ";
      AbstractSyntax.property_print !fmt property;
    end;
  let analysis_function =
    match !domain with
    | "boxes" -> if !ordinals then SingleRecurrenceBoxes.analyze else SingleRecurrenceBoxes.analyze
    | "octagons" -> if !ordinals then SingleRecurrenceOctagons.analyze else SingleRecurrenceOctagons.analyze
    | "polyhedra" -> if !ordinals then SingleRecurrencePolyhedra.analyze else SingleRecurrencePolyhedra.analyze
    | _ -> raise (Invalid_argument "Unknown Abstract Domain")
  in run_analysis (analysis_function property) program ()

let tuple () =
  if !filename = "" then raise (Invalid_argument "No Source File Specified");
  let sources = parseFile !filename in
  let (program,_) = ItoA.prog_itoa sources in
  if not !minimal then
    begin
      Format.fprintf !fmt "\nAbstract Syntax:\n";
      AbstractSyntax.prog_print !fmt program;
    end;
  AbstractSyntax.StringMap.iter (fun key v -> Format.fprintf !fmt "%s" key; List.iter print_int v.featDom) !ItoA.features; 
  let analysis_function =
    match !domain with
    | "boxes" -> if !ordinals then TupleTerminationBoxesOrdinals.analyze else TupleTerminationBoxes.analyze
    | "octagons" -> if !ordinals then TupleTerminationOctagonsOrdinals.analyze else TupleTerminationOctagons.analyze
    | "polyhedra" -> if !ordinals then TupleTerminationPolyhedraOrdinals.analyze else TupleTerminationPolyhedra.analyze
    | _ -> raise (Invalid_argument "Unknown Abstract Domain") 
  in run_analysis analysis_function program ()  

let tuple_guar () =
  if !filename = "" then raise (Invalid_argument "No Source File Specified");
  if !property = "" then raise (Invalid_argument "No Property File Specified");
  let sources = parseFile !filename in
  let property = parseProperty !property in
  let (program, property) =
    ItoA.prog_itoa ~property:(!main,property) sources in
  let property =
    match property with
    | None -> raise (Invalid_argument "Unknown Property")
    | Some property -> property
  in
  if not !minimal then
    begin
      Format.fprintf !fmt "\nAbstract Syntax:\n";
      AbstractSyntax.prog_print !fmt program;
      Format.fprintf !fmt "\nProperty: ";
      AbstractSyntax.property_print !fmt property;
    end;
  let analysis_function =
    match !domain with
    | "boxes" -> if !ordinals then TupleGuaranteeBoxes.analyze else TupleGuaranteeBoxes.analyze
    | "octagons" -> if !ordinals then TupleGuaranteeOctagons.analyze else TupleGuaranteeOctagons.analyze
    | "polyhedra" -> if !ordinals then TupleGuaranteePolyhedra.analyze else TupleGuaranteePolyhedra.analyze
    | _ -> raise (Invalid_argument "Unknown Abstract Domain")
  in run_analysis (analysis_function property) program ()

let tuple_recur () =
  if !filename = "" then raise (Invalid_argument "No Source File Specified");
  if !property = "" then raise (Invalid_argument "No Property File Specified");
  let sources = parseFile !filename in
  let property = parseProperty !property in
  let (program, property) =
    ItoA.prog_itoa ~property:(!main,property) sources in
  let property =
    match property with
    | None -> raise (Invalid_argument "Unknown Property")
    | Some property -> property
  in
  if not !minimal then
    begin
      Format.fprintf !fmt "\nAbstract Syntax:\n";
      AbstractSyntax.prog_print !fmt program;
      Format.fprintf !fmt "\nProperty: ";
      AbstractSyntax.property_print !fmt property;
    end;
  let analysis_function =
    match !domain with
    | "boxes" -> if !ordinals then TupleRecurrenceBoxes.analyze else TupleRecurrenceBoxes.analyze
    | "octagons" -> if !ordinals then TupleRecurrenceOctagons.analyze else TupleRecurrenceOctagons.analyze
    | "polyhedra" -> if !ordinals then TupleRecurrencePolyhedra.analyze else TupleRecurrencePolyhedra.analyze
    | _ -> raise (Invalid_argument "Unknown Abstract Domain")
  in run_analysis (analysis_function property) program ()

let dt () =
  if !filename = "" then raise (Invalid_argument "No Source File Specified");
  let sources = parseFile !filename in
  let (program,_) = ItoA.prog_itoa sources in
  if not !minimal then
    begin
      Format.fprintf !fmt "\nAbstract Syntax:\n";
      AbstractSyntax.prog_print !fmt program;
    end;
  AbstractSyntax.StringMap.iter (fun key v -> Format.fprintf !fmt "%s" key; List.iter print_int v.featDom) !ItoA.features; 
  let analysis_function =
    match !domain with
    | "boxes" -> if !ordinals then DTreeTerminationBoxesOrdinals.analyze else DTreeTerminationBoxes.analyze
    | "octagons" -> if !ordinals then DTreeTerminationOctagonsOrdinals.analyze else DTreeTerminationOctagons.analyze
    | "polyhedra" -> if !ordinals then DTreeTerminationPolyhedraOrdinals.analyze else DTreeTerminationPolyhedra.analyze
    | _ -> raise (Invalid_argument "Unknown Abstract Domain") 
  in run_analysis analysis_function program () 


let sketch () =
  if !filename = "" then raise (Invalid_argument "No Source File Specified");
  let sources = parseFile !filename in
  let (program,_) = ItoA.prog_itoa sources in
  if not !minimal then
    begin
      Format.fprintf !fmt "\nAbstract Syntax:\n";
      AbstractSyntax.prog_print !fmt program;
    end;
  AbstractSyntax.StringMap.iter (fun key v -> Format.fprintf !fmt "%s" key; List.iter print_int v.featDom) !ItoA.features;
  let analysis_function =
    match !domain with
    | "boxes" -> if !ordinals then SketchTerminationBoxesOrdinals.analyze else SketchTerminationBoxes.analyze
    | "octagons" -> if !ordinals then SketchTerminationOctagonsOrdinals.analyze else SketchTerminationOctagons.analyze
    | "polyhedra" -> if !ordinals then SketchTerminationPolyhedraOrdinals.analyze else SketchTerminationPolyhedra.analyze
    | _ -> raise (Invalid_argument "Unknown Abstract Domain") 
  in run_analysis analysis_function program () 


let sketchtuple () =
  if !filename = "" then raise (Invalid_argument "No Source File Specified");
  let sources = parseFile !filename in
  let (program,_) = ItoA.prog_itoa sources in
  if not !minimal then
    begin
      Format.fprintf !fmt "\nAbstract Syntax:\n";
      AbstractSyntax.prog_print !fmt program;
    end;
  AbstractSyntax.StringMap.iter (fun key v -> Format.fprintf !fmt "%s" key; List.iter print_int v.featDom) !ItoA.features;
  let analysis_function =
    match !domain with
    | "boxes" -> if !ordinals then SketchTupleTerminationBoxesOrdinals.analyze else SketchTupleTerminationBoxes.analyze
    | "octagons" -> if !ordinals then SketchTupleTerminationOctagonsOrdinals.analyze else SketchTupleTerminationOctagons.analyze
    | "polyhedra" -> if !ordinals then SketchTupleTerminationPolyhedraOrdinals.analyze else SketchTupleTerminationPolyhedra.analyze
    | _ -> raise (Invalid_argument "Unknown Abstract Domain") 
  in run_analysis analysis_function program () 

(*Main entry point for application*)
let doit () =
  parse_args ();
  match !analysis with
  | "single" -> single ()
  | "single-guarantee" -> single_guar ()
  | "single-recurrence" -> single_recur ()  
  | "tuple" -> tuple () (*raise (Invalid_argument "Undefined Yet Analysis") *)
  | "tree-guarantee" -> istree:=true; tuple_guar () 
  | "tree-recurrence" -> istree:=true; tuple_recur ()   
  | "tuple-guarantee" -> tuple_guar ()  
  | "tuple-recurrence" -> tuple_recur ()    
  | "tree" -> dt()  (*raise (Invalid_argument "Undefined Yet Analysis")  *)
  | "sketch" -> sketch() 
  | "sketchtuple" -> sketchtuple()   
  | _ -> raise (Invalid_argument "Unknown Analysis")

let _ = doit () 
