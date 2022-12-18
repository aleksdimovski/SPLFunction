

###############################################################################
## SPLFunction2 --- Lifted CTL Temporal Analyzer 								 ## 
###############################################################################

SPLFunction2 is a research prototype lifted analyzer based on abstract interpretation 
designed for performing CTL temporal analysis of C program families. 



## Author

	Aleksandar Dimovski 
	
	
# Installation


Download and untar the file SPLfunction2.tar.gz



# Compiling SPLFunction

Once all required libraries are installed, enter the folder 'SPLfunction2' and 'ocamlbuild' can be used to build lifted analyzer with the following command:

```
eval $(opam config env)                 % It will setup environment variables, that are necessary for the toolchain to work properly

ocamlbuild Main.native -use-ocamlfind -use-menhir -pkgs 'apron,gmp,oUnit,zarith' -I utils -I domains -I frontend -I cfgfrontend -I main -libs boxMPQ,octD,polkaMPQ,str,zarith
```

# Usage

The analyzer performs a forward reachability analysis of dynamic program families.

The following general command-line options are recognized:

	 -single 							set to perform single-program analysis
	 -tree								set to perform decision tree-based lifted analysis
	 -tuple							set to perform tuple-based lifted analysis
	 -tree-guarantee						set to perform decision tree-based guarantee lifted analysis	 
	 -tree-recurrence						set to perform decision tree-based recurrence lift analysis	 
	 -tuple-guarantee						set to perform tuple-based guarantee lifted analysis	 
	 -tuple-recurrence						set to perform tuple-based recurrence lifted analysis
	 -domain boxes|octagons|polyhedra   set the abstract domain (defaults to boxes)
	 -minimal 							set to print only analysis result
	 -joinbwd 3                         set the widening delay in backward analysis

#############################################################################################################################
# BENCHAMRKS
#############################################################################################################################

Enter the folder that contains the tool, and write

$ ./Main.native -tuple -domain polyhedra tests/ctl/test1.c    [ALL]
$ ./Main.native -tree -domain polyhedra tests/ctl/test1.c	[DT]

$ ./Main.native -tuple-guarantee tests/ctl/property4.ctl -domain polyhedra tests/ctl/test2.c  [ALL]
$ ./Main.native -tree-guarantee tests/ctl/property4.ctl -domain polyhedra tests/ctl/test2.c  [DT]

$ ./Main.native -tuple-guarantee tests/ctl/property1.ctl -domain polyhedra tests/ctl/liveness.c   [ALL]
$ ./Main.native -tree-guarantee tests/ctl/property1.ctl -domain polyhedra tests/ctl/liveness.c   [DT]

$ ./Main.native -tuple-recurrence tests/ctl/property1.ctl -domain polyhedra tests/ctl/liveness.c   [ALL]
$ ./Main.native -tree-recurrence tests/ctl/property1.ctl -domain polyhedra tests/ctl/liveness.c   [DT]

$ ./Main.native -tuple-recurrence tests/ctl/property3.ctl -domain polyhedra tests/ctl/sink.c   [ALL]
$ ./Main.native -tree-recurrence tests/ctl/property3.ctl -domain polyhedra tests/ctl/sink.c   [DT]

$ ./Main.native -tuple -domain polyhedra tests/ctl/PastaB2.c   [ALL]
$ ./Main.native -tree -domain polyhedra tests/ctl/PastaB2.c    [DT]

########################################################################################################

