
This documents shows how to install the tool from the paper 
¨Lifted Termination Analysis and its Application to Program Sketching¨
if there is INTERNET CONNECTION 

###############################################################################
## SPLFunction --- Lifted Termination Analyzer 								 ## 
###############################################################################

SPLFunction is a research prototype lifted termination analyzer based on abstract interpretation 
designed for performing termination analysis of C program families. 



## Author

	Aleksandar Dimovski 
	
	
# Installation


The tool requires the following applications and libraries:

* OCaml 

	```
	(sudo) apt-get install ocaml-interp
	```

* Findlib

	```
	(sudo) apt-get install ocaml-findlib
	```

* Menhir: LR(1) parser generator

	```
	(sudo) apt-get install menhir
	```
  
* Opam: https://opam.ocaml.org/doc/Install.html

	```
	(sudo) apt-get install opam
	```
* Initialize OPAM state
	```
	opam init      		  % during initilization allow opam to modify ~/.profile
	```	
	```
	eval $(opam env)      % update the current shell environment
	```    
* OUnit

	```
	opam install ounit
	```

* APRON: numerical abstract domain library

	```
	opam install depext
	opam depext apron
	opam install apron
	```

* Zarith: arbitrary-precision integer operations

	```
	opam install zarith
	```
	
*  Set the Library Path variable in ~/.bashrc 
	```
	gedit ~/.bashrc
	```
Then, set the Library Path by appending at the end of the file:
	```
	LD_LIBRARY_PATH=/home/username/.opam/default/share/apron/lib     % first find the folder where apron/lib is located
	export LD_LIBRARY_PATH
	```
Log out of the current session, then log in and check:
	```
	echo $LD_LIBRARY_PATH
	```

# Compiling SPLFunction

Once all required libraries are installed, enter the folder 'DSPLNUM2Analyzer' and 'ocamlbuild' can be used to build lifted analyzer with the following command:

```
eval $(opam config env)                 % It will setup environment variables, that are necessary for the toolchain to work properly

ocamlbuild Main.native -use-ocamlfind -use-menhir -pkgs 'apron,gmp,oUnit,zarith' -I utils -I domains -I frontend -I cfgfrontend -I main -libs boxMPQ,octD,polkaMPQ,str,zarith
```

# Usage

The analyzer performs a forward reachability analysis of dynamic program families.

The following general command-line options are recognized:

	 -single 							set to perform single-program analysis
	 -tree								set to perform decision tree-based lifted analysis
	 -tuple								set to perform tuple-based lifted analysis
	 -sketch							set to perform decision tree-based sketching	 
	 -sketchtuple						set to perform tuple-based sketching
	 -domain boxes|octagons|polyhedra   set the abstract domain (defaults to boxes)
	 -minimal 							set to print only analysis result
	 -joinbwd 3                         set the widening delay in backward analysis


# Test example
Examples from "Motivating Example" section

enter the folder that contains the tool, and write

$ ./Main.native -tree -domain polyhedra tests/terminate/cav2006-cpp.c   	// to perform decision tree-based analysis using Polyhedra domain of cav2006-cpp.c
$ ./Main.native -tuple -domain polyhedra tests/terminate/cav2006-cpp.c  // to perform tuple-based analysis using Polyhedra domain of cav2006-cpp.c
$ ./Main.native -single -domain polyhedra tests/single/cav2006.c  // to perform single analysis using Polyhedra domain of cav2006.c, which is variability encoding of cav2006-cpp.c
$ ./Main.native -sketch -domain polyhedra tests/sketch/loop1-5.c  	// to perform decision tree-based sketching using Polyhedra domain of loop1-5.c
$ ./Main.native -sketchtuple -domain polyhedra tests/sketch/loop1-5.c  // to perform tuple-based sketching using Polyhedra domain of loop1-5.c

########################################################################################################

