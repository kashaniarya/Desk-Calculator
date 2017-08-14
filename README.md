# Desk-Calculator
A desk calculator coded in Ocaml. 

Arya Kashani
kashaniarya@gmail.com
UCSC ~ CMPS 112
asg2

Files Committed:
bigint.ml
maindc.ml
Makefile
mk.build
mk.test

Breif Summary:
A desk calculator in Ocaml, a language with strong static type checking. 
Program is a strict subset of dc(1), although it will not have all of its functions.
 dc is a reverse-polish desk calculator which supports unlimited precision arithmetic. 
 It also allows you to define and call macros. Normally dc reads from the standard input; 
 if any command arguments are given to it, they are filenames, and dc reads and executes the 
 contents of the files before reading from standard input. All normal output is to standard output; 
 all error output is to standard error.
 A reverse-polish calculator stores numbers on a stack. Entering a number pushes it on the stack. 
 Arithmetic operations pop arguments off the stack and push the results.
