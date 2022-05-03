# Summary

Name : Joel Sam Mathew \
Roll No : 111901026 

## Reverse Polish

- Extended functionality of the given existing reverse polish compiler to include division and parenthesis.
- Done by changing abstract syntax by making changes to the `ast.sml`
- `grm` and `lex` files were also changed to add the syntax of the added functionalities.

## Compiling to MIPS: Lexing and Parsing

- Started off by capturing the abstract syntax tree of the MIPS assembly language from the SPIM documentation.
- This included the assembly language labels, instructions, registers and functions to pretty print the same.
- Parser and Lexer of Tiger AST was made referencing the reverse polish parser and lexer and necassary changes were made to capture the syntax of Tiger.


## IR code generation and MIPS assembly program generations

- Translation of the source language to the IR to the MIPS assembly language done through the `translate.sml`
- This makes use of a greedy register allocation method, which is considered as "temps" in the IR.
- Basic functionalities of the Tiger language like variables, constants, for loops etc. have been implemented.
- Used an environment to store variables as temporary registers.
- Created Basic block functionality as a functor.
- Added graph.sml for future use (allocating registers)
- Pretty printing to see the usage of temporary registers

## TODO

- Register Allocation using graph colouring (removing inefficient greedy method)
- Canonization of Tree IR
- Control Flow visualization with basic blocks
- More complex features of Tiger to be implemented

