# CS3140 Compiler Design Laboratory

Name : Joel Sam Mathew \
Roll No : 111901026 

## Directories
`reverse-polish/` -> contains reverse-polish lab files \
`src/` -> contains source language compiler files including Parser, Lexer, AST, Graph Structure, IR  \
`target/` -> contains files of MIPS, Temp Structure, Translate and Basic Block Structure.  \

## Usage

Build the programs using the following make command

```
make
```
Save the test input of the form of the tiger subset language into a file say `test.inp`. Run it using the command
and store the output in a file say `test.out` or run `make test` to do the same
```
./ec test.inp > test.out

```
Using SPIM, read and run the `test.out` file
```
spim
read "test.out"
run
```