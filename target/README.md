# Usage

Build the programs using the following make command

```
make
```
Save the test input of the form of the tiger subset language into a file say `test.inp` and store it in a 
file say `test.out` run it using the command

```
./ec test.inp > test.out

```
Using SPIM, now read and run the `test.out` file
```
spim
read "test.out"
run
```