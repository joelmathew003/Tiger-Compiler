.globl main
main:
li $t0, 42
li $t1, 1
li $t2, 3
$L0:
bgt $t1, $t2, $L1
move $t3, $t1
move $a0, $t3
li $v0, 1
syscall
addi $t1, $t1, 1
j $L0
$L1:
move $t4, $t0
move $a0, $t4
li $v0, 1
syscall
li $v0, 10
syscall
