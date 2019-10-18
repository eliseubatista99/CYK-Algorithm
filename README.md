# CYK-Algorithm
## CYK Algorithm (Cocke, Younger and Kasami)

### Input:

In the first line is entered the word to be recognized. This word consists only of
alphabet character = {a,...z} and has a maximum length of 50 characters. It is assumed
that the set of nonterminals is N = {A, B,...S,...Z} in which stands out the symbol S which
we will always assume the initial symbol.

The second line presents the number m of production rules of the grammar considered.

The remaining m lines introduce the rules of production. Each of these productions has
the following format N -> a1 a2 a3...an with N is not terminal

### Output:

The output consists of two lines. The first containing:

The word "YES" if the word is generated by grammar.

The word "NO" if the word is not generated (recognized) by the grammar the second line contains the following information:

if the answer was positive, then alphabetically list all possible nonterminals
for the last cell, the top of the matrix (including S, of course);
otherwise, the designation (the integer) of the matrix line in which the algorithm was able to determine that
recognition failed. The number of (lines) follows the example provided

### Sample Input 1

abbabba

7

S -> S F

S -> a

A -> C C

A -> S S

A -> C S

C -> b

F -> A S

### Sample Output 1

YES

S

### Sample Input 2

aaabbabaaaabba

8

S -> S F

S -> a

A -> C G

A -> S S

A -> C S

C -> b

F -> A S

G -> C A

### Sample Output 2

NO

4
