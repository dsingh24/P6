###
# This Makefile can be used to make a parser for the CMM language
# (parser.class) and to make a program (P6.class) that tests the parser and
# the unparse methods in ast.java.
#
# make clean removes all generated files.
#
###

JC = javac
FLAGS = -g -cp $(CP)
CP = ~cs536-1/public/tools/deps:.

P6.class: P6.java parser.class Yylex.class ASTnode.class
	$(JC) $(FLAGS) -g P6.java

parser.class: parser.java ASTnode.class Yylex.class ErrMsg.class
	$(JC) $(FLAGS) parser.java

parser.java: CMM.cup
	java -cp $(CP) java_cup.Main < CMM.cup

Yylex.class: CMM.jlex.java sym.class ErrMsg.class
	$(JC) $(FLAGS) CMM.jlex.java

ASTnode.class: ast.java
	$(JC) $(FLAGS) -g ast.java

CMM.jlex.java: CMM.jlex sym.class
	java -cp $(CP) JLex.Main CMM.jlex

sym.class: sym.java
	$(JC) $(FLAGS) -g sym.java

sym.java: CMM.cup
	java -cp $(CP) java_cup.Main < CMM.cup

ErrMsg.class: ErrMsg.java
	$(JC) $(FLAGS) ErrMsg.java

Sym.class: Sym.java Type.java ast.java
	$(JC) -g Sym.java
	
SymTable.class: SymTable.java Sym.java DuplicateSymException.java EmptySymTableException.java
	$(JC) -g SymTable.java
	
Type.class: Type.java
	$(JC) -g Type.java

DuplicateSymException.class: DuplicateSymException.java
	$(JC) -g DuplicateSymException.java
	
EmptySymTableException.class: EmptySymTableException.java
	$(JC) -g EmptySymTableException.java

###
# clean
##

#
#
all: dotproduct encrypt fibinacci filter float_bench mult power var5 var4 var3 var2 var1 var0
clean:
	rm -f *~ *.class parser.java CMM.jlex.java sym.java

test:
	java -cp $(CP) P6 test.cf test.out

test2:
	java -cp $(CP) P6 test2.cf test2.out

testout:
	java -cp $(CP) P6 test.out test.out2


dotproduct:
	java -cp $(CP) P6 dotproduct.cf dotproduct.out

encrypt:
	java -cp $(CP) P6 encrypt.cf encrypt.out

fibinacci:
	java -cp $(CP) P6 fibinacci.cf fibinacci.out

filter:
	java -cp $(CP) P6 filter.cf filter.out

float_bench:
	java -cp $(CP) P6 float_bench.cf float_bench.out

mult:
	java -cp $(CP) P6 mult.cf mult.out
power:
	java -cp $(CP) P6 power.cf power.out
var5:
	java -cp $(CP) P6 var5.cf var5.out
var4:
	java -cp $(CP) P6 var4.cf var4.out
var3:
	java -cp $(CP) P6 var3.cf var3.out
var2:
	java -cp $(CP) P6 var2.cf var2.out
var1:
	java -cp $(CP) P6 var1.cf var1.out
var0:
	java -cp $(CP) P6 var0.cf var0.out
