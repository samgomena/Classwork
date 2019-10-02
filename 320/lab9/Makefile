.SUFFIXES:
.SUFFIXES: .mini

.PHONY: all compiler clean

all:	clean compiler

compiler:
	javac MiniCompiler.java

.mini:
	java MiniCompiler $*

clean:
	-rm -f test?.s buggy?.s test? buggy?
	-rm -f MiniCompiler.class compiler/*.class lexer/*.class parser/*.class ast/*.class 

