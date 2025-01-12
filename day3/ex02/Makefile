C = ocamlopt
TEST = test
FILES = calc.ml tests.ml

all : ${TEST}

${TEST}: ${FILES}
	${C} -o ${TEST} ${FILES}
clean:
	rm -f *.cmi *.cmx *.o

fclean: clean
	rm -f ${TEST}
