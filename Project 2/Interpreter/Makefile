all: interpreter

FILES = Syntax.ml Printing.mli Printing.ml EvalUtil.mli EvalUtil.ml EvalEnv.mli EvalEnv.ml EvalSubst.mli EvalSubst.ml  Testing.ml Main.ml

# compile
interpreter: $(FILES)
	ocamlc -g -o interpreter $(FILES)

# run testing
test: interpreter
	./interpreter

clean: 
	rm -f interpreter *.cmi *.cmo
