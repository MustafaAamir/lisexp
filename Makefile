all:
	rlwrap --no-children -f completion dune exec lisp
test:
	dune test
