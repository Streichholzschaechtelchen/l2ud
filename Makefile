all: main.native

main.native: *.ml*
	ocamlbuild -use-menhir $@

gl:
	ln -s main.native $@

