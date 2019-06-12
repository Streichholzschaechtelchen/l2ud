all: main.native

main.native: *.ml*
	ocamlbuild -libs str,unix -use-menhir $@

gl:
	ln -s main.native $@

