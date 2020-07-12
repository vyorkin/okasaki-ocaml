# Frontend to dune.

default: exe

exe:
	dune exe bin/main.exe

test:
	dune exe ./test/okasaki.exe

utop:
	dune utop --

clean:
	dune clean
# Optionally, remove all files/folders ignored by git as defined
# in .gitignore (-X).
	git clean -dfXq

.PHONY: default exe test clean utop
