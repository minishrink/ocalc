
# don't print commands unless running verbose mode
#ifndef VERBOSE
#.SILENT:
#endif

SRCDIR=src
TESTDIR=tests
BUILDDIR=_build/default

all:
	dune build $(SRCDIR)/calc.exe

calc:
	dune build $(SRCDIR)/calc.exe
	dune exec calc

test:
	dune build $(TESTDIR)/calc_test.exe
	dune exec calc_test

clean:
	dune clean

reindent:
	ocp-indent -i **/*.ml*

.PHONY: all calc test clean reindent
