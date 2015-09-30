EMACS ?= emacs
CASK ?= cask

all: test

test: clean-elc
	${MAKE} unit
	${MAKE} features
	${MAKE} compile
	${MAKE} unit
	${MAKE} clean-elc

unit:
	${CASK} exec ert-runner

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile pretty-mode.el

clean-elc:
	rm -f pretty-mode.elc

features:
	${CASK} exec ecukes --win

.PHONY: all test unit features
