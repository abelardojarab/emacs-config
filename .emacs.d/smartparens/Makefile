EMACS ?= emacs
CASK ?= cask
ECUKES ?= ecukes
#$(shell find elpa/ecukes-*/ecukes | tail -1)

test: unit-tests #ecukes-features

unit-tests: elpa
	${CASK} exec ert-runner

ecukes-features: elpa
	${CASK} exec ${ECUKES} --no-win

elpa:
	mkdir -p elpa
	${CASK} install 2> elpa/install.log

clean-elpa:
	rm -rf elpa

clean-elc:
	rm -f *.elc test/*.elc

clean: clean-elpa clean-elc

print-deps:
	${EMACS} --version
	@echo CASK=${CASK}
	@echo ECUKES=${ECUKES}

travis-ci: print-deps test
