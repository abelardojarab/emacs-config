# Toplevel Makefile
#
# (C) 2011, 2012 CEDET Developers
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Emacs; see the file COPYING.  If not, write to the
# Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.

PROJECTS=lisp/cedet lisp/speedbar lisp/cedet/cogre lisp/cedet/semantic \
lisp/cedet/ede lisp/cedet/srecode lisp/cedet/semantic/bovine lisp/cedet/semantic/wisent \
lisp/cedet/semantic/analyze lisp/cedet/semantic/decorate lisp/cedet/semantic/ectags \
lisp/cedet/semantic/symref doc/texi doc/texi/semantic

PROJECTS_AUTOLOADS=lisp/cedet lisp/speedbar lisp/cedet/cogre lisp/cedet/semantic \
lisp/cedet/ede lisp/cedet/srecode

EMACS=emacs
EMACSFLAGS=-batch --no-site-file -l cedet-remove-builtin.el
LOADDEFS=loaddefs.el
BOOTSTRAP=(progn (global-ede-mode) (find-file "$(CURDIR)/lisp/Project.ede") (ede-proj-regenerate) (find-file "$(CURDIR)/doc/texi/Project.ede") (ede-proj-regenerate))
UTEST=(progn (add-to-list (quote load-path) "$(CURDIR)/tests") (require (quote cedet-utests)) (semantic-mode))
GETVERSION=(princ (format "%s %s %s" emacs-major-version emacs-minor-version (bound-and-true-p emacs-repository-version)))
RM=rm
FIND=find
INSTALL-INFO=install-info
INFO_FILES=$(shell $(FIND) $(CURDIR)/doc/texi -type f -name '*.info')
INFODIR=$(CURDIR)/doc/info
EMACS_VERSION=$(shell $(EMACS) -Q --batch --eval '$(GETVERSION)')
EMACS_MAJOR=$(firstword $(EMACS_VERSION))
EMACS_MINOR=$(word 2,$(EMACS_VERSION))

ifeq ($(EMACS_MAJOR),23)
    $(error Aborting: CEDET needs Emacs 24.x)
endif

ifneq (,$(filter $(EMACS_MINOR),1 2))
    FALLBACK_EIEIO=1
endif

$(info Emacs version: $(EMACS_VERSION))

all: touch-makefiles clean-autoloads autoloads compile info install-info

ebuild:
	$(EMACS) -Q --batch -l cedet-build.el -f cedet-build

compile:
ifdef FALLBACK_EIEIO
	$(MAKE) -C etc/fallback-libraries/eieio -f Makefile.eieio
endif
	$(MAKE) -C lisp

makefiles: $(addsuffix /Makefile,$(PROJECTS))
$(addsuffix /Makefile,$(PROJECTS)): $(addsuffix /Project.ede,$(PROJECTS))
	@echo "Creating Makefiles using EDE."
	@$(EMACS) $(EMACSFLAGS) --eval '(setq cedet-bootstrap-in-progress t ede-project-directories t)' -f toggle-debug-on-error -l cedet-devel-load.el --eval '$(BOOTSTRAP)'

makefiles-bootstrap:
	@echo "Creating Makefiles using EDE and builtin Emacs-CEDET as fallback."
	@$(EMACS) -batch --no-site-file --eval '(setq cedet-bootstrap-in-progress t ede-project-directories t)' -l cedet-devel-load.el --eval '$(BOOTSTRAP)'

touch-makefiles:
	@echo Updating timestamps on all Makefiles.
	$(FIND) lisp -name "Makefile" -exec touch '{}' \;

autoloads:
	@echo "Generating autoloads."
	@$(foreach proj,$(PROJECTS_AUTOLOADS),cd $(CURDIR)/$(proj) && $(MAKE) autoloads;)

info:
	$(MAKE) -C doc -C texi

install-info:
	@echo Installing info files under $(INFODIR)
	@mkdir -p $(INFODIR)
	@$(foreach infofile,$(INFO_FILES),cp $(infofile) $(INFODIR);$(INSTALL-INFO) --info-dir=$(INFODIR) $(infofile);)

clean-autoloads:
	@echo Removing loaddefs.el files from subprojects.
	@$(foreach proj,$(PROJECTS_AUTOLOADS),cd $(CURDIR)/$(proj) && if [ -f $(LOADDEFS) ];then $(RM) -f $(LOADDEFS);fi;)

clean-all: clean-autoloads
	@echo Calling \"$(MAKE) clean\" in all projects.
	@$(foreach proj,$(PROJECTS),echo "  > $(proj)";cd $(CURDIR)/$(proj) && $(MAKE) clean;)
ifdef FALLBACK_EIEIO
	$(MAKE) -C etc/fallback-libraries/eieio -f Makefile.eieio clean
endif


utest: 
	$(EMACS) -Q -l cedet-devel-load.el --eval '$(UTEST)' -f cedet-utest

utest-batch:
	$(EMACS) $(EMACSFLAGS) -l cedet-devel-load.el --eval '$(UTEST)' -f cedet-utest-batch

itest: itest-make itest-automake itest-cpproot itest-javaroot

itest-make:
	cd $(CURDIR)/tests;./cit-test.sh Make

itest-automake:
	cd $(CURDIR)/tests;./cit-test.sh Automake

itest-cpproot:
	cd $(CURDIR)/tests;./cit-test.sh cpproot

itest-javaroot:
	cd $(CURDIR)/tests;./cit-test.sh javaroot

itest-android:
	cd $(CURDIR)/tests;./cit-test.sh Android

itest-arduino:
	cd $(CURDIR)/tests;./cit-test.sh Arduino

itest-globalref:
	cd $(CURDIR)/tests;./cit-test.sh globalref

itest-batch: itest-make-batch itest-automake-batch itest-cpproot-batch itest-javaroot-batch itest-android-batch itest-arduino-batch # itest-globalref-batch itest-stl-batch

itest-make-batch:
	cd $(CURDIR)/tests;./cit-test.sh Make --batch

itest-automake-batch:
	cd $(CURDIR)/tests;./cit-test.sh Automake --batch

itest-cpproot-batch:
	cd $(CURDIR)/tests;./cit-test.sh cpproot --batch

itest-javaroot-batch:
	cd $(CURDIR)/tests;./cit-test.sh javaroot --batch

itest-android-batch:
	cd $(CURDIR)/tests;./cit-test.sh Android --batch

itest-arduino-batch:
	cd $(CURDIR)/tests;./cit-test.sh Arduino --batch

itest-globalref-batch:
	cd $(CURDIR)/tests;./cit-test.sh globalref --batch

itest-stl-batch:
	$(EMACS) $(EMACSFLAGS) -l cedet-devel-load.el -l $(CURDIR)/tests/cedet/semantic/stltest.el

