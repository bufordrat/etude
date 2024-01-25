# etude                             -*- makefile -*-
# GNUmakefile
# Matt Teichman <https://elucidations.vercel.app>

OPENHTML = emacsclient -n -a "emacs --no-desktop" --eval '(eww-open-file "$1")'
LIB = makefiles
SUBCLEANS =
DISPLAY = short
DUNE = dune $1 --display $(DISPLAY)

NAME = etude
DESCRIPTION = does something
# temporary kludge
PACKAGES = $(shell sed -nf makefiles/getlibs.sed dune)
LIBS = $(NAME)

include $(LIB)/Makefile.gnumake
include $(LIB)/Makefile.debug
include $(LIB)/Makefile.version

.DEFAULT_GOAL := build

all build::				## build the project binaries
	$(call DUNE, build @@default)
.PHONY: build

check test tests runtest::	## run the test suite
	$(call DUNE, runtest)
.PHONY: check test tests runtest

META: GNUmakefile
	( \
	  echo '# THIS FILE IS AUTOMAGICALLY GENERATED, DO NOT EDIT'; \
	  echo 'description = "$(DESCRIPTION)"'; \
	  echo 'archive(byte) = "$(NAME).cma"'; \
	  echo 'archive(byte, plugin) = "$(NAME).cma"'; \
	  echo 'archive(native) = "$(NAME).cmxa"'; \
	  echo 'archive(native, plugin) = "$(NAME).cmxs"'; \
	  echo 'exists_if = "$(NAME).cma"'; \
	  echo 'version = "$(call VERSION)"'; \
	  echo 'requires ="$(PACKAGES)"'; \
	) > $@

install: install-libs
.PHONY: install

install-libs: META all uninstall-libs
ifneq ($(LIBS),)
ifeq ($(word 2,$(LIBS)),)
	ocamlfind install $(basename $(LIBS)) META $(wildcard _build/default/*.a) \
	  $(wildcard _build/default/*.cm*) \
	  $(wildcard _build/*.mli) \
	  $(wildcard _build/default/.httpr_intf.objs/byte/*) \
	  $(wildcard _build/default/.httpr_intf.objs/native/*)
	@echo
	@echo if you are me, you might want to do:
	@echo make push
else
	$(error "don't know how to install multiple libraries")
endif
endif
.PHONY: install-libs

uninstall-libs:
	for lib in $(LIBS); do ocamlfind remove $$lib; done
.PHONY: uninstall-libs

reinstall-libs::
	$(MAKE) install-libs FORCE=1
.PHONY: reinstall-libs

doc::				## build documentation
	$(call DUNE, build @doc)
.PHONY: doc

# this path is bogus but _build/default/_doc/_html/index.html is always empty
read-doc: doc			## open the documentation with $(OPENHTML)
	$(call OPENHTML, $(wildcard _build/default/_doc/_html/*/*/index.html))

clean: $(SUBCLEANS)		## clean up build artifacts
	$(call DUNE, clean)
	$(RM) META $(NAME).opam
.PHONY: clean

ODOC_WEB = staff:/data/web/dldc/open/etude/
.PHONY: install-doc
install-doc: doc
	rsync -av ~/Code/GitHub/etude/_build/default/_doc/_html/ $(ODOC_WEB)

-include $(LIB)/Makefile.help
