## -----------------------------------------------------------------------------
## Erlang Compiler Flags
## -----------------------------------------------------------------------------
ERLCFLAGS += +debug_info
ERLCFLAGS += +warn_export_vars +warn_shadow_vars +warn_obsolete_guard
ERLCFLAGS += -pa ebin -I include

## -----------------------------------------------------------------------------
## Erlang Application Source Code
## -----------------------------------------------------------------------------
hello_srcdir = $(ERLANG_INSTALL_LIB_DIR_hello)/src

hello_src_DATA = \
   $(hello_headers) \
   $(hello_sources)

## Internal header files
hello_headers =

hello_sources = \
   src/hello_app.erl \
   src/hello_sup.erl \
   src/toppage_handler.erl

## -----------------------------------------------------------------------------
## Erlang Application Beam Code
## -----------------------------------------------------------------------------
hello_ebindir = $(ERLANG_INSTALL_LIB_DIR_hello)/ebin

hello_ebin_DATA = \
   ebin/hello.boot \
   ebin/hello.app \
   ebin/hello_app.beam \
   ebin/hello_sup.beam \
   ebin/toppage_handler.beam

ebin/%.beam: src/%.erl ebin
	$(ERLC) $(ERLCFLAGS) -pa ebin -I include -o ebin $<

## -----------------------------------------------------------------------------
## Public Header Files
## -----------------------------------------------------------------------------
hello_includedir = $(ERLANG_INSTALL_LIB_DIR_hello)/include

hello_include_DATA =

## -----------------------------------------------------------------------------
## Foreign Application Source Code
## -----------------------------------------------------------------------------
hello_privstaticdir = $(ERLANG_INSTALL_LIB_DIR_hello)/priv

hello_privstatic_DATA = \
   priv/index.html \
   priv/src/Hello.elm

priv/index.html: priv/src/Hello.elm
	cd $(@D) && $(ELM) make src/$(<F)

## -----------------------------------------------------------------------------
## Erlang Escripts
## -----------------------------------------------------------------------------
bin_SCRIPTS = \
   bin/hello-start.sh

## -----------------------------------------------------------------------------
## Tests
## -----------------------------------------------------------------------------
.PHONY: check
check: | static dynamic

.PHONY: static
static: | dialyzer

.PHONY: dynamic
dynamic: | common_test eunit_test

## -----------------------------------------------------------------------------
## Dialyzer
## -----------------------------------------------------------------------------
.PHONY: dialyzer
dialyzer: .hello.plt
	$(AM_V_at)$(DIALYZER) -Wno_behaviours --plts $< -- $(hello_sources) \
		$(ERLANG_LIB_DIR_ranch)/src/*.erl \
		$(ERLANG_LIB_DIR_cowlib)/src/*.erl \
		$(ERLANG_LIB_DIR_cowboy)/src/*.erl

.hello.plt:
	$(AM_V_at)$(DIALYZER) --build_plt --output_plt $@ --apps $(PLT_APPS)

PLT_APPS  = asn1 common_test compiler crypto debugger erts et ftp
PLT_APPS += hipe inets kernel mnesia observer public_key runtime_tools
PLT_APPS += sasl snmp ssh ssl stdlib syntax_tools tftp tools wx xmerl

## -----------------------------------------------------------------------------
## Common Tests
## -----------------------------------------------------------------------------
.PHONY: common_test
common_test:

## -----------------------------------------------------------------------------
## Eunit Tests
## -----------------------------------------------------------------------------
.PHONY: eunit_test
eunit_test:

## -----------------------------------------------------------------------------
## Documentation
## -----------------------------------------------------------------------------
.PHONY: docs
docs: | pdf html

# uninstall-local: uninstall-dist_pdfDATA uninstall-dist_htmlDATA

## -----------------------------------------------------------------------------
## PDF Documentation
## -----------------------------------------------------------------------------
pdf-local: $(dist_pdf_DATA)

install-pdf-local: install-dist_pdfDATA

dist_pdf_DATA =

## -----------------------------------------------------------------------------
## HTML Documentation
## -----------------------------------------------------------------------------
html-local: doc/html $(dist_html_DATA)

install-html-local: install-dist_htmlDATA

dist_html_DATA =

## -----------------------------------------------------------------------------
## Release
## -----------------------------------------------------------------------------
ebin/hello.boot: hello.rel
	$(ERL) -noshell -pa $(@D) -run systools make_script hello -s erlang halt
	mv $(@F) $(@D)

bin/hello-start.sh: bin
	echo '#!/bin/bash' > $@
	echo EXEC=\"exec erl -boot $(ERLANG_INSTALL_LIB_DIR_hello)/ebin/hello\" >> $@
	echo 'run_erl -daemon /tmp/ /tmp "$$EXEC"' >> $@

bin:
	$(MKDIR_P) $@

## -----------------------------------------------------------------------------
## Uninstall
## -----------------------------------------------------------------------------
uninstall-local:
	-rm -rf $(ERLANG_INSTALL_LIB_DIR_hello)

## -----------------------------------------------------------------------------
## Clean
## -----------------------------------------------------------------------------
MOSTLYCLEANFILES = \
   $(addsuffix .beam, $(test_modules)) \
   $(hello_ebin_DATA) \
   hello.script \
   hello.boot \
   priv/index.html

mostlyclean-local:
	rm -rf priv/elm-stuff bin

CLEANFILES = \
   dialyzer.plt

DISTCLEANFILES = \
   configure \
   Makefile.in \
   Makefile \
   aclocal.m4 \
   config.log \
   config.status \
   install-sh \
   missing \
   hello.rel

distclean-local:
	rm -rf autom4te.cache
