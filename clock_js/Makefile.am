## -----------------------------------------------------------------------------
## Erlang Compiler Flags
## -----------------------------------------------------------------------------
ERLCFLAGS += +debug_info
ERLCFLAGS += +warn_export_vars +warn_shadow_vars +warn_obsolete_guard
ERLCFLAGS += -pa ebin -I include

## -----------------------------------------------------------------------------
## Erlang Application Source Code
## -----------------------------------------------------------------------------
clock_js_srcdir = $(ERLANG_INSTALL_LIB_DIR_clock_js)/src

clock_js_src_DATA = \
   $(clock_js_headers) \
   $(clock_js_sources)

## Internal header files
clock_js_headers =

clock_js_sources = \
   src/clock_js_app.erl \
   src/clock_js_sup.erl \
   src/toppage_handler.erl

## -----------------------------------------------------------------------------
## Erlang Application Beam Code
## -----------------------------------------------------------------------------
clock_js_ebindir = $(ERLANG_INSTALL_LIB_DIR_clock_js)/ebin

clock_js_ebin_DATA = \
   ebin/clock_js.boot \
   ebin/clock_js.app \
   ebin/clock_js_app.beam \
   ebin/clock_js_sup.beam \
   ebin/toppage_handler.beam

ebin/%.beam: src/%.erl ebin
	$(ERLC) $(ERLCFLAGS) -pa ebin -I include -o ebin $<

## -----------------------------------------------------------------------------
## Public Header Files
## -----------------------------------------------------------------------------
clock_js_includedir = $(ERLANG_INSTALL_LIB_DIR_clock_js)/include

clock_js_include_DATA =

## -----------------------------------------------------------------------------
## Foreign Application Source Code
## -----------------------------------------------------------------------------
clock_js_privstaticdir = $(ERLANG_INSTALL_LIB_DIR_clock_js)/priv

clock_js_privstatic_DATA = \
   priv/index.html \
   priv/main.js \
   priv/src/Main.elm

priv/main.js: priv/src/Main.elm
	cd $(@D) && $(ELM) make src/$(<F) --output=$(@F)

## -----------------------------------------------------------------------------
## Erlang Escripts
## -----------------------------------------------------------------------------
bin_SCRIPTS = \
   bin/clock-js-start.sh

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
dialyzer: .clock_js.plt
	$(AM_V_at)$(DIALYZER) -Wno_behaviours --plts $< -- $(clock_js_sources) \
		$(ERLANG_LIB_DIR_ranch)/src/*.erl \
		$(ERLANG_LIB_DIR_cowlib)/src/*.erl \
		$(ERLANG_LIB_DIR_cowboy)/src/*.erl

.clock_js.plt:
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
ebin/clock_js.boot: clock_js.rel
	$(ERL) -noshell -pa $(@D) -run systools make_script clock_js -s erlang halt
	mv $(@F) $(@D)

bin/clock-js-start.sh: bin
	echo '#!/bin/bash' > $@
	echo EXEC=\"exec erl -boot $(ERLANG_INSTALL_LIB_DIR_clock_js)/ebin/clock_js\" >> $@
	echo 'run_erl -daemon /tmp/ /tmp "$$EXEC"' >> $@

bin:
	$(MKDIR_P) $@

## -----------------------------------------------------------------------------
## Uninstall
## -----------------------------------------------------------------------------
uninstall-local:
	-rm -rf $(ERLANG_INSTALL_LIB_DIR_clock_js)

## -----------------------------------------------------------------------------
## Clean
## -----------------------------------------------------------------------------
MOSTLYCLEANFILES = \
   $(addsuffix .beam, $(test_modules)) \
   $(clock_js_ebin_DATA) \
   clock_js.script \
   clock_js.boot \
   main.js

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
   clock_js.rel

distclean-local:
	rm -rf autom4te.cache
