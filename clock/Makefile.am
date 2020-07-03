## -----------------------------------------------------------------------------
## Erlang Compiler Flags
## -----------------------------------------------------------------------------
ERLCFLAGS += +debug_info
ERLCFLAGS += +warn_export_vars +warn_shadow_vars +warn_obsolete_guard
ERLCFLAGS += -pa ebin -I include

## -----------------------------------------------------------------------------
## Erlang Application Source Code
## -----------------------------------------------------------------------------
clock_srcdir = $(ERLANG_INSTALL_LIB_DIR_clock)/src

clock_src_DATA = \
   $(clock_headers) \
   $(clock_sources)

## Internal header files
clock_headers =

clock_sources = \
   src/clock_app.erl \
   src/clock_sup.erl \
   src/toppage_handler.erl

## -----------------------------------------------------------------------------
## Erlang Application Beam Code
## -----------------------------------------------------------------------------
clock_ebindir = $(ERLANG_INSTALL_LIB_DIR_clock)/ebin

clock_ebin_DATA = \
   ebin/clock.boot \
   ebin/clock.app \
   ebin/clock_app.beam \
   ebin/clock_sup.beam \
   ebin/toppage_handler.beam

ebin/%.beam: src/%.erl ebin
	$(ERLC) $(ERLCFLAGS) -pa ebin -I include -o ebin $<

## -----------------------------------------------------------------------------
## Public Header Files
## -----------------------------------------------------------------------------
clock_includedir = $(ERLANG_INSTALL_LIB_DIR_clock)/include

clock_include_DATA =

## -----------------------------------------------------------------------------
## Foreign Application Source Code
## -----------------------------------------------------------------------------
clock_privstaticdir = $(ERLANG_INSTALL_LIB_DIR_clock)/priv

clock_privstatic_DATA = \
   priv/index.html \
   priv/src/Clock.elm

priv/index.html: priv/src/Clock.elm
	cd $(@D) && $(ELM) make src/$(<F)

## -----------------------------------------------------------------------------
## Erlang Escripts
## -----------------------------------------------------------------------------
bin_SCRIPTS = \
   bin/clock-start.sh

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
dialyzer: .clock.plt
	$(AM_V_at)$(DIALYZER) -Wno_behaviours --plts $< -- $(clock_sources) \
		$(ERLANG_LIB_DIR_ranch)/src/*.erl \
		$(ERLANG_LIB_DIR_cowlib)/src/*.erl \
		$(ERLANG_LIB_DIR_cowboy)/src/*.erl

.clock.plt:
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
ebin/clock.boot: clock.rel
	$(ERL) -noshell -pa $(@D) -run systools make_script clock -s erlang halt
	mv $(@F) $(@D)

bin/clock-start.sh: bin
	echo '#!/bin/bash' > $@
	echo EXEC=\"exec erl -boot $(ERLANG_INSTALL_LIB_DIR_clock)/ebin/clock\" >> $@
	echo 'run_erl -daemon /tmp/ /tmp "$$EXEC"' >> $@

bin:
	$(MKDIR_P) $@

## -----------------------------------------------------------------------------
## Uninstall
## -----------------------------------------------------------------------------
uninstall-local:
	-rm -rf $(ERLANG_INSTALL_LIB_DIR_clock)

## -----------------------------------------------------------------------------
## Clean
## -----------------------------------------------------------------------------
MOSTLYCLEANFILES = \
   $(addsuffix .beam, $(test_modules)) \
   $(clock_ebin_DATA) \
   clock.script \
   clock.boot \
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
   clock.rel

distclean-local:
	rm -rf autom4te.cache
