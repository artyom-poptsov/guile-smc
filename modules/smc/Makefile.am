## Makefile.am -- Config file for GNU Automake.
##
## Copyright (C) 2021-2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
##
## This file is part of Guile-SMC.
##
## Guile-SMC is free software: you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation, either version 3 of the
## License, or (at your option) any later version.
##
## Guile-SMC is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Guile-SMC.  If not, see <http://www.gnu.org/licenses/>.

include $(top_srcdir)/build-aux/am/guile.am

SUBDIRS = core context . compiler cli

SOURCES = \
	version.scm	\
	config.scm	\
	fsm.scm 	\
	puml-context.scm \
	puml-fsm.scm	\
	puml.scm	\
	trace-context.scm \
	trace-fsm.scm	\
	compiler.scm

EXTRA_DIST += \
	puml.puml	\
	trace.puml	\
	version.scm.in	\
	config.scm.in

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)/smc/
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/smc/

SUFFIXES = .in

puml-fsm.scm: puml.puml
	@echo "  SMC      $<"
	@cat $< | smc compile \
	    --load-path "$(abs_top_srcdir)/modules" \
	    --fsm-name "puml-fsm" \
	    --fsm-module "(smc puml-fsm)" \
	    --modules "((smc context char) (smc context oop char) (smc puml-context))" > $@

trace-fsm.scm: trace.puml
	@echo "  SMC      $<"
	@cat $< | smc compile \
		--load-path $(abs_top_srcdir)/modules \
		--fsm-name "trace-fsm" \
		--fsm-module "(smc trace-fsm)" \
		--modules "((smc trace-context) (smc context char) (smc context oop char))" > $@

root_moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)/smc/
config_substitute = sed -e 's,[@]MODDIR[@],$(root_moddir),g'

config.scm: config.scm.in
	@echo "  GEN      $@"
	@$(config_substitute) "$^" > "$@"

substitute = sed -e 's,[@]VERSION[@],$(PACKAGE_VERSION),g'

version.scm: version.scm.in
	@echo "  GEN      $@"
	@$(substitute) "$^" > "$@"

CLEANFILES += version.scm config.scm
