## Makefile.am -- Config file for GNU Automake.
##
## Copyright (C) 2021 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
	fsm.scm 	\
	puml-context.scm \
	puml-fsm.scm	\
	puml.scm	\
	compiler.scm

EXTRA_DIST += \
	puml.puml	\
	version.scm.in

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)/smc/
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/smc/

SUFFIXES = .in

puml-fsm.scm: puml.puml
	@echo "  SMC      $<"
	@cat $< | smc compile \
	    --fsm-name "puml-fsm" \
	    --fsm-module "(smc puml-fsm)" \
	    --modules "((smc context char-context)(smc puml-context))" > $@

substitute = sed -e 's,[@]VERSION[@],$(PACKAGE_VERSION),g'

version.scm: version.scm.in
	@echo "  GEN      $@"
	@$(substitute) "$^" > "$@"

CLEANFILES += version.scm
