## Makefile.am -- Config file for GNU Automake.
##
## Copyright (C) 2023 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

include $(top_srcdir)/build-aux/am/guilec

EXTRA_DIST = \
	fsm.puml		\
	main.in			\
	context.scm

SOURCES = \
	fsm.scm			\
	context.scm		\
	custom-fsm.scm		\
	main.scm		\
	main.in

examplesdir = $(pkgdatadir)/examples
examples_parenthesis_checkdir = $(pkgdatadir)/examples/parenthesis-check/
examples_parenthesis_check_DATA = \
	fsm.puml		\
	custom-fsm.scm		\
	main.scm

SUFFIXES = .in .puml

substitute = sed -e 's,[@]GUILE[@],$(GUILE),g'
.in.scm:
	@echo "  GEN      $@"
	@$(MKDIR_P) "`dirname "$@"`"
	@$(substitute) "$^" > "$@.tmp" && 	\
	chmod +x "$@.tmp" &&			\
	mv "$@.tmp" "$@"

custom-fsm.scm: fsm.puml
	@echo "  SMC      $<"
	GUILE_AUTO_COMPILE=0; export GUILE_AUTO_COMPILE;
	$(abs_top_builddir)/pre-inst-env $(abs_top_builddir)/utils/smc compile \
	--log-driver null \
	-L "$(abs_top_srcdir)/modules:$(abs_top_srcdir)/examples/parenthesis-check/" \
	-U "((context))" \
	--guile-smc-path $(abs_top_srcdir)/modules/ \
	--target guile-standalone \
	--fsm-name custom-fsm \
	--fsm-module "(custom-fsm)" \
	--validate \
	$< > $@

CLEANFILES = \
	custom-fsm.scm		\
	main.scm

# Makefile.am ends here

