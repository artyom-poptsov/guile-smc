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

include $(top_srcdir)/build-aux/am/guilec

EXTRA_DIST = \
	png-signature.puml		\
	png-signature-checker.in

SOURCES = \
	png-signature.scm		\
	png-signature-context.scm	\
	png-signature-checker.in

examplesdir = $(pkgdatadir)/examples
examples_pngdir = $(pkgdatadir)/examples/png
dist_examples_png_DATA = \
	png-signature.puml		\
	png-signature.scm		\
	png-signature-context.scm	\
	png-signature-checker.scm

SUFFIXES = .in .puml

substitute = sed -e 's,[@]GUILE[@],$(GUILE),g'
.in.scm:
	@echo "  GEN      $@"
	@$(MKDIR_P) "`dirname "$@"`"
	@$(substitute) "$^" > "$@.tmp" && 	\
	chmod +x "$@.tmp" &&			\
	mv "$@.tmp" "$@"

.puml.scm:
	@echo "  SMC      $<"
	@cat $< | $(abs_top_builddir)/pre-inst-env $(abs_top_srcdir)/utils/smc compile \
	--log-driver null \
	-L "$(top_srcdir)/modules:." \
	-U "((png-signature-context) (smc core log))" \
	--fsm-name ini-fsm \
	--fsm-module "(png-signature)" \
	--validate > $@

CLEANFILES = \
	png-signature.scm		\
	png-signature-checker.scm

# Makefile.am ends here

