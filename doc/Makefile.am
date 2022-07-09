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

AUTOMAKE_OPTIONS = gnu

info_TEXINFOS = guile-smc.texi

guile_smc_TEXINFOS = \
	intro.texi		\
	installation.texi	\
	api-core-log.texi	\
	api-core-stack.texi 	\
	api-core-state.texi 	\
	api-core-transition.texi 	\
	api-context.texi	\
	api-fsm.texi		\
	api-puml.texi		\
	api-compiler.texi	\
	api-version.texi	\
	cli.texi		\
	indices.texi		\
	fdl.texi

dist_man1_MANS =		\
	smc.1

## Silent rules.
AM_V_HELP2MAN = $(AM_V_HELP2MAN_$(V))
AM_V_HELP2MAN_ = $(AM_V_HELP2MAN_$(AM_DEFAULT_VERBOSITY))
AM_V_HELP2MAN_0 = @echo "  HELP2MAN" $@;
##
gen_man =							\
	LANGUAGE= $(top_builddir)/pre-inst-env $(HELP2MAN)	\
	$(HELP2MANFLAGS)

HELP2MANFLAGS = --source=GNU --info-page=$(PACKAGE_TARNAME)

smc.1: $(abs_top_builddir)/utils/smc
	-$(AM_V_HELP2MAN)$(gen_man) --output="$@" "$<"

CLEANFILES = \
	smc.1	\
	smc.log.1

## Makefile.am ends here
