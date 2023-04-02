## Makefile.am -- Config file for GNU Automake.
##
## Copyright (C) 2021-2023 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

SOURCES = \
	config.scm	\
	common.scm	\
	log.scm		\
	transition.scm	\
	state.scm	\
	set.scm

EXTRA_DIST += \
	config.scm.in

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)/smc/core/
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/smc/core/

config_substitute = sed -e 's,[@]LOGGER_BINARY[@],$(LOGGER_BINARY),g'
config.scm: config.scm.in
	@echo "  GEN      $@"
	@$(config_substitute) "$^" > "$@"

# Makefile.am ends here

