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

SCM_TESTS = \
	log.scm		\
	transition.scm	\
	state.scm	\
	set.scm		\
	context-char.scm	\
	context-u8.scm			\
	context-common.scm		\
	context-oop-generic.scm	\
	context-oop-port.scm	\
	context-oop-char.scm	\
	context-oop-u8.scm	\
	context-functional-generic.scm	\
	context-functional-char.scm	\
	context-functional-u8.scm	\
	fsm.scm		\
	puml-context.scm	\
	puml.scm		\
	compiler.scm		\
	compiler-guile.scm	\
	compiler-guile-standalone.scm	\
	profiler.scm	\
	cli.scm	\
	cli-profile.scm

TESTS = ${SCM_TESTS}

TEST_EXTENSIONS = .scm

AM_TESTS_ENVIRONMENT = \
	abs_top_srcdir="$(abs_top_srcdir)"; export abs_top_srcdir; \
	abs_top_builddir="$(abs_top_builddir)"; export abs_top_builddir; \
	ORIGTERM=${TERM}; export ORIGTERM; \
	TERM=xterm; export TERM; \
	GUILE_AUTO_COMPILE=0; export GUILE_AUTO_COMPILE;

# LOG_COMPILER was introduced in Automake 1.12; don't expect "make
# check" or "make distcheck" to work with earlier versions.
SCM_LOG_COMPILER = \
	$(GUILE)

AM_SCM_LOG_FLAGS = \
	-L "$(abs_top_srcdir)/modules" \
	-s

EXTRA_DIST = \
	common.scm \
	test-context.scm \
	${SCM_TESTS}

CLEANFILES = \
	*.log

## Makefile.am ends here
