dnl   configuration script for Guile-SMC
dnl   Process this file with autoconf to produce configure.
dnl

define(GUILE_SMC_CONFIGURE_COPYRIGHT, [[
Copyright (C) 2021-2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>

This file is part of Guile-SMC.

Guile-SMC is free software: you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

Guile-SMC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Guile-SMC.  If not, see <http://www.gnu.org/licenses/>.
]])

AC_INIT([Guile-SMC], [0.5.0], [poptsov.artyom@gmail.com],
                     [guile-smc],
                     [https://github.com/artyom-poptsov/guile-smc])

AC_COPYRIGHT(GUILE_SMC_CONFIGURE_COPYRIGHT)
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([build-aux/m4])

AM_INIT_AUTOMAKE([gnu silent-rules -Wall -Wno-portability
                      color-tests])

m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])], [AC_SUBST([AM_DEFAULT_VERBOSITY],1)])

AC_ARG_WITH([guilesitedir],
            [AS_HELP_STRING([--with-guilesitedir],
                            [use the specified installation path for Guile modules])],
            [case "x$withval" in
             xyes|xno) guilesitedir="";;
             *)        guilesitedir="$withval";;
             esac],
             [guilesitedir=""])

GUILE_PKG([3.0.4 3.0 2.2])
GUILE_PROGS
GUILE_SITE_DIR

if test "x$guilesitedir" = "x"; then
   guilesitedir="$datadir/guile/site/$GUILE_EFFECTIVE_VERSION"
fi
AC_SUBST([guilesitedir])

AC_CONFIG_FILES([
        Makefile
        build-aux/am/Makefile
        build-aux/Makefile
        build-aux/m4/Makefile
        examples/png/Makefile
        modules/smc/Makefile
        modules/smc/core/Makefile
        modules/smc/compiler/Makefile
        modules/smc/context/Makefile
        modules/smc/cli/Makefile
        utils/Makefile
        doc/Makefile
        tests/Makefile
])

dnl Manual pages.
AM_MISSING_PROG([HELP2MAN], [help2man])

dnl texinfo package required to make the documentation in Texinfo format.
AC_CHECK_PROG(MAKEINFO_CHECK, makeinfo, yes)
AS_IF([test x"$MAKEINFO_CHECK" != x"yes"],
            [ AC_MSG_ERROR([Please install texinfo before configuring])])

AC_CONFIG_FILES([pre-inst-env:build-aux/pre-inst-env.in],
        [chmod +x pre-inst-env])

# Generate a Makefile, based on the results.
AC_OUTPUT

if test "$guilesitedir" != "$GUILE_SITE"; then
   # Guile has a different prefix than this module
   AC_MSG_WARN([]
   [The Guile modules will be installed in ${guilesitedir}.]
   [You should probably re-run `configure' with]
   [`--with-guilesitedir=$GUILE_SITE']
   [Otherwise, you will have to adjust the `GUILE_LOAD_PATH' environment]
   [variable.])
fi
