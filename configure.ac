dnl tinmop: an humble mastodon client
dnl Copyright (C) 2020  cage

dnl This program is free software: you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation, either version 3 of the License, or
dnl (at your option) any later version.

dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.

dnl You should have received a copy of the GNU General Public License
dnl along with this program.
dnl If not, see [[http://www.gnu.org/licenses/][http://www.gnu.org/licenses/]].

AC_INIT([tinmop],[0.3.2],[https://notabug.org/cage/tinmop/],[tinmop])

AM_INIT_AUTOMAKE([-Wall foreign])

AM_GNU_GETTEXT([external])

AC_CHECK_LIB([idn2], [idn2_to_ascii_8z], [], AC_MSG_ERROR([Can not this function find libidn2.]))

AC_CHECK_LIB([ssl], [SSL_get_version], AC_MSG_ERROR([Can not find libssl.]))

AC_PATH_PROG([LISP_COMPILER],[sbcl],[no])

if test "$LISP_COMPILER" = "no" ; then
   AC_MSG_ERROR([Can not find SBCL, Common Lisp compiler.])
fi

AC_PATH_PROG([CURL],[curl],[no])

if test "$CURL" = "no" ; then
   AC_MSG_ERROR([Can not find curl.])
   exit 1;
fi

AC_PATH_PROG([GPG],[gpg],[no])

if test "$GPG" = "no" ; then
   AC_MSG_ERROR([Can not find gpg, crypto software.])
   exit 1;
fi

AC_PATH_PROG([BASH],[bash],[no])

if test "$BASH" = "no" ; then
   AC_MSG_ERROR([Can not find bash shell.])
   exit 1;
fi

AC_PATH_PROG([GAWK],[gawk],[no])

if test "$GAWK" = "no" ; then
   AC_MSG_ERROR([Can not find GNU AWK (gawk).])
   exit 1;
fi

AC_PATH_PROGS([XDG_OPEN],[xdg-open open],[no])

if test "$XDG_OPEN" = "no" ; then
   AC_MSG_ERROR([Can not find xdg-open.])
   exit 1;
fi

AC_PATH_PROGS([OPENSSL],[openssl],[no])

if test "$OPENSSL" = "no" ; then
   AC_MSG_ERROR([Can not find openssl binary.])
   exit 1;
fi

AC_PROG_MKDIR_P

dnl check sbcl version
SBCL_MIN_VERSION="1.5.9";
SBCL_VERSION=`${LISP_COMPILER} --version | ${GAWK} -- '// {print $2}'`
SBCL_VERSION_OK=`echo "$SBCL_VERSION $SBCL_MIN_VERSION" | awk -f compare_version.awk`

if test "$SBCL_VERSION_OK" = "1" ; then
   AC_MSG_ERROR([Lisp compiler too old, $SBCL_MIN_VERSION is the oldest supported.])
   exit 1;
fi

AC_CONFIG_FILES([Makefile quick_quicklisp.sh po/Makefile.in src/config.lisp.in])

AC_OUTPUT
