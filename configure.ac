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

AC_INIT([tinmop],[0.9.9],[https://notabug.org/cage/tinmop/],[tinmop])

AM_INIT_AUTOMAKE([-Wall foreign])

dnl    checks for programs

AC_PATH_PROG([MSGMERGE],[msgmerge],[no])

if test "$MSGMERGE" = "no" ; then
   AC_MSG_ERROR([Can not find msgmerge, from the gettext package.])
fi

AC_PATH_PROG([XGETTEXT],[xgettext],[no])

if test "$XGETTEXT" = "no" ; then
   AC_MSG_ERROR([Can not find xgettext, from the gettext package.])
fi

AC_PATH_PROG([MSGFMT],[msgfmt],[no])

if test "$MSGFMT" = "no" ; then
   AC_MSG_ERROR([Can not find msgfmt, from the gettext package.])
fi

AM_GNU_GETTEXT([external])

AC_PATH_PROG([LISP_COMPILER],[sbcl],[no])

AC_PATH_PROG([LISP_COMPILER_ECL],[ecl],[no])

AC_ARG_WITH([ecl], [AS_HELP_STRING([--with-ecl], [Compile with Embedded Common Lisp compiler])], [LISP_COMPILER=$LISP_COMPILER_ECL], [])

if test "$LISP_COMPILER" = "no" ; then
   if test "$LISP_COMPILER_ECL" != "no" ; then
      AC_MSG_NOTICE([Unable to find SBCL but ECL seems installed on your system, try: "./reconfigure --with-ecl"])
   fi
   AC_MSG_ERROR([Unable to find a Common Lisp compiler.]);
fi

AC_MSG_NOTICE([Using $LISP_COMPILER as Common Lisp compiler])

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

AC_PATH_PROG([UNZIP],[unzip],[no])

if test "$UNZIP" = "no" ; then
   AC_MSG_WARN([Can not find unzip, genpub support will be disabled.])
fi

AC_PATH_PROG([MAN],[man],[no])

if test "$MAN" = "no" ; then
   AC_MSG_WARN([Can not find man.])
fi

dnl autoconf has an automatic test for AWK
if test -z "${AWK}"; then
   AC_MSG_ERROR([Can not find AWK.])
   exit 1
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

AC_PATH_PROGS([GIT],[git],[no])

if test "$GIT" = "no" ; then
   AC_MSG_ERROR([Can not find git executable.])
   exit 1;
fi

AC_PATH_PROGS([CHMOD],[chmod],[no])

if test "$CHMOD" = "no" ; then
   AC_MSG_ERROR([Can not find chmod executable.])
   exit 1;
fi

AC_PATH_PROGS([DIRNAME],[dirname],[no])

if test "$DIRNAME" = "no" ; then
   AC_MSG_ERROR([Can not find dirname executable.])
   exit 1;
fi

AC_PROG_MKDIR_P

dnl checks for libraries

AC_CHECK_LIB([ssl], [SSL_get_version], [], AC_MSG_ERROR([Can not find libssl.]))

AC_CHECK_LIB([ncurses], [initscr], [], AC_MSG_ERROR([Can not find ncurses.]))

AC_CHECK_LIB([sqlite3], [sqlite3_libversion], [], AC_MSG_ERROR([Can not find libsqlite3.]))

AC_CONFIG_FILES([Makefile quick_quicklisp.sh po/Makefile.in src/config.lisp.in])

AC_OUTPUT

${CHMOD} 750 quick_quicklisp.sh
