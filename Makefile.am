# tinmop: an humble mastodon client
# Copyright (C) 2020  cage

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.
# If not, see [[http://www.gnu.org/licenses/][http://www.gnu.org/licenses/]].

ACLOCAL_AMFLAGS      = -I m4

bin_SCRIPTS          = tinmop

CLEANFILES           = $(bin_SCRIPTS) $(CONF_PATH_FILE);

CONF_PATH_FILE       = src/config.lisp

CONF_PATH_FILE_IN    = src/config.lisp.in

CONF_PATH_FILE_IN_IN = src/config.lisp.in.in

BUILT_SOURCES        = $(CONF_PATH_FILE)

EXTRA_DIST           = config.rpath m4/ChangeLog  tinmop.asd README.org src \
LICENSES.org COPYING etc/shared.conf etc/default-theme.conf \
etc/init.lisp compare_version.awk

SUBDIRS              = po

dist_doc_DATA        = README.org README.txt LICENSES.org CONTRIBUTING.org \
doc/man.org doc/send-toot.lisp NEWS.org ChangeLog AUTHORS

confdir              = $(sysconfdir)/$(PACKAGE)

dist_conf_DATA       = etc/init.lisp etc/default-theme.conf etc/shared.conf

dist_pkgdata_DATA    = scripts/welcome-bot.lisp  \
scripts/delete-old-posts.lisp                    \
scripts/get-following.lisp                       \
modules/delete-by-regex.lisp                     \
modules/expand-abbrev-command-window.lisp        \
modules/next-previous-open.lisp                  \
modules/rewrite-message-urls.lisp                \
modules/share-gemini-link.lisp


dist_man1_MANS       = doc/tinmop.man

$(PACKAGE): $(CONF_PATH_FILE) *.asd src/*
	$(LISP_COMPILER) \
	--eval "(asdf:load-system '$(PACKAGE))" \
	--eval "(in-package main)" \
	--eval "(sb-ext:save-lisp-and-die \"$(PACKAGE)\" :toplevel 'main::main :executable t :purify t :save-runtime-options t)"

$(CONF_PATH_FILE):
	grep "^;" $(CONF_PATH_FILE_IN)                          >  $(CONF_PATH_FILE)
	echo -e "(in-package :config)\n"                        >> $(CONF_PATH_FILE);
	echo "(alexandria:define-constant +sys-data-dir+"       >> $(CONF_PATH_FILE);
	echo -e "\"$(pkgdatadir)\" :test #'string=)\n"          >> $(CONF_PATH_FILE);

	echo "(alexandria:define-constant +sys-conf-dir+"       >> $(CONF_PATH_FILE);
	echo -e  "\"$(confdir)\" :test #'string=)\n"            >> $(CONF_PATH_FILE);

	echo "(alexandria:define-constant +catalog-dir+"        >> $(CONF_PATH_FILE);
	echo -e "\""$(localedir)"\" :test #'string=)\n"         >> $(CONF_PATH_FILE);

	echo "(alexandria:define-constant +text-domain+"        >> $(CONF_PATH_FILE);
	echo -e "\""$(PACKAGE)"\" :test #'string=)\n"           >> $(CONF_PATH_FILE);

	echo "(alexandria:define-constant +program-name+"       >> $(CONF_PATH_FILE);
	echo -e "\""$(PACKAGE)"\" :test #'string=)\n"           >> $(CONF_PATH_FILE);

	echo "(alexandria:define-constant +program-version+"    >> $(CONF_PATH_FILE);
	echo -e "\""$(VERSION)"\" :test #'string=)\n"           >> $(CONF_PATH_FILE);

	echo "(alexandria:define-constant +issue-tracker+"      >> $(CONF_PATH_FILE);
	echo -e "\""$(PACKAGE_BUGREPORT)"\" :test #'string=)\n" >> $(CONF_PATH_FILE);
	cat  $(CONF_PATH_FILE).in | sed "\/^;;.*$\/d"           >> $(CONF_PATH_FILE);

dist-hook:
	rm -fv $(top_distdir)/$(CONF_PATH_FILE)
	rm -fv $(top_distdir)/$(CONF_PATH_FILE_IN)
