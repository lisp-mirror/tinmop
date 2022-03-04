                                ━━━━━━━━
                                 TINMOP
                                ━━━━━━━━


Table of Contents
─────────────────

1. Introduction
2. Peculiar Features
.. 1. Important note
3. Dependency
.. 1. Programs
.. 2. Lisp Libraries
.. 3. Foreign (C language) library
4. Install
.. 1. Using guix
.. 2. From source
5. Usage
6. BUGS
7. Translations
8. License
9. Privacy
10. Contributing
11. FAQ
12. NO WARRANTY
13. Acknowledgment





1 Introduction
══════════════

  Tinmop is an opinionated client for [Pleroma] (using the mastodon API)
  and [gemini protocol].  It offer a distraction free terminal
  interface.

  The name is a recursive acronym: "Tinmop Is Not Mutt or Pine".  The
  older of us can remember that, in turn, Pine is sometimes considered
  an acronym as well: "Pine Is Not Elm" and, finally, Elm means
  (according to Wikipedia): "Electronic Mail".


[Pleroma] <https://pleroma.social/>

[gemini protocol] <https://gemini.circumlunar.space/>


2 Peculiar Features
═══════════════════

  • tree structure of messages;
  • subscriptions of hashtag;
  • programmable and extensible;
  • encrypted direct message (but see 11);
  • no mentions notification, no knowledge of when or who favourited
    your status;
  • No blocking or muting, the client can be configured to ignore a list
    of accounts.


2.1 Important note
──────────────────

  This is alpha quality software, many things are broken, some feature
  are missing some works with unintended/unexpected side effects; this
  software is for testing only and remember that there is [NO WARRANTY].


[NO WARRANTY] See section 12


3 Dependency
════════════

3.1 Programs
────────────

  ⁃ to build the package: xgettext.

  ⁃ to install the package, including running the script to install lisp
    libraries (`quick_quicklisp.sh'):
    • AWK;
    • git.

  ⁃ to run the program:
    • SBCL compiler;
    • xdg-open;
    • openssl;
    • unzip (command line utitlity to decompress zip file);
    • your favourite editor (default: nano).

  All the dependencies above should be available using the package
  manager of your distribution (`apt', `yum' etc).


3.2 Lisp Libraries
──────────────────

  • alexandria;
  • cl-ppcre-unicode;
  • tooter;
  • croatoan;
  • osicat;
  • cl-spark;
  • access;
  • sqlite;
  • sxql;
  • sxql-composer;
  • marshal;
  • bordeaux-threads;
  • log4cl;
  • local-time;
  • cl-colors2;
  • cl-i18n;
  • clunit2;
  • esrap;
  • ieee-floats;
  • parse-number;
  • cl-html5-parser;
  • unix-opts;
  • crypto-shortcuts;
  • drakma;
  • usocket;
  • cffi;
  • babel;
  • percent-encoding;
  • trivial-clipboard.

  All these libraries will be downloaded, compiled and installed by the
  script `quick_quicklisp.sh', see below.


3.3 Foreign (C language) library
────────────────────────────────

  • libssl (TLS socket)

    The C library should be installed with their header (`*.h') files.
    On Debian or derived system this means installing the library
    package with the `-dev' suffix as shown below:

    ┌────
    │ # apt-get install libssl-dev
    └────


4 Install
═════════

4.1 Using guix
──────────────

  Using [guix] is the simpler way to install this software:

  ┌────
  │ $ guix pull && guix package -u && guix install tinmop
  └────

  Guix can be installed on debian (testing or sid):

  ┌────
  │ $ apt-get install guix
  └────

  or using a shell script [as explained in the guix manual].


[guix] <https://guix.gnu.org/>

[as explained in the guix manual]
<https://guix.gnu.org/manual/en/guix.html#Binary-Installation>


4.2 From source
───────────────

  1. optional step needed only if you have not already the configure
     script, you will need `autotools' for that.

     ┌────
     │ $ autoreconf -fiv
     └────

  2. run `configure' and resolve the missing dependencies (if any)

     ┌────
     │ $ ./configure
     └────

  3. the script `quick-quicklisp.sh' will download and install the
     library manager and the libraries on your home dir.

     ┌────
     │ $ ./quick_quicklisp.sh
     └────

     This step is optional if you have already installed quicklisp, in
     this case just load the [dependencies] using the client installed
     on your computer.

  4. optional step if you did not ran `quick-quicklisp.sh'.

     Clone in `$HOME/quicklisp/local-projects/' the latest version of
     croatoan, a library to wrap the ncurses TUI library.

     This step is temporary as this version will get into quicklisp
     eventually.
     ┌────
     │ $ cd $HOME/quicklisp/local-projects/
     │ $ git clone https://github.com/McParen/croatoan.git
     └────

  5. build the executable:

     ┌────
     │ $ make
     └────

  6. install on your system:

     ┌────
     │ # make install
     └────

  7. take a look the manpage:

     ┌────
     │ $ man tinmop
     └────

  8. run the software!

     ┌────
     │ $ tinmop
     └────

  9. An error will be printed about a missing file place a simple
     configuration file in one of the directory the software indicated.
     See [the FAQ below].


[dependencies] See section 3

[the FAQ below] See section 11


5 Usage
═══════

  See the command line options:

  ┌────
  │ $ tinmop -h
  └────

  To get instruction about configuration:

  ┌────
  │ $ man tinmop
  └────


6 BUGS
══════

  Please file bug reports on the [notabug repository].


[notabug repository] <https://notabug.org/cage/tinmop/>


7 Translations
══════════════

  Only Italian translation is regularly updated.


8 License
═════════

  This program is released under GNU General Public license version 3 or
  later (see COPYING file).

  The program use data and code from other sources, please see
  LICENSES.org for credits.

  Although any efforts has been put to make the list of credits
  exhaustive, errors are always possible.  Please send correction to
  cage-dev at twistfold dot it.


9 Privacy
═════════

  The author of this software collects no user data information with
  this software.

  But this software is a client to connect and interact to one or more
  remote computer.  So potentially it could share a lot of information
  with other actors but just after the user allowed it to do so.

  It is the user responsibility to checks the privacy conditions of the
  instance this software connect to.

  Moreover launching `quick_quicklisp.sh' will contact
  <https://www.quicklisp.org/>, check the [quicklisp sources] for
  details.

  By default, pressing "!" will contact the remote service located at:
  "gemini://geminispace.info/search".


[quicklisp sources] <https://beta.quicklisp.org/quicklisp.lisp>


10 Contributing
═══════════════

  Any help is appreciated. If you intend to contribute please point your
  browser to the [issue tracker] or file a [pull request].

  But, please take a minute to read the file <./CONTRIBUTING.org>


[issue tracker] <https://notabug.org/cage/tinmop/issues>

[pull request] <https://notabug.org/cage/tinmop/pulls>


11 FAQ
══════

  • I just tried to start the program for the first time but it give me
    a weird error, what's wrong?

    Did you wrote a configuration file before starting?

    Tinmop expects a configuration file in your config directory
    (usually `$HOME/.config/tinmop/').  This file must contains at least
    the username and the name of the instance you want to connect.

    Example :
    ┌────
    │ # a line starting with a '#' is a comment
    │
    │ # a file can be included in another with this directive:
    │ # use "shared.conf"
    │
    │ # The server instance name
    │ server = server address
    │
    │ # your username
    │ username = username
    └────

    If this file does not exists or is invalid tinmop could be used just
    as a gemini client.

    Please check the man page (tinmop(1)) for more information.

  • The `quick_quicklisp.sh' script failed and refuse to start again,
    what can I do?

    If tinmop is the first Common lisp program you have ever installed
    installed, try to rename the directory `$/HOME/quicklisp' and, then,
    restart the script.

    If problem persists [contact me].

  • Is tinmop compatible with mastodon servers?

    Unfortunately no, the way some API endpoint provide the toots is not
    suitable for this client, for more information please go [here].

    I hope this will change in the future but this depends entirely from
    the people are developing the server.

  • OK the program is running but how can i use it?

    Press the key `?' to get a list of the available keys available.

  • Tinmop crashed! Where can i report that?

    The issue tracker is here:

    <https://notabug.org/tinmop/issues/>

    Please also, if possible, send the backtrace of the process. To
    print a backtrace just write `backtrace' when the debugger has been
    invoked.

    *Important note*

    The backtrace can contains sensitive and personal information,
    please always *carefully checks* the backtrace contents before
    making this information public!

  • Are the encrypted messages secure?

    *No*. First only a symmetric encryption scheme is implemented (so
    there is a problem of secure key exchanging). Moreover i am not a
    crypto expert and probably i made something wrong. Note that i am
    not claiming that the algorithm (AES256) or the implementation of
    such encrypting algorithm is flawed but that, likely, is flawed the
    code i wrote to use the crypto library in this software.

    So, please do not consider the encrypted message secure at all.


[contact me] See section 6

[here] <https://github.com/tootsuite/mastodon/issues/13817>


12 NO WARRANTY
══════════════

  tinmop: an humble gemini and pleroma client Copyright (C) 2020 cage

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or (at
  your option) any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see [http://www.gnu.org/licenses/].


[http://www.gnu.org/licenses/] <http://www.gnu.org/licenses/>


13 Acknowledgment
═════════════════

  My deep thanks to the folks that provided us with wonderful SBCL and
  Common lisp libraries.

  In particular i want to thanks the authors of the libraries Croatoan
  and Tooter for their help when I started to develop this program.

  There are more people i borrowed code and data from, they are
  mentioned in the file LINCENSES.org

  This program is was born also with the help of CCCP: "Collettivo
  Computer Club Palermo".

  Also thanks to "barbar" for testing of the installation scripts.
