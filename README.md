COMPATibility Library for Emacs
===============================

Find here the source for compat.el, a forwards-compatibility library
for (GNU) Emacs Lisp, versions 24.3 and newer.

The intended audience of this library aren't day-to-day users, but
package developers that wish to make use of newer functionality, not
provided in older versions of Emacs, without breaking compatibility
for users bound to specific Emacs releases.

Version 24.3 is chosen as the oldest version, because this is the
newest version on CentOS 7. It is intended to preserve compatibility
for at least as the CentOS 7 reaches [EOL], 2024.

If you are developing a package with compat.el in mind, consider
loading `compat-help` (on your system, not in a package) to get
relevant notes inserted into the help buffers of functions that are
implemented or advised in compat.el.

Note that compat.el provides a few prefixed function, ie. functions
with a `compat-` prefix.  These are used to provide extended
functionality for commands that are already defined (`sort`, `assoc`,
...).  It might be possible to transform these into advised functions
later on, so that the modified functionality is accessible without a
prefix.  Feedback on this point is appreciated.

Installation
------------

compat.el shouldn't be installed directly, but downloaded from [GNU
ELPA].  If necessary, this can be done using M-x package-install-file
in the current working directory.

Usage
-----

The intended use-case for this library is for package developers to
add as a dependency in the header:

    ;; Package-Requires: ((emacs "24.3") (compat "28.1.2.0"))

and later on a

	(require 'compat)

This will load all non-prefixed definitions (functions and macros with
a leading `compat-`).  To load these, an additional

	(require 'compat-XY) ; e.g. 26

will be necessary, to load compatibility code for Emacs version XY.

It is recommended to subscribe to the [compat-announce] mailing list
to be notified when new versions are released or relevant changes are
made.

Contribute
----------

As compat.el is distribed as part of GNU ELPA, and therefore requires
a [copyright assignment] to the FSF, for all non-trivial code
contributions.

Source code
-----------

Compat is developed on [SourceHut]. A restricted [GitHub] mirror is
also provided.

Bug and patches
---------------

Patches and comments can be sent to the [development mailing
list][compat-devel].  Bug reports and issues should be directed to the
[issue tracker][compat-tracker] (also accessible via
[Email][compat-tracker-mailto]).  [GitHub] can also be used to submit
patches ("Pull Request").  These may include issues in the
compatibility code, missing definitions or performance issues.

When contributing, make sure to provide test and use the existing
tests defined in compat-test.el.  These can be easily executed using
the bundled Makefile:

    make test

Distribution
------------

compat.el and all other files in this directory are distributed under
the GPL, Version 3 (like Emacs itself).

[EOL]: https://wiki.centos.org/About/Product
[GNU ELPA]: http://elpa.gnu.org/packages/compat.html
[copyright assignment]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Copyright-Assignment.html
[SourceHut]: https://sr.ht/~pkal/compat
[GitHub]: https://github.com/phikal/compat.el
[compat-announce]: https://lists.sr.ht/~pkal/compat-announce
[compat-devel]: https://lists.sr.ht/~pkal/compat-devel
[compat-tracker]: https://todo.sr.ht/~pkal/compat
[compat-tracker-mailto]: mailto:~pkal/compat@todo.sr.ht
