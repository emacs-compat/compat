COMPATibility Library for Emacs
===============================

> **Note to package developers:** compat.el hasn't yet been published,
> and should not yet be added as a dependency.  The official release
> of the package will coincide with the release of Emacs 28.1.

Find here the source for compat.el, a forwards-compatibility library
for (GNU) Emacs Lisp, versions 24.3 and newer.

The intended audience of this library aren't day-to-day users, but
package developers that wish to make use of newer functionality, not
provided in older versions of Emacs, without breaking compatibility
for users bound to specific Emacs releases.

Version 24.3 is chosen as the oldest version, because this is the
newest version on CentOS 7. It is intended to preserve compatibility
for at least as the Centos 7 reaches [EOL], 2024.

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

    ;; Package-Requires: ((emacs "24.3") (compat "28.1.0.0"))

No further action should be required afterwards.  The effect should be
that all the functions and macros that compat.el provides are
automatically accessible or made accessible as soon as the right
libraries are loaded.

Contribute
----------

As compat.el is distribed as part of GNU ELPA, and therefore requires
a [copyright assignment] to the FSF, for all non-trivial code
contributions.

Source code
-----------

The project is managed can be found on [SourceHut] but has a [GitHub]
mirror as well.

Bug and patches
---------------

Patches, bug reports and comments can be sent to the mailing list

    ~pkal/public-inbox@lists.sr.ht

or via GitHub. These may include issues in the compatibility code,
missing definitions or performance issues.

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
