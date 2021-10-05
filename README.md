COMPATibility Library for Emacs
===============================

Find here the source for compat.el, a forwards-compatibility library
for (GNU) Emacs Lisp.

The intended audience of this library aren't day-to-day users, but
package developers that wish to make use of newer functionality, not
provided in older versions of Emacs, without breaking compatibility
for users bound to specific Emacs releases.

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

The project is managed can be found on [SourceHut] but has a
(disfavoured) [GitHub] mirror as well.

Bug and patches
---------------

Patches, bug reports and comments can be sent to the mailing list

    ~pkal/public-inbox@lists.sr.ht

When contributing, make sure to provide test and use the existing
tests defined in compat-test.el.  These can be easily executed using
the bundled Makefile:

    make test

Distribution
------------

compat.el and all other files in this directory are distributed under
the GPL, Version 3 (like Emacs itself).

[GNU ELPA]: http://elpa.gnu.org/packages/compat.html
[copyright assignment]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Copyright-Assignment.html
[SourceHut]: https://sr.ht/~pkal/compat
[GitHub]: https://github.com/phikal/compat.el
