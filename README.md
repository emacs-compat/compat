COMPATibility Library for Emacs
===============================

Find here the source for compat.el, a forwards-compatibility library
for (GNU) Emacs Lisp, versions 24.3 and newer.

For information on how to use Compat, please consult the [manual].
The latest state of development can be found in the [snapshot
version].

[manual]:
	https://elpa.gnu.org/packages/doc/compat.html
[snapshot version]:
	https://elpa.gnu.org/devel/doc/compat.html

Contribute
----------

As compat.el is distribed as part of [GNU ELPA], and therefore
requires a [copyright assignment] to the [FSF], for all non-trivial code
contributions.

[GNU ELPA]:
	http://elpa.gnu.org/packages/compat.html
[copyright assignment]:
	https://www.gnu.org/software/emacs/manual/html_node/emacs/Copyright-Assignment.html
[FSF]:
	https://www.fsf.org/

Source code
-----------

Compat is developed on [SourceHut]. A restricted [GitHub] mirror is
also provided.

[SourceHut]:
	https://sr.ht/~pkal/compat
[GitHub]:
	https://github.com/phikal/compat.el

Bug and patches
---------------

Patches and comments can be sent to the [development mailing list].
Bug reports and issues should be directed to the [issue tracker] (also
accessible via [email]).  [GitHub] can also be used to submit patches
("Pull Request").  These may include issues in the compatibility code,
missing definitions or performance issues.

When contributing, make sure to provide test and use the existing
tests defined in compat-test.el.  These can be easily executed using
the bundled Makefile:

	make test

[development mailing list]:
	https://lists.sr.ht/~pkal/compat-devel
[issue tracker]:
	https://todo.sr.ht/~pkal/compat
[email]:
	mailto:~pkal/compat@todo.sr.ht

Distribution
------------

compat.el and all other source files in this directory are distributed
under the [GNU Public License], Version 3 (like Emacs itself).  The manual
(`compat.texi`) is distributed under the [GNU Free Documentation
License], Version 1.3.

[GNU Public License]:
	https://www.gnu.org/licenses/gpl-3.0.en.html
[GNU Free Documentation License]:
	https://www.gnu.org/licenses/fdl-1.3.html
