### Binary Decision Diagram (BDD) Based Set
### Copyright (c) 2014, Sang Kil Cha

AC_INIT(bddset, 0.1, sangkilc@cmu.edu)
AC_MSG_RESULT(configuring $PACKAGE_STRING)

# check dependent programs
AC_CHECK_PROG(OCAMLFIND,ocamlfind,ocamlfind,x)
if test "$OCAMLFIND" = x; then
    AC_MSG_ERROR(Cannot find ocamlfind.)
fi

AC_CHECK_PROG(OCAMLDEP,ocamldep,ocamldep,x)
if test "$OCAMLDEP" = x; then
    AC_MSG_ERROR(Cannot find ocamldep.)
fi

AC_CHECK_PROG(OCAMLC,ocamlc,ocamlc,x)
if test "$OCAMLC" = x; then
    AC_MSG_ERROR(Cannot find ocamlc.)
fi

AC_CHECK_PROG(OCAMLOPT,ocamlopt,ocamlopt,x)
if test "$OCAMLOPT" = x; then
    AC_MSG_ERROR(Cannot find ocamlopt.)
fi

AC_CHECK_PROG(OCAMLBUILD,ocamlbuild,ocamlbuild,x)
if test "$OCAMLBUILD" = x ; then
    AC_MSG_ERROR(Cannot find ocamlbuild.)
fi

AC_CHECK_PROG(OCAMLDEP,ocamldep,ocamldep,x)
if test "$OCAMLDEP" = x ; then
    AC_MSG_ERROR(Cannot find ocamldep.)
fi

AC_CHECK_PROG(INSTALL,install,install,x)
if test "$INSTALL" = x ; then
    AC_MSG_ERROR(Cannot find install.)
fi

if test "x$prefix" = "xNONE" ; then
    prefix="/usr/local"
fi
PREFIX=$prefix

AC_PROG_CC
AC_PROG_RANLIB

AC_SUBST(CPPFLAGS)
AC_SUBST(CFLAGS)
AC_SUBST(LDFLAGS)
AC_SUBST(OCAMLFIND)
AC_SUBST(OCAMLC)
AC_SUBST(OCAMLOPT)
AC_SUBST(OCAMLBUILD)
AC_SUBST(OCAMLDEP)
AC_SUBST(CAMLIDL)
AC_SUBST(INSTALL)
AC_SUBST(BAPDIR)
AC_SUBST(PREFIX)

AC_CONFIG_FILES([Makefile],[chmod a-w Makefile])
AC_CONFIG_FILES([Makefile.rule],[chmod a-w Makefile.rule])
AC_OUTPUT

