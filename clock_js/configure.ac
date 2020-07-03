AC_PREREQ([2.69])
AC_INIT([clock_js], [0.1.0], [])
AC_CONFIG_SRCDIR([src/])
AM_INIT_AUTOMAKE([foreign])
AM_SILENT_RULES([yes])

# Checks for programs.
AC_ERLANG_NEED_ERL
AC_ERLANG_NEED_ERLC
AC_PATH_PROG(DIALYZER, dialyzer)
AC_PATH_PROG(ELM, elm)
if test -z $ac_cv_path_ELM; then
  AC_MSG_ERROR([Can't locate elm compiler])
fi

# Checks for libraries.
AC_ERLANG_SUBST_ERTS_VER
erlang_erts_maj_ver=`echo $ERLANG_ERTS_VER | cut -d'.' -f1`
if test $erlang_erts_maj_ver -lt 11; then
  AC_MSG_ERROR([Erlang/OTP version must >= OTP-23.0!])
fi
AC_ERLANG_CHECK_LIB([erts], [],
  [AC_MSG_ERROR([erts was not found!])])
AC_ERLANG_CHECK_LIB([kernel], [],
  [AC_MSG_ERROR([kernel was not found!])])
AC_ERLANG_CHECK_LIB([stdlib], [],
  [AC_MSG_ERROR([stdlib was not found!])])
AC_ERLANG_CHECK_LIB([sasl], [],
  [AC_MSG_ERROR([sasl was not found!])])
AC_ERLANG_CHECK_LIB([crypto], [],
  [AC_MSG_ERROR([crypto was not found!])])
AC_ERLANG_CHECK_LIB([ssl], [],
  [AC_MSG_ERROR([ssl was not found!])])
AC_ERLANG_CHECK_LIB([asn1], [],
  [AC_MSG_ERROR([asn1 was not found!])])
AC_ERLANG_CHECK_LIB([public_key], [],
  [AC_MSG_ERROR([public_key was not found!])])
AC_ERLANG_CHECK_LIB([cowlib], [],
  [AC_MSG_ERROR([cowlib was not found!])])
AC_ERLANG_CHECK_LIB([ranch], [],
  [AC_MSG_ERROR([ranch was not found!])])
AC_ERLANG_CHECK_LIB([cowboy], [],
  [AC_MSG_ERROR([cowboy was not found!])])

AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([clock_js], [0.1.0])

AC_CONFIG_FILES([
  Makefile
  ebin/clock_js.app
  clock_js.rel])
AC_OUTPUT
