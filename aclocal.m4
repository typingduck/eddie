dnl type_t_check(type, filename) 

AC_DEFUN(AC_TYPE_T_CHECK, 
	[AC_CACHE_CHECK(for $1 in $2, ac_cv_have_$1_$3,
		AC_EGREP_HEADER($1, $2, ac_cv_have_$1_$3=yes, ac_cv_have_$1_$3=no))
	if test $ac_cv_have_$1_$3 = yes ; then
        	AC_DEFINE(HAVE_$1)
		AC_DEFINE(NEED_$3)
	fi
]) dnl

dnl struct_check(struct, filename) 

AC_DEFUN(AC_STRUCT_CHECK,
	[AC_CACHE_CHECK(for $1{} in $2, ac_cv_have_$1_$3,
	 AC_EGREP_HEADER(struct.*$1, $2, ac_cv_have_$1_$3=yes, ac_cv_have_$1_$3=no))
	if test ${ac_cv_have_$1_$3} = yes ; then
        	AC_DEFINE(HAVE_$1)
		AC_DEFINE(NEED_$3)
	fi
]) dnl

dnl define_check(define, filename, filesymbol) 

AC_DEFUN(AC_DEFINE_CHECK,
	[AC_CACHE_CHECK(for $1 in $2, ac_cv_have_$1_$3,
		AC_EGREP_CPP(yes, [
#			include <$2> 
#			ifdef $1
				yes
#			endif]
			, ac_cv_have_$1_$3=yes, ac_cv_have_$1_$3=no))
	if test $ac_cv_have_$1_$3 = yes ; then
		AC_DEFINE(HAVE_$1)
		AC_DEFINE(NEED_$3)
	fi
]) dnl

dnl type_base(symbol, bytes)
dnl note when cross compiling returns common values for 1999 Unix systems

AC_DEFUN(AC_CHECK_SIZES,
	[AC_CHECK_SIZEOF(char, 1) 
	AC_CHECK_SIZEOF(short, 2) 
	AC_CHECK_SIZEOF(int, 4) 
	AC_CHECK_SIZEOF(long, 4) 
	AC_CHECK_SIZEOF(long long, 8) 
]) dnl

AC_DEFUN(AC_TYPE_BASE,
	[AC_REQUIRE([AC_CHECK_SIZES])
	if test $ac_cv_sizeof_char = $2 ; then
		AC_DEFINE($1, char)
	fi
	if test $ac_cv_sizeof_short = $2 ; then
		AC_DEFINE($1, short)
	fi
	if test $ac_cv_sizeof_int = $2 ; then
		AC_DEFINE($1, int)
	fi
	if test $ac_cv_sizeof_long = $2 ; then
		AC_DEFINE($1, long)
	fi
	if test $ac_cv_sizeof_long_long = $2 ; then
		AC_DEFINE($1, long long)
	fi
]) dnl
