# Process this file with autoconf to produce a configure script.
AC_INIT(README.FIRST)
AC_CONFIG_AUX_DIR(config)
all_green=1 # No problems so far ;-)

AC_CONFIG_SRCDIR([src/mcsubs.f])

AC_CANONICAL_HOST

#echo
#if test "$srcdir" = .; then
#   npsol_src=src
#else
#   npsol_src="$srcdir/src"
#fi
#AC_SUBST(npsol_src)

# Checks for programs.

AC_PROG_INSTALL
# Use native cc first
if test -z "$CC"; then
   AC_CHECK_PROG(CC,cc,cc)
fi
AC_PROG_CC

case "$host_os" in
linux-gnu* )
        # if F77 is not set, g77 is taken as the default
;;
*)
	# if F77 is not set, force us to check f77 first (before g77)
	if test -z "$F77"; then
	   AC_CHECK_PROG(F77, f77, f77)
	fi
;;
esac

AC_PROG_F77([gfortran g77])
case "$F77" in
	gfortran )
		FFLAGS='-O -fPIC -fdefault-integer-8'
	;;
	*)
		FFLAGS='-O'
	;;
esac
AC_F77_LIBRARY_LDFLAGS
AC_LANG_FORTRAN77

#AC_PROG_FC([gfortran f95 f90 g77],)

#AC_LANG([Fortran])


AC_PROG_RANLIB

# Checks for libraries.

#AC_FC_LIBRARY_LDFLAGS


AC_MSG_CHECKING(for GNU make)
unset gnu_make
if test -n "$MAKE"; then
	if  $MAKE --version 2> /dev/null | grep GNU > /dev/null; then
		gnu_make="$MAKE"
	fi
elif  make --version 2> /dev/null | grep GNU > /dev/null; then
	gnu_make=make
elif gmake --version 2> /dev/null | grep GNU > /dev/null; then
	gnu_make=gmake
fi

if test -n "$gnu_make"; then
	AC_MSG_RESULT($gnu_make)
	GNU_MAKE=$gnu_make
else
	AC_MSG_RESULT(no)
fi

AC_SUBST(GNU_MAKE)

# Look for blas
blas_ok=no
if test -z "$BLAS"; then
# The BLAS environment variable was not set
# Look for blas in some typical places
    case "$host_os" in
    sparc-sun-* )
    # AC_SEARCH_LIBS(dcopy_,sunperf blas,,BLAS="libblas.a")
    AC_CHECK_LIB(sunperf,dcopy_,BLAS=-lsunperf)
    AC_CHECK_LIB(blas,dcopy_,BLAS=-lblas)
    ;;
    *)
    AC_CHECK_LIB(blas,dcopy_,BLAS=-lblas)
    ;;
    esac
    if test -n "$BLAS"; then
#       Got it!
	blas_ok=yes
    fi
fi

if test "$blas_ok" = no; then
    if test -z "$BLAS"; then
#       Still don't have a candidate for the blas library. Try the default
        BLAS=libblas.a
    fi
    AC_CHECK_FILE([$BLAS],blas_ok=yes)
fi
AC_SUBST(BLAS)
if test "$blas_ok" = no; then
   AC_MSG_WARN( [I couldn't find an appropriate BLAS library.] )
fi

dnl Checks for header files.

dnl Checks for typedefs, structures, and compiler characteristics.
case "$host_os" in
	solaris* | sunos* )
		LD_SOFLAGS='-G'
	;;
	*)
		LD_SOFLAGS='-shared'
	;;
esac
AC_SUBST(LD_SOFLAGS)

AC_MSG_CHECKING(whether to create shared libraries)
AC_ARG_ENABLE( shared,
	[enable the creation of shared libraries (default: enabled)],
	:,
	[
	case "$host_os" in
		cygwin* ) enable_shared=no ;;
		darwin* ) enable_shared=no ;;
		*)        enable_shared=yes ;;
	esac
	]
)

AC_MSG_RESULT($enable_shared)
AC_SUBST(enable_shared)

# Now configure the modules

# Test for the presence of the modules
possible_modules="src examples fmex"
modules=""
for module in $possible_modules; do
	if test -d $module; then
		modules="$modules $module"
	fi
done

AC_MSG_CHECKING(The following modules are present)
AC_MSG_RESULT($modules)
AC_SUBST(modules)

if test -z "$gnu_make"; then
	unset all_green
	AC_MSG_WARN(I could not find GNU make.)
fi

dnl check for options
AC_MSG_CHECKING(whether to install f2c)
AC_ARG_WITH(f2c, [  --with-f2c:             F2C support needed (default: enabled)],,
	      with_f2c=yes )

if test "$with_f2c" = yes; then
    F2C_ENABLED=f2c_enabled
    rm -f f2c/DISABLED
    F2CINCLUDE=$PWD/f2c/src
    F2CLIBRARY=$PWD/lib
    F2C=$PWD/bin
else
    F2C_ENABLED='f2c_disabled'
    echo F2c support has been disabled. > f2c/DISABLED
    if   test -z "$F2CINCLUDE"; then
         unset all_green
	 AC_MSG_WARN(Please set F2CINCLUDE environment variable)
    fi
    if   test -z "$F2CLIBRARY"; then
         unset all_green
	 AC_MSG_WARN(Please set F2CLIBRARY environment variable)
    fi
    if   test -z "$F2C"; then
         unset all_green
	 AC_MSG_WARN(Please set F2C environment variable)
    fi
fi
AC_MSG_RESULT($with_f2c)

AC_SUBST(F2C_ENABLED)
AC_SUBST(F2CINCLUDE)
AC_SUBST(F2CLIBRARY)
AC_SUBST(F2C)

#  End check options

AC_OUTPUT(GNUmakefile)

dnl Did everything work so far?

if test -z "$all_green"; then
	echo
	echo Some problems were detected during configuration.
	echo Please read all warning messages given above.
fi

if test $gnu_make != make; then
	echo
	echo Remember to use GNU make to build this distribution.
	echo GNU make is installed on your system as $gnu_make.
fi
