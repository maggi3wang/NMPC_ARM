LP      = lpoptcmex  mexUtility lpprmswrapper
QP      = qpoptcmex  mexUtility qpprmswrapper

LP_O    = $(LP:%=%.o)
QP_O    = $(QP:%=%.o)

OBJECTS = $(LP_O) $(QP_O)

CMEX = ../

MEXOPTS = -I$(F2CINC) -argcheck $(MFLAGS)
F2COPTS = -ec -a -A

BLAS    = $(LIBDIR)/libblas_c.a
QPOPT   = $(LIBDIR)/libqpopt_c.a

QPLIBS  = $(QPOPT) $(BLAS) $(F2CLIB)/libf2c.a

# The target which represents the "mex" file. The actual mex file
# will be named something like "qpoptcmex.mexsg". The extension depends
# on the operating system. I cannot find a way to determine the extension
# automatically, so I had to do some funny things in the build rules.
#
# The file "dummy.c" exists only to force mex into C mode.  It should
# default to C mode, but as of version 5.3.0.421, it does not do so
# gracefully.

all: lpoptcmex qpoptcmex
lpoptcmex: $(F2CINC)/f2c.h $(LP_O) $(QPLIBS)
	@echo "Installing lpopt cmex files in ${CMEX}"
	mex $(MEXOPTS) -output lpoptcmex  dummy.c $(LP_O) $(QPLIBS)
	mv lpoptcmex.mex* $(CMEX)
	@echo "Ignore warnings about dummy.c."

qpoptcmex: $(F2CINC)/f2c.h $(QP_O) $(QPLIBS)
	@echo "Installing qpopt cmex files in ${CMEX}"
	mex $(MEXOPTS) -output qpoptcmex dummy.c $(QP_O) $(QPLIBS)
	mv qpoptcmex.mex* $(CMEX)
	@echo "Ignore warnings about dummy.c."

# OBJECT files depend on header files
mexUtility.o:  mexUtility.h
lpoptcmex.o:   mexUtility.h lpopt.h
qpoptcmex.o:   mexUtility.h qpopt.h

# The *filewrapper.c routines are f2c translated files.
lpfilewrapper.c: lpfilewrapper.f
	f2c $(F2COPTS) lpfilewrapper.f
qpfilewrapper.c: qpfilewrapper.f
	f2c $(F2COPTS) qpfilewrapper.f

# If not commmented out, all files will be recompiled if the makefile
# changes.
$(OBJECTS): GNUmakefile

# Use mex to compile the object files
.c.o:
	mex $(MEXOPTS) -c $*.c

# Fake target to remind people to set the F2C environment variable
$(F2CINC)/f2c.h:
	@echo "Could not find the f2c distribution."
	@echo "Set the environment variable F2CINCLUDE"
	@echo "to the directory containing f2c."
	@false
$(F2CLIB)/libf2c.a:
	@echo "Could not find the f2c library."
	@echo "Set the environment variable F2CLIBRARY"
	@echo "to the directory containing libf2c.a."
	@false
clean:
	\rm -f *.o
