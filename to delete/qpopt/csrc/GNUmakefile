# This is csrc/GNUmakefile

%.o : %.f
	$(FC) -c $(FFLAGS) $< -o $@

%.o : %.c
	$(CC) -c $(CFLAGS) $< -o $@

BLAS     = blas1        blas2
QPOPT    = mcsubs       opsubs       f06subs      cmsubs     \
           utcomsubs    rzsubs       lpoptsubs    qpoptsubs  \
           lcpar1_com   lcpar2_com   solmch_com              \
           sol1cm_com   sol3cm_com   sol4cm_com   sol5cm_com \
           sol1lc_com   sol2lc_com   sol3lc_com   sol4lc_com \
           sol1qp_com

BLAS_O   = $(BLAS:%=%.o)
QPOPT_O  = $(QPOPT:%=%.o)

ALL_O   = $(BLAS_O) $(QPOPT_O)

all: $(LIBDIR) $(LIBDIR)/libblas_c.a  \
               $(LIBDIR)/libqpopt_c.a

$(LIBDIR):
	mkdir $@

$(LIBDIR)/libblas_c.a:   $(BLAS_O)
	 ar ruv $@       $(BLAS_O)
	-ranlib $@

$(LIBDIR)/libqpopt_c.a:  $(QPOPT_O)
	 ar ruv $@       $(QPOPT_O)
	-ranlib $@

clean:
	-\rm -f *.o
