
$(EXEDIR)/%.o : %.f
	$(FC) -c $(FFLAGS) $< -o $@

# _EXE  = tester qptestmain
_EXE  = tester

EXE   = $(_EXE:%=$(EXEDIR)/%)

QPOPT = libqpopt_f77 libblas_f77

QPOPT_LIBS  = $(QPOPT:%=$(LIBDIR)/%.a)

all: $(EXEDIR) $(EXE)

$(EXEDIR):
	mkdir $@

$(EXEDIR)/tester: $(EXEDIR)/tester.o $(QPOPT_LIBS)
	$(FC)  $(FFLAGS) $^ -o $@ $(LIBRARIES)

$(EXEDIR)/qptestmain: $(EXEDIR)/qptestmain.o $(QPOPT_LIBS)
	$(FC)  $(FFLAGS) $^ -o $@ $(LIBRARIES)

clean:
	rm -f *.o
