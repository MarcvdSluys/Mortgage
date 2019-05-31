# Compile mortgage code

.PHONY: clean cleanIFs exit code_version
.PRECIOUS: %.o



#No text/space after statements below!
FORT = gfortran#      Use GNU Fortran compiler.  Need >= 4.5
#FORT = g95#           USE g95 compiler
#FORT = ifort#         Use Intel Fortran compiler

WARN = yes#    # compile with compile-time warnings
RUNCHECK = yes#   # compile with run-time checks
DEBUG = yes#   # compile with debug options
F2008 = yes#   # compile with Fortran 2008 standard
#OPENMP = yes#  # compile with OpenMP support
#PROFILE = yes#  # compile with profiling information


#General:
#OPT = -O0
#OPT = -O1
OPT = -O2
#OPT = -O3
LFLAGS = #-L${HOME}/usr/lib 

# Directories for the source, object, module, libary and executable files:
SRCDIR = src
OBJDIR = .obj
#MODDIR = .mod
MODDIR = .
LIBDIR = lib
#BINDIR = bin
BINDIR = .

# Standard objects:
STDOBJ  = # $(OBJDIR)/modules.o
STDLOBJ = $(STDOBJ)


### gfortran: similar in speed to ifort, but file reading is ~2.5x slower!  (Turns out default for ifort = -Ofast?)
ifeq ($(FORT),gfortran)
   CFLAGS += -I/usr/include/libSUFR/
   LFLAGS += -lSUFR
   STATLIBS += /usr/lib/libSUFR.a

   CFLAGS += -ffree-line-length-0 -fwhole-file #-fwhole-program
   #CFLAGS += -fdiagnostics-color=auto

   #CFLAGS += -pipe
   #CFLAGS+=" -funroll-loops"
   #CFLAGS += -Ofast

   ifeq ($(RUNCHECK),yes)
      OPT = -O0
      CFLAGS += -fcheck=all
      CFLAGS += -fbacktrace
   endif

   ifeq ($(WARN),yes)
      CFLAGS += -Wall -Wextra
      CFLAGS += -Wcharacter-truncation -Wunderflow
#      CFLAGS += -Warray-temporaries
#      CFLAGS += -Wconversion
#      CFLAGS += -Wimplicit-interface
      CFLAGS += -fmax-errors=10
# Use implicit none everywhere:
#      CFLAGS += -fimplicit-none

# Turn warnings into errors:
#      CFLAGS += -Werror
   endif

   #To debug code:
   ifeq ($(DEBUG),yes)
      OPT = -O0
      CFLAGS += -g -ggdb
   endif

   ifeq ($(F2008),yes)
      CFLAGS += -std=f2008 -fall-intrinsics -pedantic
   else
      CFLAGS += -ffree-line-length-none
   endif

   ifeq ($(OPENMP),yes)
      CFLAGS += -fopenmp
      LFLAGS += -fopenmp
   endif

   ifeq ($(PROFILE),yes)
      OPT = -O0
      CFLAGS += -pg --coverage
      LFLAGS += -pg --coverage
   endif

   CFLAGS += -I$(MODDIR) -J$(MODDIR)
   STATIC = -static

endif




### g95: ###

ifeq ($(FORT),g95)
   CFLAGS += -ffree-line-length-huge
   CFLAGS += -I /usr/include/libSUFR
   LFLAGS += -lSUFR
   STATLIBS += -lSUFR
   PGSTATLIBS += -lgfortran

   ifeq ($(RUNCHECK),yes)
      OPT = -O0
      CFLAGS += -fbounds-check
      CFLAGS += -ftrace=full
   endif

   ifeq ($(WARN),yes)
      CFLAGS += -Wall -Wextra
      CFLAGS += -Wno=165
      # Turn warnings into errors:
      CFLAGS += -Werror -fone-error
   endif

   # To debug code:
   ifeq ($(DEBUG),yes)
      OPT = -O0
      CFLAGS += -g
   endif

   ifeq ($(F2008),yes)
      CFLAGS += -std=f2003
#   else
#      CFLAGS += 
   endif

   ifeq ($(OPENMP),yes)
      #CFLAGS += -fopenmp
      #LFLAGS += -lgomp
   endif

   CFLAGS += -I$(MODDIR) -fmod=$(MODDIR)
   #STDOBJ += $(OBJDIR)/nagfor.o
   STATIC = -static
endif




#ifort:
ifeq ($(FORT),ifort)
   CFLAGS += -I /usr/include/libSUFR
   LFLAGS += -lSUFR
   STATLIBS += /usr/lib/libSUFR.a
   PGSTATLIBS += /usr/lib/libgfortran.a /usr/lib/libquadmath.a

   # To develop code  (don't mix -check bounds with -O>0!):
   ifeq ($(RUNCHECK),yes)
      OPT = -O0
      CFLAGS += -check all -check noarg_temp_created
   endif

   ifeq ($(WARN),yes)
      CFLAGS += -warn all -WB
      #CFLAGS += -warn errors # Not icw 5268
   endif

   ifeq ($(F2008),yes)
      CFLAGS += -std08 -diag-disable 5268,6894,8290
   endif

   ifeq ($(OPENMP),yes)
      CFLAGS += -openmp
      LFLAGS += -openmp
   endif

   CFLAGS += -qopt-report0 -traceback -error-limit 10
   CFLAGS += -module $(MODDIR) -I$(MODDIR)

   # To debug code:
   ifeq ($(DEBUG),yes)
      OPT = -O0
      CFLAGS += -g
   endif


   CFLAGS += -nogen-interfaces  # For ifort >= v.11
   #STDOBJ += $(OBJDIR)/ifort.o
   STATIC = -static

   #OPT += -ipo
   #LFLAGS += -ipo
   #OPT += -xsse4.1
   #OPT += -xhost
   #CFLAGS += -static $(STATLIBS)
   #CFLAGS += -prof-gen -prof-dir./  #Don't use -ipo. Run the code thus compiled (a few times) and use -prof-use below for the final version
   #CFLAGS += -prof-use -prof-dir./

endif


#nagfor:
ifeq ($(FORT),nagfor)
   ifeq ($(RUNCHECK),yes)
      CFLAGS = -O0
      CFLAGS += -C=all
   endif
   ifneq ($(WARN),yes)
      CFLAGS += -w=all  #Warnings are on by default
   endif
   CFLAGS += -colour -f2008 -u -gline
   STDOBJ += $(OBJDIR)/nagfor.o
   CFLAGS += -I$(MODDIR) -mdir $(MODDIR)
   STATIC = -Bstatic
endif





#CFLAGS += -fPIC


GLFLAGS += $(LFLAGS)
PLLFLAGS += $(LFLAGS)





all: mortgage

printversion:
	$(FORT) --version

exit:

$(OBJDIR):
	mkdir $(OBJDIR)

$(OBJDIR)/%.o: $(SRCDIR)/%.f90 $(OBJDIR)
	$(FORT) $(OPT) $(CFLAGS) -c $< -o $@



mortgage: $(STDOBJ) $(OBJDIR)/mortgage.o
	$(FORT) $(LFLAGS) -o $(BINDIR)/mortgage $(STDLOBJ) $(OBJDIR)/mortgage.o







clean:
	rm -f $(OBJDIR)/*.o $(OBJDIR)/*.gcda $(OBJDIR)/*.gcno $(MODDIR)/*.mod

cleanoptimisation:
	rm -f ????????_?????.dyn pgopti.dpi pgopti.dpi.lock

cleanIFs:
	rm -f $(MODDIR)/*__genmod.f90 $(MODDIR)/*__genmod.mod

cleanbin:
	rm -f mortgage

cleanall:  clean cleanoptimisation cleanIFs




