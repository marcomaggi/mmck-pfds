## Process this file with automake to produce Makefile.in

libvicaredir	= $(libdir)/vicare-scheme
libvicare2dir	= $(libdir)/vicare-scheme

dist_pkglibexec_SCRIPTS		= compile-all.sps

# These files are hand-written.
nobase_dist_libvicare_DATA = \
	pfds/bbtrees.sls		\
	pfds/deques.sls			\
	pfds/dlists.sls			\
	pfds/fingertrees.sls		\
	pfds/heaps.sls			\
	pfds/private/lazy-lists.sls	\
	pfds/psqs.sls			\
	pfds/queues.sls			\
	pfds/sequences.sls		\
	pfds/sets.sls

#page
#### generation of fasl files under the build directory

VICARE_FASL_DIRECTORY		= $(builddir)/fasl
VICARE_COMPILE_LIBRARY_PATH	= $(builddir):$(srcdir):$(VICARE_LIBRARY_PATH)
VICARE_COMPILE_FASL_PATH	= $(VICARE_FASL_DIRECTORY):$(VICARE_FASL_PATH)

# The variable "VFLAGS" is for the user  to be set on the command line
# of "make" or "configure", for example:
#
#   make VFLAGS="-g -O2 --print-loaded-libraries"
#
VFLAGS		= @VFLAGS@
VICARE		= @VICARE@
VICARE_COMPILE_FLAGS	= --fasl-directory $(VICARE_FASL_DIRECTORY) $(VFLAGS)
VICARE_COMPILE_ENV	= VICARE_LIBRARY_PATH=$(VICARE_COMPILE_LIBRARY_PATH)	\
			  VICARE_FASL_PATH=$(VICARE_COMPILE_FASL_PATH)
VICARE_COMPILE_RUN	= $(VICARE_COMPILE_ENV) $(VICARE) $(VICARE_COMPILE_FLAGS)

VICARE_COMPILE_INST_FLAGS = --print-loaded-libraries
VICARE_COMPILE_INST_ENV	= VICARE_LIBRARY_PATH=$(VICARE_LIBRARY_PATH)
VICARE_COMPILE_INST_RUN	= $(VICARE_COMPILE_INST_ENV) $(VICARE) $(VICARE_COMPILE_INST_FLAGS)

VICARE_COMPILE_OPTIONS	= --compile-dependencies $(srcdir)/compile-all.sps

.PHONY : vfasl vfasl-installed

vfasl:
	test -d $(VICARE_FASL_DIRECTORY) || $(MKDIR_P) $(VICARE_FASL_DIRECTORY)
	$(VICARE_COMPILE_RUN) $(VICARE_COMPILE_OPTIONS)

vfasl-installed:
	$(VICARE_COMPILE_INST_RUN) $(VICARE_COMPILE_OPTIONS)

clean-local:
	-rm -rf $(VICARE_FASL_DIRECTORY)

### end of file
