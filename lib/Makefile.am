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

include $(top_srcdir)/meta/makefiles/libs.am

### end of file
