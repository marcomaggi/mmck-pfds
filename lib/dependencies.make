## dependencies.make --
#
# Automatically built.

lib/pfds/bbtrees.fasl: \
		lib/pfds/bbtrees.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_pfds_bbtrees_fasldir = $(bundledlibsdir)/pfds
lib_pfds_bbtrees_slsdir  = $(bundledlibsdir)/pfds
nodist_lib_pfds_bbtrees_fasl_DATA = lib/pfds/bbtrees.fasl
if WANT_INSTALL_SOURCES
dist_lib_pfds_bbtrees_sls_DATA = lib/pfds/bbtrees.sls
endif
EXTRA_DIST += lib/pfds/bbtrees.sls
CLEANFILES += lib/pfds/bbtrees.fasl

lib/pfds/deques.fasl: \
		lib/pfds/deques.sls \
		lib/pfds/private/lazy-lists.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_pfds_deques_fasldir = $(bundledlibsdir)/pfds
lib_pfds_deques_slsdir  = $(bundledlibsdir)/pfds
nodist_lib_pfds_deques_fasl_DATA = lib/pfds/deques.fasl
if WANT_INSTALL_SOURCES
dist_lib_pfds_deques_sls_DATA = lib/pfds/deques.sls
endif
EXTRA_DIST += lib/pfds/deques.sls
CLEANFILES += lib/pfds/deques.fasl

lib/pfds/private/lazy-lists.fasl: \
		lib/pfds/private/lazy-lists.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_pfds_private_lazy_lists_fasldir = $(bundledlibsdir)/pfds/private
lib_pfds_private_lazy_lists_slsdir  = $(bundledlibsdir)/pfds/private
nodist_lib_pfds_private_lazy_lists_fasl_DATA = lib/pfds/private/lazy-lists.fasl
if WANT_INSTALL_SOURCES
dist_lib_pfds_private_lazy_lists_sls_DATA = lib/pfds/private/lazy-lists.sls
endif
EXTRA_DIST += lib/pfds/private/lazy-lists.sls
CLEANFILES += lib/pfds/private/lazy-lists.fasl

lib/pfds/dlists.fasl: \
		lib/pfds/dlists.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_pfds_dlists_fasldir = $(bundledlibsdir)/pfds
lib_pfds_dlists_slsdir  = $(bundledlibsdir)/pfds
nodist_lib_pfds_dlists_fasl_DATA = lib/pfds/dlists.fasl
if WANT_INSTALL_SOURCES
dist_lib_pfds_dlists_sls_DATA = lib/pfds/dlists.sls
endif
EXTRA_DIST += lib/pfds/dlists.sls
CLEANFILES += lib/pfds/dlists.fasl

lib/pfds/fingertrees.fasl: \
		lib/pfds/fingertrees.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_pfds_fingertrees_fasldir = $(bundledlibsdir)/pfds
lib_pfds_fingertrees_slsdir  = $(bundledlibsdir)/pfds
nodist_lib_pfds_fingertrees_fasl_DATA = lib/pfds/fingertrees.fasl
if WANT_INSTALL_SOURCES
dist_lib_pfds_fingertrees_sls_DATA = lib/pfds/fingertrees.sls
endif
EXTRA_DIST += lib/pfds/fingertrees.sls
CLEANFILES += lib/pfds/fingertrees.fasl

lib/pfds/heaps.fasl: \
		lib/pfds/heaps.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_pfds_heaps_fasldir = $(bundledlibsdir)/pfds
lib_pfds_heaps_slsdir  = $(bundledlibsdir)/pfds
nodist_lib_pfds_heaps_fasl_DATA = lib/pfds/heaps.fasl
if WANT_INSTALL_SOURCES
dist_lib_pfds_heaps_sls_DATA = lib/pfds/heaps.sls
endif
EXTRA_DIST += lib/pfds/heaps.sls
CLEANFILES += lib/pfds/heaps.fasl

lib/pfds/psqs.fasl: \
		lib/pfds/psqs.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_pfds_psqs_fasldir = $(bundledlibsdir)/pfds
lib_pfds_psqs_slsdir  = $(bundledlibsdir)/pfds
nodist_lib_pfds_psqs_fasl_DATA = lib/pfds/psqs.fasl
if WANT_INSTALL_SOURCES
dist_lib_pfds_psqs_sls_DATA = lib/pfds/psqs.sls
endif
EXTRA_DIST += lib/pfds/psqs.sls
CLEANFILES += lib/pfds/psqs.fasl

lib/pfds/queues.fasl: \
		lib/pfds/queues.sls \
		lib/pfds/private/lazy-lists.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_pfds_queues_fasldir = $(bundledlibsdir)/pfds
lib_pfds_queues_slsdir  = $(bundledlibsdir)/pfds
nodist_lib_pfds_queues_fasl_DATA = lib/pfds/queues.fasl
if WANT_INSTALL_SOURCES
dist_lib_pfds_queues_sls_DATA = lib/pfds/queues.sls
endif
EXTRA_DIST += lib/pfds/queues.sls
CLEANFILES += lib/pfds/queues.fasl

lib/pfds/sequences.fasl: \
		lib/pfds/sequences.sls \
		lib/pfds/fingertrees.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_pfds_sequences_fasldir = $(bundledlibsdir)/pfds
lib_pfds_sequences_slsdir  = $(bundledlibsdir)/pfds
nodist_lib_pfds_sequences_fasl_DATA = lib/pfds/sequences.fasl
if WANT_INSTALL_SOURCES
dist_lib_pfds_sequences_sls_DATA = lib/pfds/sequences.sls
endif
EXTRA_DIST += lib/pfds/sequences.sls
CLEANFILES += lib/pfds/sequences.fasl

lib/pfds/sets.fasl: \
		lib/pfds/sets.sls \
		lib/pfds/bbtrees.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_pfds_sets_fasldir = $(bundledlibsdir)/pfds
lib_pfds_sets_slsdir  = $(bundledlibsdir)/pfds
nodist_lib_pfds_sets_fasl_DATA = lib/pfds/sets.fasl
if WANT_INSTALL_SOURCES
dist_lib_pfds_sets_sls_DATA = lib/pfds/sets.sls
endif
EXTRA_DIST += lib/pfds/sets.sls
CLEANFILES += lib/pfds/sets.fasl


### end of file
# Local Variables:
# mode: makefile-automake
# End:
