## Process this file with automake to produce Makefile.in

include meta/automake/mmck-rules.make


#### documentation

info_TEXINFOS		= doc/mmck-pfds.texi
doc_mmck_pfds_TEXINFOS = \
	doc/macros.texi			\
	doc/fdl-1.3.texi


#### library build targets

# All the source files of the extension library.
#
MMCK_PFDS_MODULES		= \
	lib/mmck.pfds.helpers.scm		\
	lib/mmck.pfds.lazy-lists.scm		\
	lib/mmck.pfds.bbtrees.scm		\
	lib/mmck.pfds.deques.scm		\
	lib/mmck.pfds.dlists.scm		\
	lib/mmck.pfds.fingertrees.scm		\
	lib/mmck.pfds.heaps.scm			\
	lib/mmck.pfds.psqs.scm			\
	lib/mmck.pfds.queues.scm		\
	lib/mmck.pfds.sequences.scm		\
	lib/mmck.pfds.sets.scm			\
	lib/mmck.pfds.version.scm		\
	lib/mmck.pfds.scm

# All the object files that must be linked in the extension library.
#
MMCK_PFDS_RUNTIME_OBJECTS	= \
	lib/mmck.pfds.helpers.$(OBJEXT)		\
	lib/mmck.pfds.lazy-lists.$(OBJEXT)	\
	lib/mmck.pfds.bbtrees.$(OBJEXT)		\
	lib/mmck.pfds.deques.$(OBJEXT)		\
	lib/mmck.pfds.dlists.$(OBJEXT)		\
	lib/mmck.pfds.fingertrees.$(OBJEXT)	\
	lib/mmck.pfds.heaps.$(OBJEXT)		\
	lib/mmck.pfds.psqs.$(OBJEXT)		\
	lib/mmck.pfds.queues.$(OBJEXT)		\
	lib/mmck.pfds.sequences.$(OBJEXT)	\
	lib/mmck.pfds.sets.$(OBJEXT)		\
	lib/mmck.pfds.version.$(OBJEXT)		\
	lib/mmck.pfds.$(OBJEXT)

# All the source files of all the modules' import libraries.
#
MMCK_PFDS_IMPORT_SOURCES	= \
	lib/mmck.pfds.helpers.import.scm	\
	lib/mmck.pfds.lazy-lists.import.scm	\
	lib/mmck.pfds.bbtrees.import.scm	\
	lib/mmck.pfds.deques.import.scm		\
	lib/mmck.pfds.dlists.import.scm		\
	lib/mmck.pfds.fingertrees.import.scm	\
	lib/mmck.pfds.heaps.import.scm		\
	lib/mmck.pfds.psqs.import.scm		\
	lib/mmck.pfds.queues.import.scm		\
	lib/mmck.pfds.sequences.import.scm	\
	lib/mmck.pfds.sets.import.scm		\
	lib/mmck.pfds.version.import.scm	\
	lib/mmck.pfds.import.scm

# The module's import libraries needed to compile main module's object
# file.  It does not include the main import library:
#
#   lib/mmck.pfds.import.$(MMCK_SOEXT)
#
MMCK_PFDS_IMPORT_DEPS	= \
	lib/mmck.pfds.helpers.import.$(MMCK_SOEXT)	\
	lib/mmck.pfds.lazy-lists.import.$(MMCK_SOEXT)	\
	lib/mmck.pfds.bbtrees.import.$(MMCK_SOEXT)	\
	lib/mmck.pfds.deques.import.$(MMCK_SOEXT)	\
	lib/mmck.pfds.dlists.import.$(MMCK_SOEXT)	\
	lib/mmck.pfds.fingertrees.import.$(MMCK_SOEXT)	\
	lib/mmck.pfds.heaps.import.$(MMCK_SOEXT)	\
	lib/mmck.pfds.psqs.import.$(MMCK_SOEXT)		\
	lib/mmck.pfds.queues.import.$(MMCK_SOEXT)	\
	lib/mmck.pfds.sequences.import.$(MMCK_SOEXT)	\
	lib/mmck.pfds.sets.import.$(MMCK_SOEXT)		\
	lib/mmck.pfds.version.import.$(MMCK_SOEXT)

# The import  libraries needed  to compile new  code using  the shared
# library  "(mmck pfds)".   We need  to build  and install  all of
# them.
#
MMCK_PFDS_IMPORT_SHARED_LIBS	= \
	lib/mmck.pfds.helpers.import.$(MMCK_SOEXT)	\
	lib/mmck.pfds.lazy-lists.import.$(MMCK_SOEXT)	\
	lib/mmck.pfds.bbtrees.import.$(MMCK_SOEXT)	\
	lib/mmck.pfds.deques.import.$(MMCK_SOEXT)	\
	lib/mmck.pfds.dlists.import.$(MMCK_SOEXT)	\
	lib/mmck.pfds.fingertrees.import.$(MMCK_SOEXT)	\
	lib/mmck.pfds.heaps.import.$(MMCK_SOEXT)	\
	lib/mmck.pfds.psqs.import.$(MMCK_SOEXT)		\
	lib/mmck.pfds.queues.import.$(MMCK_SOEXT)	\
	lib/mmck.pfds.sequences.import.$(MMCK_SOEXT)	\
	lib/mmck.pfds.sets.import.$(MMCK_SOEXT)		\
	lib/mmck.pfds.version.import.$(MMCK_SOEXT)	\
	lib/mmck.pfds.import.$(MMCK_SOEXT)

# This is the "(mmck pfds)" shared library file pathname.
#
MMCK_PFDS_RUNTIME_SHARED_LIBS	= lib/mmck.pfds.$(MMCK_SOEXT)

## --------------------------------------------------------------------

EXTRA_DIST	+= $(MMCK_PFDS_MODULES)

CLEANFILES	+= \
	$(MMCK_PFDS_RUNTIME_SHARED_LIBS)	\
	$(MMCK_PFDS_RUNTIME_OBJECTS)		\
	$(MMCK_PFDS_IMPORT_SHARED_LIBS)		\
	$(MMCK_PFDS_IMPORT_SOURCES)

## --------------------------------------------------------------------

lib/mmck.pfds.version.$(OBJEXT): lib/mmck.pfds.version.scm $(MMCK_OBJECTS_DEPS)
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) $(@) $(<) -prologue lib/config.scm
	@$(MV) mmck.pfds.version.import.scm lib/mmck.pfds.version.import.scm

lib/mmck.pfds.version.import.scm: lib/mmck.pfds.version.$(OBJEXT)

lib/mmck.pfds.version.import.$(MMCK_SOEXT): lib/mmck.pfds.version.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)

## --------------------------------------------------------------------

lib/mmck.pfds.helpers.$(OBJEXT): lib/mmck.pfds.helpers.scm $(MMCK_OBJECTS_DEPS)
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) $(@) $(<)
	@$(MV) mmck.pfds.helpers.import.scm lib/mmck.pfds.helpers.import.scm

lib/mmck.pfds.helpers.import.scm: lib/mmck.pfds.helpers.$(OBJEXT)

lib/mmck.pfds.helpers.import.$(MMCK_SOEXT): lib/mmck.pfds.helpers.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)

## --------------------------------------------------------------------

lib/mmck.pfds.lazy-lists.$(OBJEXT): lib/mmck.pfds.lazy-lists.scm $(MMCK_OBJECTS_DEPS)
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) $(@) $(<)
	@$(MV) mmck.pfds.lazy-lists.import.scm lib/mmck.pfds.lazy-lists.import.scm

lib/mmck.pfds.lazy-lists.import.scm: lib/mmck.pfds.lazy-lists.$(OBJEXT)

lib/mmck.pfds.lazy-lists.import.$(MMCK_SOEXT): lib/mmck.pfds.lazy-lists.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)

## --------------------------------------------------------------------

MMCK_PFDS_BBTREES_DEPS = \
	lib/mmck.pfds.helpers.import.$(MMCK_SOEXT)	\
	$(MMCK_OBJECTS_DEPS)

lib/mmck.pfds.bbtrees.$(OBJEXT): lib/mmck.pfds.bbtrees.scm $(MMCK_PFDS_BBTREES_DEPS)
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) $(@) $(<)
	@$(MV) mmck.pfds.bbtrees.import.scm lib/mmck.pfds.bbtrees.import.scm

lib/mmck.pfds.bbtrees.import.scm: lib/mmck.pfds.bbtrees.$(OBJEXT)

lib/mmck.pfds.bbtrees.import.$(MMCK_SOEXT): lib/mmck.pfds.bbtrees.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)

## --------------------------------------------------------------------

MMCK_PFDS_DEQUES_DEPS = \
	lib/mmck.pfds.helpers.import.$(MMCK_SOEXT)	\
	lib/mmck.pfds.lazy-lists.import.$(MMCK_SOEXT)	\
	$(MMCK_OBJECTS_DEPS)

lib/mmck.pfds.deques.$(OBJEXT): lib/mmck.pfds.deques.scm $(MMCK_PFDS_DEQUES_DEPS)
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) $(@) $(<)
	@$(MV) mmck.pfds.deques.import.scm lib/mmck.pfds.deques.import.scm

lib/mmck.pfds.deques.import.scm: lib/mmck.pfds.deques.$(OBJEXT)

lib/mmck.pfds.deques.import.$(MMCK_SOEXT): lib/mmck.pfds.deques.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)

## --------------------------------------------------------------------

MMCK_PFDS_DLISTS_DEPS = \
	lib/mmck.pfds.helpers.import.$(MMCK_SOEXT)	\
	$(MMCK_OBJECTS_DEPS)

lib/mmck.pfds.dlists.$(OBJEXT): lib/mmck.pfds.dlists.scm $(MMCK_PFDS_DLISTS_DEPS)
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) $(@) $(<)
	@$(MV) mmck.pfds.dlists.import.scm lib/mmck.pfds.dlists.import.scm

lib/mmck.pfds.dlists.import.scm: lib/mmck.pfds.dlists.$(OBJEXT)

lib/mmck.pfds.dlists.import.$(MMCK_SOEXT): lib/mmck.pfds.dlists.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)

## --------------------------------------------------------------------

MMCK_PFDS_FINGERTREES_DEPS = \
	lib/mmck.pfds.helpers.import.$(MMCK_SOEXT)	\
	$(MMCK_OBJECTS_DEPS)

lib/mmck.pfds.fingertrees.$(OBJEXT): lib/mmck.pfds.fingertrees.scm $(MMCK_PFDS_FINGERTREES_DEPS)
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) $(@) $(<)
	@$(MV) mmck.pfds.fingertrees.import.scm lib/mmck.pfds.fingertrees.import.scm

lib/mmck.pfds.fingertrees.import.scm: lib/mmck.pfds.fingertrees.$(OBJEXT)

lib/mmck.pfds.fingertrees.import.$(MMCK_SOEXT): lib/mmck.pfds.fingertrees.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)

## --------------------------------------------------------------------

MMCK_PFDS_HEAPS_DEPS = \
	lib/mmck.pfds.helpers.import.$(MMCK_SOEXT)	\
	$(MMCK_OBJECTS_DEPS)

lib/mmck.pfds.heaps.$(OBJEXT): lib/mmck.pfds.heaps.scm $(MMCK_PFDS_HEAPS_DEPS)
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) $(@) $(<)
	@$(MV) mmck.pfds.heaps.import.scm lib/mmck.pfds.heaps.import.scm

lib/mmck.pfds.heaps.import.scm: lib/mmck.pfds.heaps.$(OBJEXT)

lib/mmck.pfds.heaps.import.$(MMCK_SOEXT): lib/mmck.pfds.heaps.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)

## --------------------------------------------------------------------

MMCK_PFDS_PSQS_DEPS = \
	lib/mmck.pfds.helpers.import.$(MMCK_SOEXT)	\
	$(MMCK_OBJECTS_DEPS)

lib/mmck.pfds.psqs.$(OBJEXT): lib/mmck.pfds.psqs.scm $(MMCK_PFDS_PSQS_DEPS)
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) $(@) $(<)
	@$(MV) mmck.pfds.psqs.import.scm lib/mmck.pfds.psqs.import.scm

lib/mmck.pfds.psqs.import.scm: lib/mmck.pfds.psqs.$(OBJEXT)

lib/mmck.pfds.psqs.import.$(MMCK_SOEXT): lib/mmck.pfds.psqs.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)

## --------------------------------------------------------------------

MMCK_PFDS_QUEUES_DEPS = \
	lib/mmck.pfds.helpers.import.$(MMCK_SOEXT)	\
	$(MMCK_OBJECTS_DEPS)

lib/mmck.pfds.queues.$(OBJEXT): lib/mmck.pfds.queues.scm $(MMCK_PFDS_QUEUES_DEPS)
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) $(@) $(<)
	@$(MV) mmck.pfds.queues.import.scm lib/mmck.pfds.queues.import.scm

lib/mmck.pfds.queues.import.scm: lib/mmck.pfds.queues.$(OBJEXT)

lib/mmck.pfds.queues.import.$(MMCK_SOEXT): lib/mmck.pfds.queues.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)

## --------------------------------------------------------------------

MMCK_PFDS_SEQUENCES_DEPS = \
	lib/mmck.pfds.helpers.import.$(MMCK_SOEXT)	\
	lib/mmck.pfds.fingertrees.import.$(MMCK_SOEXT)	\
	$(MMCK_OBJECTS_DEPS)

lib/mmck.pfds.sequences.$(OBJEXT): lib/mmck.pfds.sequences.scm $(MMCK_PFDS_SEQUENCES_DEPS)
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) $(@) $(<)
	@$(MV) mmck.pfds.sequences.import.scm lib/mmck.pfds.sequences.import.scm

lib/mmck.pfds.sequences.import.scm: lib/mmck.pfds.sequences.$(OBJEXT)

lib/mmck.pfds.sequences.import.$(MMCK_SOEXT): lib/mmck.pfds.sequences.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)

## --------------------------------------------------------------------

MMCK_PFDS_SETS_DEPS = \
	lib/mmck.pfds.helpers.import.$(MMCK_SOEXT)	\
	lib/mmck.pfds.bbtrees.import.$(MMCK_SOEXT)	\
	$(MMCK_OBJECTS_DEPS)

lib/mmck.pfds.sets.$(OBJEXT): lib/mmck.pfds.sets.scm $(MMCK_PFDS_SETS_DEPS)
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) $(@) $(<)
	@$(MV) mmck.pfds.sets.import.scm lib/mmck.pfds.sets.import.scm

lib/mmck.pfds.sets.import.scm: lib/mmck.pfds.sets.$(OBJEXT)

lib/mmck.pfds.sets.import.$(MMCK_SOEXT): lib/mmck.pfds.sets.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)

## --------------------------------------------------------------------

# This is the main compilation unit and it defines the main module; it
# depends upon all the import libraries of the other modules.
#
lib/mmck.pfds.$(OBJEXT): lib/mmck.pfds.scm $(MMCK_PFDS_IMPORT_DEPS) $(MMCK_OBJECTS_DEPS)
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) $(@) $(<)
	@$(MV) mmck.pfds.import.scm lib/mmck.pfds.import.scm

lib/mmck.pfds.import.scm: lib/mmck.pfds.$(OBJEXT)

lib/mmck.pfds.import.$(MMCK_SOEXT): lib/mmck.pfds.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)

## --------------------------------------------------------------------

lib/mmck.pfds.$(MMCK_SOEXT): $(MMCK_PFDS_RUNTIME_OBJECTS)
	$(MMCK_LIB_ENV) $(CSC_LINK_LIBRARY) $(@) $(^)


#### shared libraries installation

# All the import shared libraries that we want to installe.
#
MMCK_ALL_IMPORT_SHARED_LIBS	= \
	$(MMCK_PFDS_IMPORT_SHARED_LIBS)

# All the runtime shared libraries that we want to install.
#
MMCK_ALL_RUNTIME_SHARED_LIBS	= \
	$(MMCK_PFDS_RUNTIME_SHARED_LIBS)

# These variables are parsed by GNU Automake to build the main library
# targets and install them under the directory $(MMUX_CHICKEN_LIBDIR).
#
# We are shamelessly cheating by  installing them using GNU Automake's
# primary  suffix  "_SCRIPTS"; these  are  not  scripts!  Scripts  are
# installed with the permissions 0755, which, for some reason, is what
# we want  for shared libraries.
#
# We  could use  the  primary suffix  "_DATA", but  we  would get  the
# permissions 0644.
#
sharedlibdir		= $(MMUX_CHICKEN_LIBDIR)
sharedlib_SCRIPTS	= $(MMCK_ALL_RUNTIME_SHARED_LIBS) $(MMCK_ALL_IMPORT_SHARED_LIBS)


#### interface to "make check"
#
# Read "Parallel Test Harness" in the documentation of GNU Automake to
# understand how to use this interface for "make check".
#

EXTRA_DIST		+= \
	tests/test-version.scm			\
	tests/test-pfds-bbtrees.scm		\
	tests/test-pfds-deques.scm		\
	tests/test-pfds-dlists.scm		\
	tests/test-pfds-fingertrees.scm		\
	tests/test-pfds-heaps.scm		\
	tests/test-pfds-psqs.scm		\
	tests/test-pfds-queues.scm		\
	tests/test-pfds-sequences.scm		\
	tests/test-pfds-sets.scm		\
	tests/test-pfds-docs.scm

MMCK_CHECK_OBJECTS	= \
	tests/test-version.$(OBJEXT)		\
	tests/test-pfds-bbtrees.$(OBJEXT)	\
	tests/test-pfds-deques.$(OBJEXT)	\
	tests/test-pfds-dlists.$(OBJEXT)	\
	tests/test-pfds-fingertrees.$(OBJEXT)	\
	tests/test-pfds-heaps.$(OBJEXT)		\
	tests/test-pfds-psqs.$(OBJEXT)		\
	tests/test-pfds-queues.$(OBJEXT)	\
	tests/test-pfds-sequences.$(OBJEXT)	\
	tests/test-pfds-sets.$(OBJEXT)		\
	tests/test-pfds-docs.$(OBJEXT)

MMCK_CHECK_PROGS	= \
	tests/test-version.exe			\
	tests/test-pfds-bbtrees.exe		\
	tests/test-pfds-deques.exe		\
	tests/test-pfds-dlists.exe		\
	tests/test-pfds-fingertrees.exe		\
	tests/test-pfds-heaps.exe		\
	tests/test-pfds-psqs.exe		\
	tests/test-pfds-queues.exe		\
	tests/test-pfds-sequences.exe		\
	tests/test-pfds-sets.exe		\
	tests/test-pfds-docs.exe

CLEANFILES	+= $(MMCK_CHECK_PROGS) $(MMCK_CHECK_OBJECTS)

if MMCK_TESTING_ENABLED
TESTS		= $(MMCK_CHECK_PROGS)
endif

MMCK_CHECK_DEPS	+= $(MMCK_ALL_RUNTIME_SHARED_LIBS) $(MMCK_ALL_IMPORT_LIBS)

## --------------------------------------------------------------------

tests/test-version.$(OBJEXT): tests/test-version.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-version.exe: tests/test-version.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

tests/test-pfds-bbtrees.$(OBJEXT): tests/test-pfds-bbtrees.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-pfds-bbtrees.exe: tests/test-pfds-bbtrees.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

tests/test-pfds-deques.$(OBJEXT): tests/test-pfds-deques.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-pfds-deques.exe: tests/test-pfds-deques.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

tests/test-pfds-dlists.$(OBJEXT): tests/test-pfds-dlists.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-pfds-dlists.exe: tests/test-pfds-dlists.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

tests/test-pfds-fingertrees.$(OBJEXT): tests/test-pfds-fingertrees.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-pfds-fingertrees.exe: tests/test-pfds-fingertrees.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

tests/test-pfds-heaps.$(OBJEXT): tests/test-pfds-heaps.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-pfds-heaps.exe: tests/test-pfds-heaps.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

tests/test-pfds-psqs.$(OBJEXT): tests/test-pfds-psqs.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-pfds-psqs.exe: tests/test-pfds-psqs.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

tests/test-pfds-queues.$(OBJEXT): tests/test-pfds-queues.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-pfds-queues.exe: tests/test-pfds-queues.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

tests/test-pfds-sequences.$(OBJEXT): tests/test-pfds-sequences.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-pfds-sequences.exe: tests/test-pfds-sequences.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

tests/test-pfds-sets.$(OBJEXT): tests/test-pfds-sets.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-pfds-sets.exe: tests/test-pfds-sets.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

tests/test-pfds-docs.$(OBJEXT): tests/test-pfds-docs.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-pfds-docs.exe: tests/test-pfds-docs.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

### end of file
