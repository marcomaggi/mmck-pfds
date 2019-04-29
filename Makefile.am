## Process this file with automake to produce Makefile.in

include meta/automake/mmck-rules.make

## --------------------------------------------------------------------

nodist_pkgconfig_DATA	= meta/scripts/mmck-template.pc


#### documentation

info_TEXINFOS		= doc/mmck-template.texi
doc_mmck_template_TEXINFOS = \
	doc/macros.texi			\
	doc/lgpl-3.0.texi		\
	doc/fdl-1.3.texi


#### library build targets

# All the source files of the extension library.
#
MMCK_TEMPLATE_MODULES		= \
	lib/mmck.template.alpha.scm		\
	lib/mmck.template.version.scm		\
	lib/mmck.template.scm

# All the object files that must be linked in the extension library.
#
MMCK_TEMPLATE_RUNTIME_OBJECTS	= \
	lib/mmck.template.alpha.$(OBJEXT)	\
	lib/mmck.template.version.$(OBJEXT)	\
	lib/mmck.template.$(OBJEXT)

# All the source files of all the modules' import libraries.
#
MMCK_TEMPLATE_IMPORT_SOURCES	= \
	lib/mmck.template.alpha.import.scm	\
	lib/mmck.template.version.import.scm	\
	lib/mmck.template.import.scm

# The module's import libraries needed to compile main module's object
# file.  It does not include the main import library:
#
#   lib/mmck.template.import.$(MMCK_SOEXT)
#
MMCK_TEMPLATE_IMPORT_DEPS	= \
	lib/mmck.template.alpha.import.$(MMCK_SOEXT)	\
	lib/mmck.template.version.import.$(MMCK_SOEXT)

# The import  libraries needed  to compile new  code using  the shared
# library  "(mmck template)".   We need  to build  and install  all of
# them.
#
MMCK_TEMPLATE_IMPORT_SHARED_LIBS	= \
	lib/mmck.template.alpha.import.$(MMCK_SOEXT)	\
	lib/mmck.template.version.import.$(MMCK_SOEXT)	\
	lib/mmck.template.import.$(MMCK_SOEXT)

# This is the "(mmck template)" shared library file pathname.
#
MMCK_TEMPLATE_RUNTIME_SHARED_LIBS	= lib/mmck.template.$(MMCK_SOEXT)

## --------------------------------------------------------------------

EXTRA_DIST	+= $(MMCK_TEMPLATE_MODULES)

CLEANFILES	+= \
	$(MMCK_TEMPLATE_RUNTIME_SHARED_LIBS)	\
	$(MMCK_TEMPLATE_RUNTIME_OBJECTS)	\
	$(MMCK_TEMPLATE_IMPORT_SHARED_LIBS)	\
	$(MMCK_TEMPLATE_IMPORT_SOURCES)

## --------------------------------------------------------------------

lib/mmck.template.version.$(OBJEXT): lib/mmck.template.version.scm $(MMCK_OBJECTS_DEPS)
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) $(@) $(<) -prologue lib/config.scm
	@$(MV) mmck.template.version.import.scm lib/mmck.template.version.import.scm

lib/mmck.template.version.import.scm: lib/mmck.template.version.$(OBJEXT)

lib/mmck.template.version.import.$(MMCK_SOEXT): lib/mmck.template.version.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)

## --------------------------------------------------------------------

lib/mmck.template.alpha.$(OBJEXT): lib/mmck.template.alpha.scm $(MMCK_OBJECTS_DEPS)
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) $(@) $(<)
	@$(MV) mmck.template.alpha.import.scm lib/mmck.template.alpha.import.scm

lib/mmck.template.alpha.import.scm: lib/mmck.template.alpha.$(OBJEXT)

lib/mmck.template.alpha.import.$(MMCK_SOEXT): lib/mmck.template.alpha.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)

## --------------------------------------------------------------------

# This is the main compilation unit and it defines the main module; it
# depends upon all the import libraries of the other modules.
#
lib/mmck.template.$(OBJEXT): lib/mmck.template.scm $(MMCK_TEMPLATE_IMPORT_DEPS) $(MMCK_OBJECTS_DEPS)
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) $(@) $(<)
	@$(MV) mmck.template.import.scm lib/mmck.template.import.scm

lib/mmck.template.import.scm: lib/mmck.template.$(OBJEXT)

lib/mmck.template.import.$(MMCK_SOEXT): lib/mmck.template.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)

## --------------------------------------------------------------------

lib/mmck.template.$(MMCK_SOEXT): $(MMCK_TEMPLATE_RUNTIME_OBJECTS)
	$(MMCK_LIB_ENV) $(CSC_LINK_LIBRARY) $(@) $(^)


#### shared libraries installation

# All the import shared libraries that we want to installe.
#
MMCK_ALL_IMPORT_SHARED_LIBS	= \
	$(MMCK_TEMPLATE_IMPORT_SHARED_LIBS)

# All the runtime shared libraries that we want to install.
#
MMCK_ALL_RUNTIME_SHARED_LIBS	= \
	$(MMCK_TEMPLATE_RUNTIME_SHARED_LIBS)

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
	tests/test-version.scm		\
	tests/test-alpha.scm

MMCK_CHECK_OBJECTS	= \
	tests/test-version.$(OBJEXT)	\
	tests/test-alpha.$(OBJEXT)

MMCK_CHECK_PROGS	= \
	tests/test-version.exe		\
	tests/test-alpha.exe

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

tests/test-alpha.$(OBJEXT): tests/test-alpha.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-alpha.exe: tests/test-alpha.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

### end of file
