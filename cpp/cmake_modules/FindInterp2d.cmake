



# - Try to find interp2d 
# Once done this will define
#  LIBINTERP2D_FOUND - System has LibInterp2d
#  LIBINTERP2D_INCLUDE_DIRS - The LibInterp2d include directories
#  LIBINTERP2D_LIBRARIES - The libraries needed to use LibInterp2d
#  LIBINTERP2D_DEFINITIONS - Compiler switches required for using LibInterp2d

find_package(PkgConfig)
pkg_check_modules(LIBINTERP2D QUIET interp2d)
set(LIBINTERP2D_DEFINITIONS ${LIBINTERP2D_CFLAGS_OTHER})


find_path(LIBINTERP2D_INCLUDE_DIR interp2d.h interp2d_spline.h
          HINTS ${LIBINTERP2D_INCLUDEDIR} ${LIBINTERP2D_INCLUDE_DIRS}
          )

find_library(LIBINTERP2D_LIBRARY NAMES libinterp2d.a libinterp2d_spline
	HINTS ${LIBINTERP2D_LIBDIR} ${LIBINTERP2D_LIBRARY_DIRS} )

set(LIBINTERP2D_LIBRARIES ${LIBINTERP2D_LIBRARY} )
set(LIBINTERP2D_INCLUDE_DIRS ${LIBINTERP2D_INCLUDE_DIR} )

include(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set LIBINTERP2D_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(LIBINTERP2D  DEFAULT_MSG
                                  LIBINTERP2D_LIBRARY LIBINTERP2D_INCLUDE_DIR)

mark_as_advanced(LIBINTERP2D_INCLUDE_DIR LIBINTERP2D_LIBRARY )
