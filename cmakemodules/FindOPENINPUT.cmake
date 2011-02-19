#-------------------------------------------------------------------
# 
#-------------------------------------------------------------------

# - Try to find OpenInput
# Once done, this will define
#
#  OPENINPUT_FOUND - system has OpenInput
#  OPENINPUT_INCLUDE_DIRS - the OpenInput include directories 
#  OPENINPUT_LIBRARIES - link these to use OpenInput

include(FindPkgMacros)
findpkg_begin(OPENINPUT)

# Get path, convert backslashes as ${ENV_${var}}
getenv_path(OPENINPUT_HOME)

# construct search paths
set(OPENINPUT_PREFIX_PATH ${OPENINPUT_HOME} ${ENV_OPENINPUT_HOME} /usr/local /usr/local/include /usr/local/lib /usr/include /usr/lib /usr/local/include/openinput /usr/include/openinput /usr/lib/openinput /usr/local/lib/openinput)
create_search_paths(OPENINPUT)
# redo search if prefix path changed
clear_if_changed(OPENINPUT_PREFIX_PATH
  OPENINPUT_LIBRARY_FWK
  OPENINPUT_LIBRARY_REL
  OPENINPUT_LIBRARY_DBG
  OPENINPUT_INCLUDE_DIR
)

set(OPENINPUT_LIBRARY_NAMES openinput)
get_debug_names(OPENINPUT_LIBRARY_NAMES)

use_pkgconfig(OPENINPUT_PKGC OPENINPUT)

findpkg_framework(OPENINPUT)

find_path(OPENINPUT_INCLUDE_DIR NAMES openinput.h HINTS ${OPENINPUT_INC_SEARCH_PATH} ${OPENINPUT_PKGC_INCLUDE_DIRS} PATH_SUFFIXES openinput)
find_library(OPENINPUT_LIBRARY_REL NAMES ${OPENINPUT_LIBRARY_NAMES} HINTS ${OPENINPUT_LIB_SEARCH_PATH} ${OPENINPUT_PKGC_LIBRARY_DIRS} PATH_SUFFIXES "" release relwithdebinfo minsizerel)
find_library(OPENINPUT_LIBRARY_DBG NAMES ${OPENINPUT_LIBRARY_NAMES_DBG} HINTS ${OPENINPUT_LIB_SEARCH_PATH} ${OPENINPUT_PKGC_LIBRARY_DIRS} PATH_SUFFIXES "" debug)
make_library_set(OPENINPUT_LIBRARY)

findpkg_finish(OPENINPUT)
add_parent_dir(OPENINPUT_INCLUDE_DIRS OPENINPUT_INCLUDE_DIR)

