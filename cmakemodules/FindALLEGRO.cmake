#-------------------------------------------------------------------
# 
#-------------------------------------------------------------------

# - Try to find Allegro
# Once done, this will define
#
#  ALLEGRO_FOUND - system has Allegro
#  ALLEGRO_INCLUDE_DIRS - the Allegro include directories 
#  ALLEGRO_LIBRARIES - link these to use Allegro

include(FindPkgMacros)
findpkg_begin(ALLEGRO)

# Get path, convert backslashes as ${ENV_${var}}
getenv_path(ALLEGRO_HOME)

# construct search paths
set(ALLEGRO_PREFIX_PATH ${ALLEGRO_HOME} ${ENV_ALLEGRO_HOME} /usr/local /usr/local/include /usr/local/lib /usr/include /usr/lib /usr/local/include/allegro /usr/include/allegro /usr/lib/allegro /usr/local/lib/allegro)
create_search_paths(ALLEGRO)
# redo search if prefix path changed
clear_if_changed(ALLEGRO_PREFIX_PATH
  ALLEGRO_LIBRARY_REL
  ALLEGRO_LIBRARY_DBG
  ALLEGRO_INCLUDE_DIR
)

set(ALLEGRO_LIBRARY_NAMES allegro)
get_debug_names(ALLEGRO_LIBRARY_NAMES)

use_pkgconfig(ALLEGRO_PKGC ALLEGRO)

findpkg_framework(ALLEGRO)

find_path(ALLEGRO_INCLUDE_DIR NAMES allegro.h HINTS ${ALLEGRO_INC_SEARCH_PATH} ${ALLEGRO_PKGC_INCLUDE_DIRS} PATH_SUFFIXES allegro)
find_library(ALLEGRO_LIBRARY_REL NAMES ${ALLEGRO_LIBRARY_NAMES} HINTS ${ALLEGRO_LIB_SEARCH_PATH} ${ALLEGRO_PKGC_LIBRARY_DIRS} PATH_SUFFIXES "" release relwithdebinfo minsizerel)
find_library(ALLEGRO_LIBRARY_DBG NAMES ${ALLEGRO_LIBRARY_NAMES_DBG} HINTS ${ALLEGRO_LIB_SEARCH_PATH} ${ALLEGRO_PKGC_LIBRARY_DIRS} PATH_SUFFIXES "" debug)
make_library_set(ALLEGRO_LIBRARY)

findpkg_finish(ALLEGRO)
add_parent_dir(ALLEGRO_INCLUDE_DIRS ALLEGRO_INCLUDE_DIR)

