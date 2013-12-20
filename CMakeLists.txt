cmake_minimum_required(VERSION 2.8)


# project name
project(llcoi)


####################
#### PARAMETERS ####
####################
set(OGRE_BINARIES_DIR ${CMAKE_INSTALL_PREFIX}/lib CACHE PATH "where the ogre binaries are, i.e. libOgreMain.so")
set(OGRE_HEADERS_DIR ${CMAKE_INSTALL_PREFIX}/include/OGRE CACHE PATH "where the ogre header files are, i.e. OgreRoot.h")
set(OGRE_BUILDSETTINGS_DIR ${OGRE_HEADERS_DIR} CACHE PATH "where OgreBuildSettings.h is located (created by Ogre cmake)")
set(OIS_BINARIES_DIR ${OGRE_BINARIES_DIR} CACHE PATH "where the OIS binaries are, i.e. libOIS.so")
set(OIS_HEADERS_DIR ${CMAKE_INSTALL_PREFIX}/include/OIS CACHE PATH "where the OIS header files are, i.e. OIS.h")

# do not default to empty build type
if (CMAKE_BUILD_TYPE STREQUAL "")
  set(CMAKE_BUILD_TYPE "RelWithDebInfo" CACHE STRING "Choose the type of build, options are: None (CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug Release RelWithDebInfo MinSizeRel." FORCE)
endif ()


# TODO: boost


#################
#### COMPILE ####
#################
set(LLCOI_HEADER_FILES
  ./interface/include/axisalignedbox_bind.h
  ./interface/include/binding_utils.h
  ./interface/include/bone_bind.h
  ./interface/include/camera_bind.h
  ./interface/include/colourvalue_bind.h
  ./interface/include/configfile_bind.h
  ./interface/include/entity_bind.h
  ./interface/include/framelistener_bind.h
  ./interface/include/fwd.h
  ./interface/include/inputmanager_bind.h
  ./interface/include/keyboard_bind.h
  ./interface/include/light_bind.h
  ./interface/include/llcoi.h
  ./interface/include/log_bind.h
  ./interface/include/logmanager_bind.h
  ./interface/include/main.h
  ./interface/include/manualobject_bind.h
  ./interface/include/material_bind.h
  ./interface/include/materialmanager_bind.h
  ./interface/include/meshmanager_bind.h
  ./interface/include/mouse_bind.h
  ./interface/include/movableobject_bind.h
  ./interface/include/node_bind.h
  ./interface/include/ogre_interface.h
  ./interface/include/ois_interface.h
  ./interface/include/overlay_bind.h
  ./interface/include/overlaycontainer_bind.h
  ./interface/include/overlayelement_bind.h
  ./interface/include/overlayelementfactory_bind.h
  ./interface/include/overlaymanager_bind.h
  ./interface/include/paneloverlayelement_bind.h
  ./interface/include/pass_bind.h
  ./interface/include/plane_bind.h
  ./interface/include/planeboundedvolume_bind.h
  ./interface/include/ray_bind.h
  ./interface/include/renderoperation_bind.h
  ./interface/include/rendersystem_bind.h
  ./interface/include/renderwindow_bind.h
  ./interface/include/resource_bind.h
  ./interface/include/resourcegroupmanager_bind.h
  ./interface/include/resourcemanager_bind.h
  ./interface/include/root_bind.h
  ./interface/include/scenemanager_bind.h
  ./interface/include/scenenode_bind.h
  ./interface/include/scenequery_bind.h
  ./interface/include/skeleton_bind.h
  ./interface/include/skeletoninstance_bind.h
  ./interface/include/sphere_bind.h
  ./interface/include/tagpoint_bind.h
  ./interface/include/technique_bind.h
  ./interface/include/textareaoverlayelement_bind.h
  ./interface/include/texturemanager_bind.h
  ./interface/include/timer_bind.h
  ./interface/include/vector3_bind.h
  ./interface/include/vertexindexdata_bind.h
  ./interface/include/viewport_bind.h
  ./interface/include/windoweventlistener_bind.h
)

set(LLCOI_SOURCE_FILES
  ./interface/source/axisalignedbox_bind.cpp
  ./interface/source/binding_utils.cpp
  ./interface/source/bone_bind.cpp
  ./interface/source/camera_bind.cpp
  ./interface/source/colourvalue_bind.cpp
  ./interface/source/configfile_bind.cpp
  ./interface/source/entity_bind.cpp
  ./interface/source/framelistener_bind.cpp
  ./interface/source/inputmanager_bind.cpp
  ./interface/source/keyboard_bind.cpp
  ./interface/source/light_bind.cpp
  ./interface/source/log_bind.cpp
  ./interface/source/logmanager_bind.cpp
  ./interface/source/main.cpp
  ./interface/source/manualobject_bind.cpp
  ./interface/source/material_bind.cpp
  ./interface/source/materialmanager_bind.cpp
  ./interface/source/meshmanager_bind.cpp
  ./interface/source/mouse_bind.cpp
  ./interface/source/movableobject_bind.cpp
  ./interface/source/node_bind.cpp
  ./interface/source/overlay_bind.cpp
  ./interface/source/overlaycontainer_bind.cpp
  ./interface/source/overlayelement_bind.cpp
  ./interface/source/overlayelementfactory_bind.cpp
  ./interface/source/overlaymanager_bind.cpp
  ./interface/source/paneloverlayelement_bind.cpp
  ./interface/source/pass_bind.cpp
  ./interface/source/plane_bind.cpp
  ./interface/source/planeboundedvolume_bind.cpp
  ./interface/source/ray_bind.cpp
  ./interface/source/renderoperation_bind.cpp
  ./interface/source/rendersystem_bind.cpp
  ./interface/source/renderwindow_bind.cpp
  ./interface/source/resource_bind.cpp
  ./interface/source/resourcegroupmanager_bind.cpp
  ./interface/source/resourcemanager_bind.cpp
  ./interface/source/root_bind.cpp
  ./interface/source/scenemanager_bind.cpp
  ./interface/source/scenenode_bind.cpp
  ./interface/source/scenequery_bind.cpp
  ./interface/source/skeleton_bind.cpp
  ./interface/source/skeletoninstance_bind.cpp
  ./interface/source/sphere_bind.cpp
  ./interface/source/tagpoint_bind.cpp
  ./interface/source/technique_bind.cpp
  ./interface/source/textareaoverlayelement_bind.cpp
  ./interface/source/texturemanager_bind.cpp
  ./interface/source/timer_bind.cpp
  ./interface/source/vector3_bind.cpp
  ./interface/source/vertexindexdata_bind.cpp
  ./interface/source/viewport_bind.cpp
  ./interface/source/windoweventlistener_bind.cpp
  )


include_directories(${OGRE_HEADERS_DIR} ${OGRE_BUILDSETTINGS_DIR} ${OIS_HEADERS_DIR} "${CMAKE_SOURCE_DIR}/interface/include")
link_directories(${OGRE_BINARIES_DIR} ${OIS_BINARIES_DIR} )

add_definitions(-Dllcoi_EXPORTS)
add_library(llcoi SHARED ${LLCOI_HEADER_FILES} ${LLCOI_SOURCE_FILES})
set(CMAKE_LIBRARY_PATH ${CMAKE_INSTALL_PREFIX}/lib/RelWithDebInfo ${CMAKE_INSTALL_PREFIX}/lib/Release)
target_link_libraries(llcoi OgreMain OIS OgreOverlay)
# set(CMAKE_C++_CREATE_SHARED_LIBRARY 1)
set_target_properties(llcoi PROPERTIES LINKER_LANGUAGE CXX)

set_target_properties(llcoi PROPERTIES OUTPUT_NAME "llcoi" VERSION 0.1 SOVERSION 0.1)

IF(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
install (TARGETS llcoi DESTINATION bin)
ENDIF()
IF(NOT ${CMAKE_SYSTEM_NAME} MATCHES "Windows")
install (TARGETS llcoi DESTINATION lib)
ENDIF()
FILE (GLOB files "${CMAKE_CURRENT_SOURCE_DIR}/interface/include/*.h")
install ( FILES ${files} DESTINATION include/llcoi)


# TODO: create FindOgre.cmake module
# TODO: create FindOIS.cmake module
# TODO: create FindBoost.cmake module
# NOTE! some of those might already exist in the ogre sources
#option(OGRE_INCLUDE "Directory containing Ogre header files")

# custom modules
#set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmakemodules/")
#set(BUILD_ROOTDIR "${CMAKE_CURRENT_BINARY_DIR}")

#include_directories(interface/include)
#add_subdirectory(interface)
