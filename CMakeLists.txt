#/*
#-----------------------------------------------------------------------------
#Filename:    CMakeLists.txt
#-----------------------------------------------------------------------------
#
#This source file is part of the
#   ___                 __    __ _ _    _ 
#  /___\__ _ _ __ ___  / / /\ \ (_) | _(_)
# //  // _` | '__/ _ \ \ \/  \/ / | |/ / |
#/ \_// (_| | | |  __/  \  /\  /| |   <| |
#\___/ \__, |_|  \___|   \/  \/ |_|_|\_\_|
#      |___/                              
#      Tutorial Framework
#      http://www.ogre3d.org/tikiwiki/
#-----------------------------------------------------------------------------
#*/
cmake_minimum_required(VERSION 2.8)

project(llcoi)

option(LLCOI_DYNAMIC_BUILD "Build OgreInterface as shared library" TRUE)

set(BUILD_ROOTDIR "${CMAKE_CURRENT_BINARY_DIR}")
set(CMAKE_INSTALL_PREFIX "${BUILD_ROOTDIR}/test/dist")

include_directories(interface)

add_subdirectory(interface)
add_subdirectory(test)

