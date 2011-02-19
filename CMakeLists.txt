#/******************************************************************************
# * CMakeLists.txt - main llcoi CMake script
# ******************************************************************************
# * This file is part of
# *     __ __              _ 
# *    / // /_____ ____   (_)
# *   / // // ___// __ \ / / 
# *  / // // /__ / /_/ // /  
# * /_//_/ \___/ \____//_/   
# *                          
# * Low Level C Ogre Interface (llcoi)
# *
# * See http://code.google.com/p/llcoi/ for more information.
# *
# * Copyright (c) 2011, Llcoi Team
# * 
# * License: MIT
# * 
# * Permission is hereby granted, free of charge, to any person obtaining a copy
# * of this software and associated documentation files (the "Software"), to deal
# * in the Software without restriction, including without limitation the rights
# * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# * copies of the Software, and to permit persons to whom the Software is
# * furnished to do so, subject to the following conditions:
# * 
# * The above copyright notice and this permission notice shall be included in
# * all copies or substantial portions of the Software.
# * 
# * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# * THE SOFTWARE.
# ******************************************************************************/
cmake_minimum_required(VERSION 2.8)

project(llcoi)

option(LLCOI_DYNAMIC_BUILD "Build OgreInterface as shared library" TRUE)
option(LLCOI_TEST_USE_ALLEGRO "Build Test program with Allegro instead of OpenInput)" FALSE)

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmakemodules;${CMAKE_MODULE_PATH}")

set(BUILD_ROOTDIR "${CMAKE_CURRENT_BINARY_DIR}")
set(CMAKE_INSTALL_PREFIX "${BUILD_ROOTDIR}/test/dist")

include_directories(interface)

add_subdirectory(interface)
add_subdirectory(llcoiinput)
add_subdirectory(test)

