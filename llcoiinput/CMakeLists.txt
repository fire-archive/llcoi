#/******************************************************************************
# * CMakeLists.txt - llcoiInput CMake script
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

project(llcoiInput)

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmakemodules;${CMAKE_MODULE_PATH}")
 
set(LLCOIINPUT_VERSION_STRING "0.0.1")
set(abi_version ${LLCOIINPUT_VERSION_STRING})

if (CMAKE_BUILD_TYPE STREQUAL "")
  # CMake defaults to leaving CMAKE_BUILD_TYPE empty. This screws up
  # differentiation between debug and release builds.
  set(CMAKE_BUILD_TYPE "RelWithDebInfo" CACHE STRING "Choose the type of build, options are: None (CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug Release RelWithDebInfo MinSizeRel." FORCE)
endif ()
 
set(CMAKE_DEBUG_POSTFIX "_d")
 
if(WIN32)
	find_package(D3D9 REQUIRED)
	find_package(DInput REQUIRED)
	find_package(DXGuid REQUIRED)
endif()

set(HDRS
	./include/openinput.h
	./include/openinput_action.h
	./include/openinput_api.h
	./include/openinput_events.h
	./include/openinput_joystick.h
	./include/openinput_keyboard.h
	./include/openinput_mouse.h
	./include/openinput_types.h
	./src/private.h
	./src/internal.h
	./src/bootstrap.h
)

set(SRCS
	./src/action.c
	./src/appstate.c
	./src/debug.c
	./src/device.c
	./src/events.c
	./src/joystick.c
	./src/keyboard.c
	./src/keynames.c
	./src/main.c
	./src/mouse.c
	./src/queue.c
)

if (WIN32)
	list(APPEND SRCS ./src/win32/win32.c)
	list(APPEND HDRS ./src/win32/win32.h)
	list(APPEND SRCS ./src/win32/win32_events.c)
	list(APPEND SRCS ./src/win32/win32_translate.c)
	list(APPEND SRCS ./src/dx9/dx9.c)
	list(APPEND HDRS ./src/dx9/dx9.h)
	list(APPEND SRCS ./src/dx9/dx9_events.c)
	list(APPEND SRCS ./src/dx9/dx9_translate.c)
elseif (UNIX)
	list(APPEND SRCS ./src/linuxjoy/linuxjoy.c)
	list(APPEND HDRS ./src/linuxjoy/linuxjoy.h)
	list(APPEND HDRS ./src/linuxjoy/linuxjoy_devs.h)
	list(APPEND SRCS ./src/unixsignal/unixsignal.c)
	list(APPEND HDRS ./src/unixsignal/unixsignal.h)
	list(APPEND SRCS ./src/x11/x11.c)
	list(APPEND HDRS ./src/x11/x11.h)
	list(APPEND SRCS ./src/x11/x11_events.c)
	list(APPEND SRCS ./src/x11/x11_translate.c)
endif ()

add_library(llcoiInput SHARED ${HDRS} ${SRCS})

set_target_properties(llcoiInput PROPERTIES DEBUG_POSTFIX _d)

if(UNIX)
  set_target_properties( llcoiInput PROPERTIES OUTPUT_NAME "llcoiInput" VERSION
${LLCOIINPUT_VERSION_STRING} SOVERSION ${abi_version} )
endif()

include_directories(./include ./src)

if(WIN32)
include_directories(./include/win32 ./include/dx9)
include_directories(${D3D9_INCLUDE_DIR} ${DINPUT_INCLUDE_DIR})
target_link_libraries(llcoiInput ${DINPUT_LIBRARY} ${D3D9_LIBRARY} ${DXGUID_LIBRARY})
set_target_properties(llcoiInput PROPERTIES 
	COMPILE_DEFINITIONS ENABLE_WIN32)
set_target_properties(llcoiInput PROPERTIES 
	COMPILE_DEFINITIONS ENABLE_DX9)
else()
endif()

set(LIBRARY_OUTPUT_PATH "${BUILD_ROOTDIR}/lib")

# post-build copy for win32
if(WIN32 AND NOT MINGW)
	add_custom_command( TARGET llcoiInput PRE_BUILD
		COMMAND if not exist ..\\test\\dist\\bin mkdir ..\\test\\dist\\bin )
	add_custom_command( TARGET llcoiInput POST_BUILD
		COMMAND copy \"$(TargetPath)\" ..\\test\\dist\\bin )
endif(WIN32 AND NOT MINGW)

if(WIN32)
	install(TARGETS llcoiInput
		RUNTIME DESTINATION bin
		CONFIGURATIONS All)
endif()
