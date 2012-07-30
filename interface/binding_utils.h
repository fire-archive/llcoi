/******************************************************************************
 * binding_utils.h - utility functions for LLCOI
 ******************************************************************************
 * This file is part of
 *     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 *                          
 * Low Level C Ogre Interface (llcoi)
 *
 * See http://code.google.com/p/llcoi/ for more information.
 *
 * Copyright (c) 2011, Llcoi Team
 * 
 * License: MIT
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 ******************************************************************************/
#pragma once
#ifndef LLCOI_BINDING_UTILS
#define LLCOI_BINDING_UTILS
#include "ogre_interface.h" // as we can't forward declare enums. ):
#include <OgreLog.h>        // Ditto.
#include <OgreHardwareBuffer.h> // Ditto.


#if defined(LLCOI_BUILD_DYNAMIC)
#   if defined( WIN32 ) || defined( _WINDOWS )
#       ifndef llcoi_EXPORTS
#           define DLLX __declspec(dllimport)
#           define SYMX __declspec(dllimport)
#       else
#           define DLLX extern "C++" __declspec(dllexport)
#           define SYMX __declspec(dllexport)
#       endif
#   else
#       ifndef llcoi_EXPORTS
#           define DLLX
#           define SYMX
#       else
#           if defined( __GNUC__ ) && __GNUC__ >= 4
#               define DLLX extern "C++" __attribute__ ((visibility("default")))
#               define SYMX __attribute__ ((visibility("default")))
#           else
#               define DLLX extern "C++"
#               define SYMX __attribute__ ((visibility("default")))
#           endif
#       endif
#   endif
#else
#   if defined( LLCOI_BUILD_STATIC )
#       if defined( __GNUC__ ) && __GNUC__ >= 4
#           define DLLX extern "C++" __attribute__ ((visibility("default")))
#           define SYMX __attribute__ ((visibility("default")))
#       else
#           define DLLX extern "C++"
#       endif
#   else
#       define DLLX
#       define SYMX
#   endif
#endif


log_message_level ogre_lml_to_llcoi_lml(Ogre::LogMessageLevel lml);
Ogre::LogMessageLevel llcoi_lml_to_ogre_lml(log_message_level lml);

hardware_buffer_usage ogre_hbu_to_llcoi_hbu(Ogre::HardwareBuffer::Usage ogre_hbu);
Ogre::HardwareBuffer::Usage llcoi_hbu_to_ogre_hbu(hardware_buffer_usage llcoi_hbu);


logging_level ogre_ll_to_llcoi_ll(Ogre::LoggingLevel ll);
Ogre::LoggingLevel llcoi_ll_to_ogre_ll(logging_level ll);
#endif
