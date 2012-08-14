/******************************************************************************
 * log_bind.h - bindings for Ogre::Log
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
#ifndef LOG_BIND_H
#define LOG_BIND_H


#include "ogre_interface.h"

#define LogHandle void*
#define LogListenerHandle void*

typedef void(*LogListenerEvent)(const char* message, log_message_level lml, int maskDebug, const char* log_name, int skip_message);

// Same as LogListenerEvent but allows the client to
// send additional data via a void pointer. We assume the
// client knows what they're doing if they use this. (:
typedef void(*LogListenerCtx)(const char* message, log_message_level lml, int maskDebug, const char* log_name, int skip_message, void* userdata);

//Log::addListener
DLL LogListenerHandle add_log_listener(LogListenerEvent log_event, LogHandle log_handle);

//Log::addListener
DLL LogListenerHandle add_log_listener_ctx(LogListenerCtx log_event, LogHandle log_handle, void* userdata);

//Log::removeListener
DLL void remove_log_listener(LogListenerHandle handle, LogHandle log_handle);

//Log::addListener
DLL void remove_log_listener_ctx(LogListenerHandle handle, LogHandle log_handle);

//Log::logMessage
DLL void log_log_message(LogHandle handle, const char *message, log_message_level lml, int maskDebug);


#endif
