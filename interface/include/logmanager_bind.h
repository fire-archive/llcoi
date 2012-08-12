/******************************************************************************
 * logmanager_bind.h - bindings for Ogre::LogManager
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
#ifndef LOGMANAGER_BIND_H
#define LOGMANAGER_BIND_H

#include "ogre_interface.h"
#include "log_bind.h"

#define LogManagerHandle void*

// LogManager
DLL LogManagerHandle create_log_manager();

// LogManager::getSingletonPtr
DLL LogManagerHandle get_log_manager();

//LogManager::getLog
DLL LogHandle logmanager_get_log(const char* name);

//LogManager::getDefaultLog
DLL LogHandle logmanager_get_default_log();

//LogManager::setDefaultLog
DLL LogHandle logmanager_set_default_log(LogHandle log_handle);

//LogManager::createLog
DLL LogHandle logmanager_create_log(const char* name, int default_log, int debugger_output, int suppress_file_output);

// n.b., Allows for finer grained control over the log messages at the cost of
// having to supply all these variables. If you don't need this control,
// use log_message above.
//LogManager::logMessage
DLL void logmanager_log_message(const char* message, log_message_level lml, int maskDebug, const char* log_name, int skip_message);

//LogManager::setLogDetail
DLL void logmanager_set_log_detail(logging_level lvl);

//LogManager::destroyLog
DLL void logmanager_destroy_log(const char* name);

//LogManager::destroyLog
DLL void logmanager_destroy_log_by_handle(LogHandle log_handle);

#endif
