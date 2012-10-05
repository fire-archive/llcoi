/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#pragma once

#include "ogre_interface.h"

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
