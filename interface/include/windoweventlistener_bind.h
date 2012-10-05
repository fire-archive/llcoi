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

DLL void add_window_listener(RenderWindowHandle window_handle, WindowListenerEvent window_event);

DLL void remove_window_listener(RenderWindowHandle window_handle);

DLL WindowListenerHandle add_window_listener_ctx(RenderWindowHandle window_handle, WindowListenerEvent window_event, void* userdata);

DLL void remove_window_listener_ctx(RenderWindowHandle window_handle, WindowListenerHandle listener_handle);

DLL void pump_messages();
