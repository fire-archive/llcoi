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

#include "ois_interface.h"

DLL InputManagerHandle create_input_system(unsigned int window_handle);

// For sending parameters to the input constructor.
DLL InputManagerHandle create_input_system_ex(ParamListHandle handle);

DLL void destroy_input_system(InputManagerHandle input_manager_handle);

DLL MouseInputHandle create_mouse_object(InputManagerHandle input_manager_handle, int buffered);

DLL KeyboardInputHandle create_keyboard_object(InputManagerHandle input_manager_handle, int buffered);

DLL void destroy_mouse_object(InputManagerHandle input_manager_handle, MouseInputHandle mouse_handle);

DLL void destroy_keyboard_object(InputManagerHandle input_manager_handle, KeyboardInputHandle keyboard_handle);
