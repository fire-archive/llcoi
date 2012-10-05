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

DLL int keyboard_is_key_down(KeyboardInputHandle keyboard_handle, KeyCode key_code);

DLL int keyboard_is_modifier_down(KeyboardInputHandle keyboard_handle, KeyModifier key_modifier);

DLL void keyboard_set_buffered(KeyboardInputHandle keyboard_handle, int buffered);

DLL void keyboard_capture(KeyboardInputHandle keyboard_handle);
