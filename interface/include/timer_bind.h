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

// Ogre::Timer
DLL int timer_set_option(TimerHandle handle, const char* key, void* value);
DLL unsigned long timer_get_milliseconds(TimerHandle handle);
DLL unsigned long timer_get_microseconds(TimerHandle handle);
DLL unsigned long timer_get_milliseconds_cpu(TimerHandle handle);
DLL unsigned long timer_get_microseconds_cpu(TimerHandle handle);
DLL void timer_reset(TimerHandle handle);
