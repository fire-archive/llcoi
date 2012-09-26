/******************************************************************************
 * timer_bind.cpp - bindings for Ogre::Timer
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
#include "ogre_interface.h"
#include "timer_bind.h"
#include <OgreRoot.h>
#include <OgreTimer.h>

int timer_set_option(TimerHandle handle, const char* key, void* value)
{
    Ogre::Timer* timer = reinterpret_cast<Ogre::Timer*>(handle);
    return timer->setOption(Ogre::String(key), value);
}

unsigned long timer_get_milliseconds(TimerHandle handle)
{
    Ogre::Timer* timer = reinterpret_cast<Ogre::Timer*>(handle);
    return timer->getMilliseconds();
}

unsigned long timer_get_microseconds(TimerHandle handle)
{
    Ogre::Timer* timer = reinterpret_cast<Ogre::Timer*>(handle);
    return timer->getMicroseconds();
}

unsigned long timer_get_milliseconds_cpu(TimerHandle handle)
{
    Ogre::Timer* timer = reinterpret_cast<Ogre::Timer*>(handle);
    return timer->getMillisecondsCPU();
}

unsigned long timer_get_microseconds_cpu(TimerHandle handle)
{
    Ogre::Timer* timer = reinterpret_cast<Ogre::Timer*>(handle);
    return timer->getMicrosecondsCPU();
}

void timer_reset(TimerHandle handle)
{
    Ogre::Timer* timer = reinterpret_cast<Ogre::Timer*>(handle);
    timer->reset();
}

