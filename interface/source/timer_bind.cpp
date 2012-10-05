/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
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

