/******************************************************************************
 * binding_utils.cpp - helper functions for LLCOI
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
#include "binding_utils.h"

log_message_level ogre_lml_to_llcoi_lml(Ogre::LogMessageLevel lml)
{
    log_message_level converted;

    switch(lml)
    {
        case Ogre::LML_TRIVIAL:
            converted = LML_TRIVIAL;
            break;

        case Ogre::LML_NORMAL:
            converted = LML_NORMAL;
            break;

        case Ogre::LML_CRITICAL:
            converted = LML_CRITICAL;
            break;
    }

    return converted;
}


Ogre::LogMessageLevel llcoi_lml_to_ogre_lml(log_message_level lml)
{
    Ogre::LogMessageLevel converted;

    switch(lml)
    {
        case LML_TRIVIAL:
            converted = Ogre::LML_TRIVIAL;
            break;

        case LML_NORMAL:
            converted = Ogre::LML_NORMAL;
            break;

        case Ogre::LML_CRITICAL:
            converted = Ogre::LML_CRITICAL;
            break;
    }
    return converted;
}


logging_level ogre_ll_to_llcoi_ll(Ogre::LoggingLevel ll);
Ogre::LoggingLevel llcoi_ll_to_ogre_ll(logging_level ll);


// XXX: Experimental code below. Some languages (D, for instance)
// can speak to C++ if namespaces are omitted and only single inheritance
// is used.

void LogListenerEx::messageLogged(const Ogre::String &message, Ogre::LogMessageLevel lml, bool maskDebug, const Ogre::String &logName, bool &skipThisMessage)
{
    if(emitter)
        emitter(message.c_str(), lml, maskDebug, logName.c_str(), skipThisMessage);
}

void LogListenerEx::setEmitter(LogEmitter em)
{
    this->emitter = em;
}

LogListenerEx* create_ex()
{
    LogListenerEx* listener = new LogListenerEx;
    return listener;
}

void set_emitter(LogListenerEx* ll, LogEmitter em)
{
    ll->setEmitter(em);
}

void destroy_ex(LogListenerEx* ll)
{
    delete ll;
}

//Log::addListener
void add_log_listener_ex(LogListenerEx* log_ex, LogHandle log_handle)
{
    Ogre::Log* log = reinterpret_cast<Ogre::Log*>(log_handle);
    log->addListener(log_ex);
}
