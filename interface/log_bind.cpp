/******************************************************************************
 * log_bind.cpp - bindings for Ogre::Log
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
#include <OgreLog.h>
#include <OgreStringConverter.h>

class LogListenerBind : public Ogre::LogListener
{
public:
    LogListenerBind(LogListenerEvent lle) : logListenerHandle(lle)
    {
    }

    void messageLogged(const Ogre::String &message, Ogre::LogMessageLevel lml, bool maskDebug, const Ogre::String &logName, bool &skipThisMessage)
    {

        if(logListenerHandle)
        {
            log_message_level converted = ogre_lml_to_llcoi_lml(lml);
            logListenerHandle(message.c_str(), converted, maskDebug, logName.c_str(), skipThisMessage);
        }
    }

    LogListenerEvent logListenerHandle;
};

// Same as above, only it holds user data as well.
class LogListenerCTX: public Ogre::LogListener
{
public:
    LogListenerCTX(LogListenerCtx lle, void* data) : callback(lle), userdata(data)
    {
    }

    void messageLogged(const Ogre::String &message, Ogre::LogMessageLevel lml, bool maskDebug, const Ogre::String &logName, bool &skipThisMessage)
    {
        if(callback)
        {
            log_message_level converted = ogre_lml_to_llcoi_lml(lml);
            callback(message.c_str(), converted, maskDebug, logName.c_str(), skipThisMessage, userdata);
        }
    }
    void* userdata;
    LogListenerCtx callback;
};

//Log::addListener
LogListenerHandle add_log_listener_ctx(LogListenerCtx lle, LogHandle handle, void* userdata)
{
    Ogre::Log* log = reinterpret_cast<Ogre::Log*>(handle);
    LogListenerCTX *listener = new LogListenerCTX(lle, userdata);
    log->addListener(listener);
    return reinterpret_cast<LogListenerHandle>(listener);
}

//Log::addListener
void remove_log_listener_ctx(LogListenerHandle llh, LogHandle handle, void* userdata)
{
    Ogre::Log* log = reinterpret_cast<Ogre::Log*>(handle);
    LogListenerCTX *listener = reinterpret_cast<LogListenerCTX*>(llh);
    log->removeListener(listener);
    delete listener;
}

void log_log_message(LogHandle handle, const char *message, log_message_level lml, int maskDebug)
{
    Ogre::Log* l = reinterpret_cast<Ogre::Log*>(handle);

    Ogre::LogMessageLevel converted = llcoi_lml_to_ogre_lml(lml);
    l->logMessage(Ogre::StringConverter::toString(message), converted, maskDebug);
}

//Log::addListener
LogListenerHandle add_log_listener(LogListenerEvent log_event, LogHandle log_handle)
{
    Ogre::Log* log = reinterpret_cast<Ogre::Log*>(log_handle);
    LogListenerBind *listener = new LogListenerBind(log_event);
    log->addListener(listener);
}


//Log::removeListener
void remove_log_listener(LogListenerHandle llh, LogHandle log_handle)
{
    Ogre::Log* log = reinterpret_cast<Ogre::Log*>(log_handle);
    LogListenerBind* listener = reinterpret_cast<LogListenerBind*>(llh);
    log->removeListener(listener);
    delete listener;
}
