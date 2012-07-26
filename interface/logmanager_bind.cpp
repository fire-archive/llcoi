/******************************************************************************
 * logmanager_bind.cpp - bindings for Ogre::LogManager
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
#include <OgreLogManager.h>
#include <OgreLog.h>

LogManagerHandle create_log_manager()
{
    Ogre::LogManager* log_manager = new Ogre::LogManager;

    return reinterpret_cast<LogManagerHandle>(log_manager);
}
// LogManager::getSingletonPtr
LogManagerHandle get_log_manager()
{
    return reinterpret_cast<LogManagerHandle>(Ogre::LogManager::getSingletonPtr());
}

//LogManager::getLog
LogHandle logmanager_get_log(const char* name)
{
    return reinterpret_cast<LogHandle>(Ogre::LogManager::getSingletonPtr()->getLog(name));
}

//LogManager::getDefaultLog
LogHandle logmanager_get_default_log()
{
    return reinterpret_cast<LogHandle>(Ogre::LogManager::getSingletonPtr()->getDefaultLog());
}

//LogManager::setDefaultLog
LogHandle logmanager_set_default_log(LogHandle log_handle)
{
    return reinterpret_cast<LogHandle>(Ogre::LogManager::getSingletonPtr()->setDefaultLog(reinterpret_cast<Ogre::Log*>(log_handle)));
}

//LogManager::createLog
LogHandle logmanager_create_log(const char* name, int default_log, int debugger_output, int suppress_file_output)
{
    return reinterpret_cast<LogHandle>(Ogre::LogManager::getSingletonPtr()->createLog(name, default_log, debugger_output, suppress_file_output));
}

//LogManager::logMessage
void logmanager_log_message(const char* message, log_message_level lml, int maskDebug, const char* log_name, int skip_message)
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

    Ogre::LogManager::getSingletonPtr()->logMessage(message, converted, maskDebug);
}

//LogManager::destroyLog
void logmanager_destroy_log(const char* name)
{
    Ogre::LogManager::getSingletonPtr()->destroyLog(name);
}

//LogManager::destroyLog
void logmanager_destroy_log_by_handle(LogHandle log_handle)
{
    Ogre::Log* log = reinterpret_cast<Ogre::Log*>(log_handle);
    Ogre::LogManager::getSingletonPtr()->destroyLog(log);
}

//LogManager::setLogDetail
void logmanager_set_log_detail(logging_level lvl)
{
    Ogre::LoggingLevel converted;

    switch(lvl)
    {
        case LL_LOW:
            converted = Ogre::LL_LOW;
            break;

        case LL_NORMAL:
            converted = Ogre::LL_NORMAL;
            break;

        case LL_BOREME:
            converted = Ogre::LL_BOREME;
            break;
    }
    Ogre::LogManager::getSingletonPtr()->setLogDetail(converted);
}
