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

hardware_buffer_usage ogre_hbu_to_llcoi_hbu(Ogre::HardwareBuffer::Usage ogre_hbu)
{
    hardware_buffer_usage converted;

    switch(ogre_hbu)
    {
        case Ogre::HardwareBuffer::HBU_STATIC:
            converted = HBU_STATIC;
            break;

        case Ogre::HardwareBuffer::HBU_DYNAMIC:
            converted = HBU_DYNAMIC;
            break;

        case Ogre::HardwareBuffer::HBU_WRITE_ONLY:
            converted = HBU_WRITE_ONLY;
            break;

        case Ogre::HardwareBuffer::HBU_DISCARDABLE:
            converted = HBU_DISCARDABLE;
            break;

        case Ogre::HardwareBuffer::HBU_STATIC_WRITE_ONLY:
            converted = HBU_STATIC_WRITE_ONLY;
            break;

        case Ogre::HardwareBuffer::HBU_DYNAMIC_WRITE_ONLY:
            converted = HBU_DYNAMIC_WRITE_ONLY;
            break;

        case Ogre::HardwareBuffer::HBU_DYNAMIC_WRITE_ONLY_DISCARDABLE:
            converted = HBU_DYNAMIC_WRITE_ONLY_DISCARDABLE;
            break;
    }
    return converted;
}

Ogre::HardwareBuffer::Usage llcoi_hbu_to_ogre_hbu(hardware_buffer_usage llcoi_hbu)
{
    Ogre::HardwareBuffer::Usage converted;

    switch(llcoi_hbu)
    {
        case HBU_STATIC:
            converted = Ogre::HardwareBuffer::HBU_STATIC;
            break;

        case HBU_DYNAMIC:
            converted = Ogre::HardwareBuffer::HBU_DYNAMIC;
            break;

        case HBU_WRITE_ONLY:
            converted = Ogre::HardwareBuffer::HBU_WRITE_ONLY;
            break;

        case HBU_DISCARDABLE:
            converted = Ogre::HardwareBuffer::HBU_DISCARDABLE;
            break;

        case HBU_STATIC_WRITE_ONLY:
            converted = Ogre::HardwareBuffer::HBU_STATIC_WRITE_ONLY;
            break;

        case HBU_DYNAMIC_WRITE_ONLY:
            converted = Ogre::HardwareBuffer::HBU_DYNAMIC_WRITE_ONLY;
            break;

        case HBU_DYNAMIC_WRITE_ONLY_DISCARDABLE:
            converted = Ogre::HardwareBuffer::HBU_DYNAMIC_WRITE_ONLY_DISCARDABLE;
            break;
    }
    return converted;
}



logging_level ogre_ll_to_llcoi_ll(Ogre::LoggingLevel ll);
Ogre::LoggingLevel llcoi_ll_to_ogre_ll(logging_level ll);
