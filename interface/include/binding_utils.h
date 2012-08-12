/******************************************************************************
 * binding_utils.h - utility functions for LLCOI
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
#pragma once
#ifndef LLCOI_BINDING_UTILS
#define LLCOI_BINDING_UTILS
#include "ogre_interface.h"      // as we can't forward declare enums. ):
#include "ois_interface.h"       // ditto
#include "log_bind.h"            // log_message_level, logging_level
#include "plane_bind.h"          // plane_side
#include "axisalignedbox_bind.h" // Extent, CornerEnum

#include <OgreLog.h>            // LogMessageLevel and LoggingLevel
#include <OgreHardwareBuffer.h> // HardwareBuffer::Usage
#include <OgreLight.h>          // Light::LightTypes
#include <OgreNode.h>           // Node::TransformSpace
#include <OISMouse.h>           // OIS::MouseButtonID

// tempting...
//#include <boost/unordered_map.hpp>
//#include <boost/assign/list_of.hpp>


// LLCOI forward declarations
typedef struct _MouseEvent MouseEvent;

// OIS forward declarations
namespace OIS
{
    class MouseEvent;
};


/* OGRE enum <-> LLCOI enum converters */
log_message_level ogre_lml_to_llcoi_lml(Ogre::LogMessageLevel lml);
Ogre::LogMessageLevel llcoi_lml_to_ogre_lml(log_message_level lml);

hardware_buffer_usage ogre_hbu_to_llcoi_hbu(Ogre::HardwareBuffer::Usage ogre_hbu);
Ogre::HardwareBuffer::Usage llcoi_hbu_to_ogre_hbu(hardware_buffer_usage llcoi_hbu);

light_types ogre_light_type_to_llcoi_light_type(Ogre::Light::LightTypes type);
Ogre::Light::LightTypes llcoi_light_types_to_ogre_light_types(light_types type);

Ogre::Node::TransformSpace llcoi_ts_to_ogre_ts(transform_space llcoi_ts);
transform_space ogre_ts_to_llcoi_ts(Ogre::Node::TransformSpace ogre_ts);

Ogre::Plane::Side llcoi_plane_side_to_ogre_plane_side(plane_side side);
plane_side ogre_plane_side_to_llcoi_plane_side(Ogre::Plane::Side side);

logging_level ogre_ll_to_llcoi_ll(Ogre::LoggingLevel ll);
Ogre::LoggingLevel llcoi_ll_to_ogre_ll(logging_level ll);

Ogre::AxisAlignedBox::Extent llcoi_extent_to_ogre_extent(Extent e);
Extent ogre_extent_to_llcoi_extent(Ogre::AxisAlignedBox::Extent e);

Ogre::AxisAlignedBox::CornerEnum llcoi_cornerenum_to_ogre_cornerenum(CornerEnum e);
CornerEnum ogre_cornerenum_to_llcoi_cornerenum(Ogre::AxisAlignedBox::CornerEnum e);



/* OIS MouseState <-> LLCOI MouseState converters */

void ois_mouse_event_to_llcoi_mouse_event(const OIS::MouseEvent* ois_mouse_event, MouseEvent* llcoi_mouse_event);
MouseButtonID ois_mbid_to_llcoi_mbid(OIS::MouseButtonID id);

#endif
