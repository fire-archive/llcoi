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

#include <OgreLog.h>            // LogMessageLevel and LoggingLevel
#include <OgreSceneQuery.h>     // SceneQuery::WorldFragmentType
#include <OgreHardwareBuffer.h> // HardwareBuffer::Usage
#include <OgreFrustum.h>        // Ogre::OrientationMode
#include <OgreOverlayElement.h> // GuiVerticalAlignment, GuiMetricsMode, GuiHorizontalAlignment
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

/* OGRE data <-> LLCOI data converters */
void ogre_matrix3_to_llcoi_matrix3(const Ogre::Matrix3& o, coiMatrix3& l);
void llcoi_matrix3_to_ogre_matrix3(const coiMatrix3& l, Ogre::Matrix3& o);

void ogre_matrix4_to_llcoi_matrix4(const Ogre::Matrix4& o, coiMatrix4& l);
void llcoi_matrix4_to_ogre_matrix4(const coiMatrix4& l, Ogre::Matrix4& o);


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

Ogre::SceneQuery::WorldFragmentType llcoi_wft_to_ogre_wft(world_fragment_type wft);
world_fragment_type ogre_wft_to_llcoi_wft(Ogre::SceneQuery::WorldFragmentType wft);

// from OgreFrustum.h
Ogre::OrientationMode llcoi_orientation_mode_to_ogre_orientation_mode(orientation_mode mode);
orientation_mode ogre_orientation_mode_to_llcoi_orientation_mode(Ogre::OrientationMode mode);

Ogre::ProjectionType llcoi_projection_type_to_ogre_projection_type(projection_type type);
projection_type ogre_projection_type_to_llcoi_projection_type(Ogre::ProjectionType type);

Ogre::FrustumPlane llcoi_frustum_plane_to_ogre_frustum_plane(frustum_plane plane);
frustum_plane ogre_frustum_plane_to_llcoi_frustum_plane(Ogre::FrustumPlane plane);

// from OgreOverlayElement.h
Ogre::GuiMetricsMode llcoi_gui_metrics_mode_to_ogre_gui_metrics_mode(gui_metrics_mode gmm);
gui_metrics_mode ogre_gui_metrics_mode_to_llcoi_metrics_mode(Ogre::GuiMetricsMode gmm);

Ogre::GuiHorizontalAlignment llcoi_gui_horizontal_alignment_to_ogre_gui_horizontal_alignment(gui_horizontal_alignment gha);
gui_horizontal_alignment ogre_gui_horizontal_alignment_to_ogre_gui_horizontal_alignment(Ogre::GuiHorizontalAlignment gha);

Ogre::GuiVerticalAlignment llcoi_gui_vertical_alignment_to_ogre_gui_vertical_alignment(gui_vertical_alignment gva);
gui_vertical_alignment ogre_gui_vertical_alignment_to_llcoi_gui_vertical_alignment(Ogre::GuiVerticalAlignment gva);

/* OIS MouseState <-> LLCOI MouseState converters */

void ois_mouse_event_to_llcoi_mouse_event(const OIS::MouseEvent* ois_mouse_event, MouseEvent* llcoi_mouse_event);
MouseButtonID ois_mbid_to_llcoi_mbid(OIS::MouseButtonID id);

#endif
