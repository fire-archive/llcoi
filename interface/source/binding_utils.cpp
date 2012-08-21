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
#include <OISMouse.h>
#include <OgreMatrix3.h>
#include <OgreMatrix4.h>

void ogre_matrix3_to_llcoi_matrix3(const Ogre::Matrix3& o, coiMatrix3& l)
{
    l.m[0][0] = o[0][0];
    l.m[0][1] = o[0][1];
    l.m[0][2] = o[0][2];

    l.m[1][0] = o[1][0];
    l.m[1][1] = o[1][1];
    l.m[1][2] = o[1][2];

    l.m[2][0] = o[2][0];
    l.m[2][1] = o[2][1];
    l.m[2][2] = o[2][2];
}


void llcoi_matrix3_to_ogre_matrix3(const coiMatrix3& l, Ogre::Matrix3& o)
{
    o[0][0] = l.m[0][0];
    o[0][1] = l.m[0][1];
    o[0][2] = l.m[0][2];

    o[1][0] = l.m[1][0];
    o[1][1] = l.m[1][1];
    o[1][2] = l.m[1][2];

    o[2][0] = l.m[2][0];
    o[2][1] = l.m[2][1];
    o[2][2] = l.m[2][2];
}


void llcoi_matrix4_to_ogre_matrix4(const coiMatrix4& l, Ogre::Matrix4& o)
{
    o[0][0] = l.m[0][0];
    o[0][1] = l.m[0][1];
    o[0][2] = l.m[0][2];
    o[0][3] = l.m[0][3];

    o[1][0] = l.m[1][0];
    o[1][1] = l.m[1][1];
    o[1][2] = l.m[1][2];
    o[1][3] = l.m[1][3];

    o[2][0] = l.m[2][0];
    o[2][1] = l.m[2][1];
    o[2][2] = l.m[2][2];
    o[2][3] = l.m[2][3];

    o[3][0] = l.m[3][0];
    o[3][1] = l.m[3][1];
    o[3][2] = l.m[3][2];
    o[3][3] = l.m[3][3];
}

void ogre_matrix4_to_llcoi_matrix4(const Ogre::Matrix4&o, coiMatrix4& l)
{
    l.m[0][0] = o[0][0];
    l.m[0][1] = o[0][1];
    l.m[0][2] = o[0][2];
    l.m[0][3] = o[0][3];

    l.m[1][0] = o[1][0];
    l.m[1][1] = o[1][1];
    l.m[1][2] = o[1][2];
    l.m[1][3] = o[1][3];

    l.m[2][0] = o[2][0];
    l.m[2][1] = o[2][1];
    l.m[2][2] = o[2][2];
    l.m[2][3] = o[2][3];

    l.m[3][0] = o[3][0];
    l.m[3][1] = o[3][1];
    l.m[3][2] = o[3][2];
    l.m[3][3] = o[3][3];
}


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



light_types ogre_light_type_to_llcoi_light_type(Ogre::Light::LightTypes type)
{
    light_types converted;
    switch(type)
    {
        case Ogre::Light::LT_POINT:
            converted = LT_POINT;
            break;
        case Ogre::Light::LT_DIRECTIONAL:
            converted = LT_DIRECTIONAL;
            break;
        case Ogre::Light::LT_SPOTLIGHT:
            converted = LT_SPOTLIGHT;
            break;
    }
    return converted;
}

Ogre::Light::LightTypes llcoi_light_types_to_ogre_light_types(light_types type)
{
    Ogre::Light::LightTypes converted;
    switch(type)
    {
        case LT_POINT:
            converted = Ogre::Light::LT_POINT;
            break;
        case LT_DIRECTIONAL:
            converted = Ogre::Light::LT_DIRECTIONAL;
            break;
        case LT_SPOTLIGHT:
            converted = Ogre::Light::LT_SPOTLIGHT;
            break;
    }
    return converted;
}


transform_space ogre_ts_to_llcoi_ts(Ogre::Node::TransformSpace ogre_ts)
{
    transform_space converted;
    switch(ogre_ts)
    {
        case Ogre::Node::TS_LOCAL:
            converted = TS_LOCAL;
            break;
        case Ogre::Node::TS_PARENT:
            converted = TS_PARENT;
            break;
        case Ogre::Node::TS_WORLD:
            converted = TS_WORLD;
            break;
    }
    return converted;
}

Ogre::Node::TransformSpace llcoi_ts_to_ogre_ts(transform_space llcoi_ts)
{
    Ogre::Node::TransformSpace converted;
    switch(llcoi_ts)
    {
        case TS_LOCAL:
            converted = Ogre::Node::TS_LOCAL;
            break;
        case TS_PARENT:
            converted = Ogre::Node::TS_PARENT;
            break;
        case TS_WORLD:
            converted = Ogre::Node::TS_WORLD;
            break;
    }
    return converted;
}


Ogre::Plane::Side llcoi_plane_side_to_ogre_plane_side(plane_side side)
{
    Ogre::Plane::Side converted;

    switch(side)
    {
        case NO_SIDE:
            converted = Ogre::Plane::NO_SIDE;
            break;
        case POSITIVE_SIDE:
            converted = Ogre::Plane::POSITIVE_SIDE;
            break;
        case NEGATIVE_SIDE:
            converted = Ogre::Plane::NEGATIVE_SIDE;
            break;
        case BOTH_SIDE:
            converted = Ogre::Plane::BOTH_SIDE;
            break;
    }

    return converted;
}

plane_side ogre_plane_side_to_llcoi_plane_side(Ogre::Plane::Side side)
{
    plane_side converted;

    switch(side)
    {
        case Ogre::Plane::NO_SIDE:
            converted = NO_SIDE;
            break;
        case Ogre::Plane::POSITIVE_SIDE:
            converted = POSITIVE_SIDE;
            break;
        case Ogre::Plane::NEGATIVE_SIDE:
            converted = NEGATIVE_SIDE;
            break;
        case Ogre::Plane::BOTH_SIDE:
            converted = BOTH_SIDE;
            break;
    }


    return converted;
}

MouseButtonID ois_mbid_to_llcoi_mbid(OIS::MouseButtonID id)
{
    MouseButtonID converted;
    switch(id)
    {
        case OIS::MB_Left:
            converted = MB_Left;
            break;
        case OIS::MB_Right:
            converted = MB_Right;
            break;
        case OIS::MB_Middle:
            converted = MB_Middle;
            break;
        case OIS::MB_Button3:
            converted = MB_Button3;
            break;
        case OIS::MB_Button4:
            converted = MB_Button4;
            break;
        case OIS::MB_Button5:
            converted = MB_Button5;
            break;
        case OIS::MB_Button6:
            converted = MB_Button6;
            break;
        case OIS::MB_Button7:
            converted = MB_Button7;
            break;
    }
    return converted;
}

void ois_mouse_event_to_llcoi_mouse_event(const OIS::MouseEvent* oevent, MouseEvent* levent)
{
    /* copy dimensions */
    levent->state.width   = oevent->state.width;
    levent->state.height  = oevent->state.height;

    /* copy Axis */

    levent->state.X.abs  = oevent->state.X.abs;
    levent->state.Y.abs  = oevent->state.Y.abs;
    levent->state.Z.abs  = oevent->state.Z.abs;

    levent->state.X.rel  = oevent->state.X.rel;
    levent->state.Y.rel  = oevent->state.Y.rel;
    levent->state.Z.rel  = oevent->state.Z.rel;

    levent->state.X.absOnly  = oevent->state.X.absOnly;
    levent->state.Y.absOnly  = oevent->state.Y.absOnly;
    levent->state.Z.absOnly  = oevent->state.Z.absOnly;

    /* copy buttons. */
    levent->state.buttons = oevent->state.buttons;
}


logging_level ogre_ll_to_llcoi_ll(Ogre::LoggingLevel ll)
{
    // FIXME: Finish this.
    logging_level converted;
    return converted;
}

Ogre::LoggingLevel llcoi_ll_to_ogre_ll(logging_level ll)
{
    // FIXME: Finish this.
    Ogre::LoggingLevel converted;
    return converted;
}

Ogre::AxisAlignedBox::Extent llcoi_extent_to_ogre_extent(Extent e)
{
    Ogre::AxisAlignedBox::Extent converted;

    switch(e)
    {
        case EXTENT_NULL:
            converted = Ogre::AxisAlignedBox::EXTENT_NULL;
            break;

        case EXTENT_FINITE:
            converted = Ogre::AxisAlignedBox::EXTENT_FINITE;
            break;

        case EXTENT_INFINITE:
            converted = Ogre::AxisAlignedBox::EXTENT_INFINITE;
            break;

    }
    return converted;
}

Extent ogre_extent_to_llcoi_extent(Ogre::AxisAlignedBox::Extent e)
{
    Extent converted;
    switch(e)
    {
        case Ogre::AxisAlignedBox::EXTENT_NULL:
            converted = EXTENT_NULL;
            break;

        case Ogre::AxisAlignedBox::EXTENT_FINITE:
            converted = EXTENT_FINITE;
            break;

        case Ogre::AxisAlignedBox::EXTENT_INFINITE:
            converted = EXTENT_INFINITE;
            break;
    }

    return converted;
}


Ogre::AxisAlignedBox::CornerEnum llcoi_cornerenum_to_ogre_cornerenum(CornerEnum e)
{
    Ogre::AxisAlignedBox::CornerEnum converted;
    switch(e)
    {
        case FAR_LEFT_BOTTOM:
            converted = Ogre::AxisAlignedBox::FAR_LEFT_BOTTOM;
            break;

        case FAR_LEFT_TOP:
            converted = Ogre::AxisAlignedBox::FAR_LEFT_TOP;
            break;

        case FAR_RIGHT_TOP:
            converted = Ogre::AxisAlignedBox::FAR_RIGHT_TOP;
            break;

        case FAR_RIGHT_BOTTOM:
            converted = Ogre::AxisAlignedBox::FAR_RIGHT_BOTTOM;
            break;

        case NEAR_RIGHT_BOTTOM:
            converted = Ogre::AxisAlignedBox::NEAR_RIGHT_BOTTOM;
            break;

        case NEAR_LEFT_BOTTOM:
            converted = Ogre::AxisAlignedBox::NEAR_LEFT_BOTTOM;
            break;

        case NEAR_LEFT_TOP:
            converted = Ogre::AxisAlignedBox::NEAR_LEFT_TOP;
            break;

        case NEAR_RIGHT_TOP:
            converted = Ogre::AxisAlignedBox::NEAR_RIGHT_TOP;
            break;

    }
    return converted;
}

CornerEnum ogre_cornerenum_to_llcoi_cornerenum(Ogre::AxisAlignedBox::CornerEnum e)
{
    CornerEnum converted;
    switch(e)
    {
        case Ogre::AxisAlignedBox::FAR_LEFT_BOTTOM:
            converted = FAR_LEFT_BOTTOM;
            break;

        case Ogre::AxisAlignedBox::FAR_LEFT_TOP:
            converted = FAR_LEFT_TOP;
            break;

        case Ogre::AxisAlignedBox::FAR_RIGHT_TOP:
            converted = FAR_RIGHT_TOP;
            break;

        case Ogre::AxisAlignedBox::FAR_RIGHT_BOTTOM:
            converted = FAR_RIGHT_BOTTOM;
            break;

        case Ogre::AxisAlignedBox::NEAR_RIGHT_BOTTOM:
            converted = NEAR_RIGHT_BOTTOM;
            break;

        case Ogre::AxisAlignedBox::NEAR_LEFT_BOTTOM:
            converted = NEAR_LEFT_BOTTOM;
            break;

        case Ogre::AxisAlignedBox::NEAR_LEFT_TOP:
            converted = NEAR_LEFT_TOP;
            break;

        case Ogre::AxisAlignedBox::NEAR_RIGHT_TOP:
            converted = NEAR_RIGHT_TOP;
            break;

    }
    return converted;
}

Ogre::SceneQuery::WorldFragmentType llcoi_wft_to_ogre_wft(world_fragment_type wft)
{
    Ogre::SceneQuery::WorldFragmentType converted;
    switch(wft)
    {
        case WFT_NONE:
            converted = Ogre::SceneQuery::WFT_NONE;
            break;
        case WFT_PLANE_BOUNDED_REGION:
            converted = Ogre::SceneQuery::WFT_PLANE_BOUNDED_REGION;
            break;
        case WFT_SINGLE_INTERSECTION:
            converted = Ogre::SceneQuery::WFT_SINGLE_INTERSECTION;
            break;
        case WFT_CUSTOM_GEOMETRY:
            converted = Ogre::SceneQuery::WFT_CUSTOM_GEOMETRY;
            break;
        case WFT_RENDER_OPERATION:
            converted = Ogre::SceneQuery::WFT_RENDER_OPERATION;
            break;
    }

    return converted;
}

world_fragment_type ogre_wft_to_llcoi_wft(Ogre::SceneQuery::WorldFragmentType wft)
{
    world_fragment_type converted;
    switch(wft)
    {
        case Ogre::SceneQuery::WFT_NONE:
            converted = WFT_NONE;
            break;

        case Ogre::SceneQuery::WFT_PLANE_BOUNDED_REGION:
            converted = WFT_PLANE_BOUNDED_REGION;
            break;

        case Ogre::SceneQuery::WFT_SINGLE_INTERSECTION:
            converted = WFT_SINGLE_INTERSECTION;
            break;

        case Ogre::SceneQuery::WFT_CUSTOM_GEOMETRY:
            converted = WFT_CUSTOM_GEOMETRY;
            break;

        case Ogre::SceneQuery::WFT_RENDER_OPERATION:
            converted = WFT_RENDER_OPERATION;
            break;

    }
    return converted;
}

Ogre::OrientationMode llcoi_orientation_mode_to_ogre_orientation_mode(orientation_mode mode)
{
    Ogre::OrientationMode converted;
    switch(mode)
    {
        case OR_DEGREE_0:
            converted = Ogre::OR_DEGREE_0;
            break;

        case OR_DEGREE_90:
            converted = Ogre::OR_DEGREE_90;
            break;


        case OR_DEGREE_180:
            converted = Ogre::OR_DEGREE_180;
            break;


        case OR_DEGREE_270:
            converted = Ogre::OR_DEGREE_270;
            break;

    }

    return converted; 
}

orientation_mode ogre_orientation_mode_to_llcoi_orientation_mode(Ogre::OrientationMode mode)
{
    orientation_mode converted;
    switch(mode)
    {
        case Ogre::OR_DEGREE_0:
            converted = OR_DEGREE_0;
            break;


        case Ogre::OR_DEGREE_90:
            converted = OR_DEGREE_90;
            break;


        case Ogre::OR_DEGREE_180:
            converted = OR_DEGREE_180;
            break;


        case Ogre::OR_DEGREE_270:
            converted = OR_DEGREE_270;
            break;

    }
    return converted;
}

Ogre::ProjectionType llcoi_projection_type_to_ogre_projection_type(projection_type type)
{
    Ogre::ProjectionType converted;

    switch(type)
    {
        case PT_ORTHOGRAPHIC:
            converted = Ogre::PT_ORTHOGRAPHIC;
            break;


        case PT_PERSPECTIVE:
            converted = Ogre::PT_PERSPECTIVE;
            break;

    }

    return converted;
}

projection_type ogre_projection_type_to_llcoi_projection_type(Ogre::ProjectionType type)
{
    projection_type converted;

    switch(type)
    {
        case Ogre::PT_ORTHOGRAPHIC:
            converted = PT_ORTHOGRAPHIC;
            break;

        case Ogre::PT_PERSPECTIVE:
            converted = PT_PERSPECTIVE;
            break;
    }

    return converted;
}

Ogre::FrustumPlane llcoi_frustum_plane_to_ogre_frustum_plane(frustum_plane plane)
{
    Ogre::FrustumPlane converted;

    switch(plane)
    {
        case FRUSTUM_PLANE_NEAR:
            converted = Ogre::FRUSTUM_PLANE_NEAR;
            break;

        case FRUSTUM_PLANE_FAR:
            converted = Ogre::FRUSTUM_PLANE_FAR;
            break;

        case FRUSTUM_PLANE_LEFT:
            converted = Ogre::FRUSTUM_PLANE_LEFT;
            break;

        case FRUSTUM_PLANE_RIGHT:
            converted = Ogre::FRUSTUM_PLANE_RIGHT;
            break;

        case FRUSTUM_PLANE_TOP:
            converted = Ogre::FRUSTUM_PLANE_TOP;
            break;

        case FRUSTUM_PLANE_BOTTOM:
            converted = Ogre::FRUSTUM_PLANE_BOTTOM;
            break;
    }

    return converted;
}

frustum_plane ogre_frustum_plane_to_llcoi_frustum_plane(Ogre::FrustumPlane plane)
{
    frustum_plane converted;

    switch(plane)
    {
        case Ogre::FRUSTUM_PLANE_NEAR:
            converted = FRUSTUM_PLANE_NEAR;
            break;

        case Ogre::FRUSTUM_PLANE_FAR:
            converted = FRUSTUM_PLANE_FAR;
            break;

        case Ogre::FRUSTUM_PLANE_LEFT:
            converted = FRUSTUM_PLANE_LEFT;
            break;

        case Ogre::FRUSTUM_PLANE_RIGHT:
            converted = FRUSTUM_PLANE_RIGHT;
            break;

        case Ogre::FRUSTUM_PLANE_TOP:
            converted = FRUSTUM_PLANE_TOP;
            break;

        case Ogre::FRUSTUM_PLANE_BOTTOM:
            converted = FRUSTUM_PLANE_BOTTOM;
            break;
    }

    return converted;
}


Ogre::GuiMetricsMode llcoi_gui_metrics_mode_to_ogre_gui_metrics_mode(gui_metrics_mode gmm)
{

    Ogre::GuiMetricsMode converted;
    switch(gmm)
    {
        case GMM_RELATIVE:
            converted = Ogre::GMM_RELATIVE;
            break;

        case GMM_PIXELS:
            converted = Ogre::GMM_PIXELS;
            break;

        case GMM_RELATIVE_ASPECT_ADJUSTED:
            converted = Ogre::GMM_RELATIVE_ASPECT_ADJUSTED;
            break;
    }
    return converted;
}

gui_metrics_mode ogre_gui_metrics_mode_to_llcoi_metrics_mode(Ogre::GuiMetricsMode gmm)
{
    gui_metrics_mode converted;
    switch(gmm)
    {
        case Ogre::GMM_RELATIVE:
            converted = GMM_RELATIVE;
            break;

        case Ogre::GMM_PIXELS:
            converted = GMM_PIXELS;
            break;

        case Ogre::GMM_RELATIVE_ASPECT_ADJUSTED:
            converted = GMM_RELATIVE_ASPECT_ADJUSTED;
            break;
    }
    return converted;
}

Ogre::GuiHorizontalAlignment llcoi_gui_horizontal_alignment_to_ogre_gui_horizontal_alignment(gui_horizontal_alignment gha)
{
    Ogre::GuiHorizontalAlignment converted;
    switch(gha)
    {
        case GHA_LEFT:
            converted = Ogre::GHA_LEFT;
            break;

        case GHA_CENTER:
            converted = Ogre::GHA_CENTER;
            break;

        case GHA_RIGHT:
            converted = Ogre::GHA_RIGHT;
            break;

    }
    return converted;
}

gui_horizontal_alignment ogre_gui_horizontal_alignment_to_ogre_gui_horizontal_alignment(Ogre::GuiHorizontalAlignment gha)
{
    gui_horizontal_alignment converted;
    switch(gha)
    {
        case Ogre::GHA_LEFT:
            converted = GHA_LEFT;
            break;

        case Ogre::GHA_CENTER:
            converted = GHA_CENTER;
            break;

        case Ogre::GHA_RIGHT:
            converted = GHA_RIGHT;
            break;

    }
    return converted;
}

Ogre::GuiVerticalAlignment llcoi_gui_vertical_alignment_to_ogre_gui_vertical_alignment(gui_vertical_alignment gva)
{
    Ogre::GuiVerticalAlignment converted;

    switch(gva)
    {
        case GVA_TOP:
            converted = Ogre::GVA_TOP;
            break;

        case GVA_CENTER:
            converted = Ogre::GVA_CENTER;
            break;

        case GVA_BOTTOM:
            converted = Ogre::GVA_BOTTOM;
            break;

    }
    return converted;
}

gui_vertical_alignment ogre_gui_vertical_alignment_to_llcoi_gui_vertical_alignment(Ogre::GuiVerticalAlignment gva)
{
    gui_vertical_alignment converted;
    switch(gva)
    {
        case Ogre::GVA_TOP:
            converted = GVA_TOP;
            break;

        case Ogre::GVA_CENTER:
            converted = GVA_CENTER;
            break;

        case Ogre::GVA_BOTTOM:
            converted = GVA_BOTTOM;
            break;

    }
    return converted;
}
