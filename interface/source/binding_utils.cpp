/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
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


// TODO: Since we're not exposing the utils to C, refactor all of these enum converters to be overloads of 'enum_converter'
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

size_t ogre_light_list_to_llcoi(Ogre::LightList& l, LightHandle c_vector)
{
    c_vector = static_cast<LightHandle>(&l[0]);
    return l.size();
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

void ois_mouse_event_to_llcoi_mouse_state(const OIS::MouseEvent* oevent, MouseState* lstate)
{
    lstate->X.abs  = oevent->state.X.abs;
    lstate->Y.abs  = oevent->state.Y.abs;
    lstate->Z.abs  = oevent->state.Z.abs;

    lstate->X.rel  = oevent->state.X.rel;
    lstate->Y.rel  = oevent->state.Y.rel;
    lstate->Z.rel  = oevent->state.Z.rel;

    lstate->X.absOnly  = oevent->state.X.absOnly;
    lstate->Y.absOnly  = oevent->state.Y.absOnly;
    lstate->Z.absOnly  = oevent->state.Z.absOnly;

    /* copy buttons. */
    lstate->buttons = oevent->state.buttons;
}


logging_level ogre_ll_to_llcoi_ll(Ogre::LoggingLevel ll)
{
    logging_level converted;

    switch(ll)
    {
        case Ogre::LL_LOW:
            converted = LL_LOW;
            break;

        case Ogre::LL_NORMAL:
            converted = LL_NORMAL;
            break;

        case Ogre::LL_BOREME:
            converted = LL_BOREME;
            break;
    }

    return converted;
}

Ogre::LoggingLevel llcoi_ll_to_ogre_ll(logging_level ll)
{
    Ogre::LoggingLevel converted;

    switch(ll)
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

gui_horizontal_alignment ogre_gui_horizontal_alignment_to_llcoi_gui_horizontal_alignment(Ogre::GuiHorizontalAlignment gha)
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


Ogre::TextAreaOverlayElement::Alignment llcoi_textarea_alignment_to_ogre_textarea_alignment(textarea_overlayelement_alignment align)
{
    Ogre::TextAreaOverlayElement::Alignment converted;

    switch(align)
    {
        case Left:
            converted = Ogre::TextAreaOverlayElement::Left;
            break;

        case Right:
            converted = Ogre::TextAreaOverlayElement::Right;
            break;

        case Center:
            converted = Ogre::TextAreaOverlayElement::Center;
            break;
    }

    return converted;
}

textarea_overlayelement_alignment ogre_textarea_alignment_to_llcoi_textarea_alignment(Ogre::TextAreaOverlayElement::Alignment align)
{
    textarea_overlayelement_alignment converted;

    switch(align)
    {
        case Ogre::TextAreaOverlayElement::Left:
            converted = Left;
            break;

        case Ogre::TextAreaOverlayElement::Right:
            converted = Right;
            break;

        case Ogre::TextAreaOverlayElement::Center:
            converted = Center;
            break;
    }

    return converted;
}

// from OgreSkeleton.h
Ogre::SkeletonAnimationBlendMode llcoi_skeleton_blend_mode_to_ogre(skeleton_animation_blend_mode mode)
{
    Ogre::SkeletonAnimationBlendMode converted;

    switch(mode)
    {
        case ANIMBLEND_AVERAGE:
            converted = Ogre::ANIMBLEND_AVERAGE;
            break;

        case ANIMBLEND_CUMULATIVE:
            converted = Ogre::ANIMBLEND_CUMULATIVE;
            break;
    }

    return converted;
}

skeleton_animation_blend_mode ogre_skeleton_blend_mode_to_llcoi(Ogre::SkeletonAnimationBlendMode mode)
{
    skeleton_animation_blend_mode converted;

    switch(mode)
    {
        case Ogre::ANIMBLEND_AVERAGE:
            converted = ANIMBLEND_AVERAGE;
            break;

        case Ogre::ANIMBLEND_CUMULATIVE:
            converted = ANIMBLEND_CUMULATIVE;
            break;
    }

    return converted;
}

// OgreRenderOperation.h
Ogre::RenderOperation::OperationType llcoi_operation_type_to_ogre(operation_type op_type)
{
    Ogre::RenderOperation::OperationType converted;
    switch(op_type)
    {
        case OT_POINT_LIST:
            converted = Ogre::RenderOperation::OT_POINT_LIST;
            break;

        case OT_LINE_LIST:
            converted = Ogre::RenderOperation::OT_LINE_LIST;
            break;

        case OT_LINE_STRIP:
            converted = Ogre::RenderOperation::OT_LINE_STRIP;
            break;

        case OT_TRIANGLE_LIST:
            converted = Ogre::RenderOperation::OT_TRIANGLE_LIST;
            break;

        case OT_TRIANGLE_STRIP:
            converted = Ogre::RenderOperation::OT_TRIANGLE_STRIP;
            break;

        case OT_TRIANGLE_FAN:
            converted = Ogre::RenderOperation::OT_TRIANGLE_FAN;
            break;
    }
    return converted;
}

operation_type ogre_operation_type_to_llcoi(Ogre::RenderOperation::OperationType op_type)
{

    operation_type converted;
    switch(op_type)
    {
        case Ogre::RenderOperation::OT_POINT_LIST:
            converted = OT_POINT_LIST;
            break;

        case Ogre::RenderOperation::OT_LINE_LIST:
            converted = OT_LINE_LIST;
            break;

        case Ogre::RenderOperation::OT_LINE_STRIP:
            converted = OT_LINE_STRIP;
            break;

        case Ogre::RenderOperation::OT_TRIANGLE_LIST:
            converted = OT_TRIANGLE_LIST;
            break;

        case Ogre::RenderOperation::OT_TRIANGLE_STRIP:
            converted = OT_TRIANGLE_STRIP;
            break;

        case Ogre::RenderOperation::OT_TRIANGLE_FAN:
            converted = OT_TRIANGLE_FAN;
            break;
    }
    return converted;
}

//OgreResource.h
Ogre::Resource::LoadingState llcoi_loading_state_to_ogre(loading_state state)
{
    Ogre::Resource::LoadingState converted;
    switch(state)
    {

        case LOADSTATE_UNLOADED:
            converted = Ogre::Resource::LOADSTATE_UNLOADED;
            break;

        case LOADSTATE_LOADING:
            converted = Ogre::Resource::LOADSTATE_LOADING;
            break;

        case LOADSTATE_LOADED:
            converted = Ogre::Resource::LOADSTATE_LOADED;
            break;

        case LOADSTATE_UNLOADING:
            converted = Ogre::Resource::LOADSTATE_UNLOADING;
            break;

        case LOADSTATE_PREPARED:
            converted = Ogre::Resource::LOADSTATE_PREPARED;
            break;

        case LOADSTATE_PREPARING:
            converted = Ogre::Resource::LOADSTATE_PREPARING;
            break;

    }
    return converted;
}

loading_state ogre_loading_state_to_llcoi(Ogre::Resource::LoadingState state)
{
    loading_state converted;
    switch(state)
    {

        case Ogre::Resource::LOADSTATE_UNLOADED:
            converted = LOADSTATE_UNLOADED;
            break;

        case Ogre::Resource::LOADSTATE_LOADING:
            converted = LOADSTATE_LOADING;
            break;

        case Ogre::Resource::LOADSTATE_LOADED:
            converted = LOADSTATE_LOADED;
            break;

        case Ogre::Resource::LOADSTATE_UNLOADING:
            converted = LOADSTATE_UNLOADING;
            break;

        case Ogre::Resource::LOADSTATE_PREPARED:
            converted = LOADSTATE_PREPARED;
            break;

        case Ogre::Resource::LOADSTATE_PREPARING:
            converted = LOADSTATE_PREPARING;
            break;

    }
    return converted;
}

// OgreBlendMode.h
Ogre::SceneBlendType enum_converter(scene_blend_type arg)
{
    Ogre::SceneBlendType converted;
    switch(arg)
    {
        case SBT_TRANSPARENT_ALPHA:
            converted = Ogre::SBT_TRANSPARENT_ALPHA;
            break;

        case SBT_TRANSPARENT_COLOUR:
            converted = Ogre::SBT_TRANSPARENT_COLOUR;
            break;

        case SBT_ADD:
            converted = Ogre::SBT_ADD;
            break;

        case SBT_MODULATE:
            converted = Ogre::SBT_MODULATE;
            break;

        case SBT_REPLACE:
            converted = Ogre::SBT_REPLACE;
            break;

    }
    return converted;
}

scene_blend_type enum_converter(Ogre::SceneBlendType arg)
{
    scene_blend_type converted;
    switch(arg)
    {
        case Ogre::SBT_TRANSPARENT_ALPHA:
            converted = SBT_TRANSPARENT_ALPHA;
            break;

        case Ogre::SBT_TRANSPARENT_COLOUR:
            converted = SBT_TRANSPARENT_COLOUR;
            break;

        case Ogre::SBT_ADD:
            converted = SBT_ADD;
            break;

        case Ogre::SBT_MODULATE:
            converted = SBT_MODULATE;
            break;

        case Ogre::SBT_REPLACE:
            converted = SBT_REPLACE;
            break;

    }
    return converted;
}


Ogre::LayerBlendType enum_converter(layer_blend_type arg)
{
    Ogre::LayerBlendType converted;
    switch(arg)
    {
        case LBT_COLOUR:
            converted = Ogre::LBT_COLOUR;
            break;

        case LBT_ALPHA:
            converted = Ogre::LBT_ALPHA;
            break;

    }
    return converted;
}

layer_blend_type enum_converter(Ogre::LayerBlendType arg)
{
    layer_blend_type converted;
    switch(arg)
    {
        case Ogre::LBT_COLOUR:
            converted = LBT_COLOUR;
            break;

        case Ogre::LBT_ALPHA:
            converted = LBT_ALPHA;
            break;

    }
    return converted;
}


Ogre::SceneBlendFactor enum_converter(scene_blend_factor arg)
{
    Ogre::SceneBlendFactor converted;
    switch(arg)
    {
        case SBF_ONE:
            converted = Ogre::SBF_ONE;
            break;

        case SBF_ZERO:
            converted = Ogre::SBF_ZERO;
            break;

        case SBF_DEST_COLOUR:
            converted = Ogre::SBF_DEST_COLOUR;
            break;

        case SBF_SOURCE_COLOUR:
            converted = Ogre::SBF_SOURCE_COLOUR;
            break;

        case SBF_ONE_MINUS_DEST_COLOUR:
            converted = Ogre::SBF_ONE_MINUS_DEST_COLOUR;
            break;

        case SBF_ONE_MINUS_SOURCE_COLOUR:
            converted = Ogre::SBF_ONE_MINUS_SOURCE_COLOUR;
            break;

        case SBF_DEST_ALPHA:
            converted = Ogre::SBF_DEST_ALPHA;
            break;

        case SBF_SOURCE_ALPHA:
            converted = Ogre::SBF_SOURCE_ALPHA;
            break;

        case SBF_ONE_MINUS_DEST_ALPHA:
            converted = Ogre::SBF_ONE_MINUS_DEST_ALPHA;
            break;

        case SBF_ONE_MINUS_SOURCE_ALPHA:
            converted = Ogre::SBF_ONE_MINUS_SOURCE_ALPHA;
            break;

    }
    return converted;
}

scene_blend_factor enum_converter(Ogre::SceneBlendFactor arg)
{
    scene_blend_factor converted;
    switch(arg)
    {
        case Ogre::SBF_ONE:
            converted = SBF_ONE;
            break;

        case Ogre::SBF_ZERO:
            converted = SBF_ZERO;
            break;

        case Ogre::SBF_DEST_COLOUR:
            converted = SBF_DEST_COLOUR;
            break;

        case Ogre::SBF_SOURCE_COLOUR:
            converted = SBF_SOURCE_COLOUR;
            break;

        case Ogre::SBF_ONE_MINUS_DEST_COLOUR:
            converted = SBF_ONE_MINUS_DEST_COLOUR;
            break;

        case Ogre::SBF_ONE_MINUS_SOURCE_COLOUR:
            converted = SBF_ONE_MINUS_SOURCE_COLOUR;
            break;

        case Ogre::SBF_DEST_ALPHA:
            converted = SBF_DEST_ALPHA;
            break;

        case Ogre::SBF_SOURCE_ALPHA:
            converted = SBF_SOURCE_ALPHA;
            break;

        case Ogre::SBF_ONE_MINUS_DEST_ALPHA:
            converted = SBF_ONE_MINUS_DEST_ALPHA;
            break;

        case Ogre::SBF_ONE_MINUS_SOURCE_ALPHA:
            converted = SBF_ONE_MINUS_SOURCE_ALPHA;
            break;

    }
    return converted;
}

Ogre::LayerBlendOperation enum_converter(layer_blend_operation arg)
{
    Ogre::LayerBlendOperation converted;
    switch(arg)
    {
        case LBO_REPLACE:
            converted = Ogre::LBO_REPLACE;
            break;

        case LBO_ADD:
            converted = Ogre::LBO_ADD;
            break;

        case LBO_MODULATE:
            converted = Ogre::LBO_MODULATE;
            break;

        case LBO_ALPHA_BLEND:
            converted = Ogre::LBO_ALPHA_BLEND;
            break;

    }
    return converted;
}

layer_blend_operation enum_converter(Ogre::LayerBlendOperation arg)
{
    layer_blend_operation converted;
    switch(arg)
    {
        case Ogre::LBO_REPLACE:
            converted = LBO_REPLACE;
            break;

        case Ogre::LBO_ADD:
            converted = LBO_ADD;
            break;

        case Ogre::LBO_MODULATE:
            converted = LBO_MODULATE;
            break;

        case Ogre::LBO_ALPHA_BLEND:
            converted = LBO_ALPHA_BLEND;
            break;

    }
    return converted;
}



// OgreCommon.h
Ogre::CompareFunction enum_converter(compare_function func)
{
    Ogre::CompareFunction converted;

    switch(func)
    {
        case CMPF_ALWAYS_FAIL:
            converted = Ogre::CMPF_ALWAYS_FAIL;
            break;

        case CMPF_ALWAYS_PASS:
            converted = Ogre::CMPF_ALWAYS_PASS;
            break;

        case CMPF_LESS:
            converted = Ogre::CMPF_LESS;
            break;

        case CMPF_LESS_EQUAL:
            converted = Ogre::CMPF_LESS_EQUAL;
            break;

        case CMPF_EQUAL:
            converted = Ogre::CMPF_EQUAL;
            break;

        case CMPF_NOT_EQUAL:
            converted = Ogre::CMPF_NOT_EQUAL;
            break;

        case CMPF_GREATER_EQUAL:
            converted = Ogre::CMPF_GREATER_EQUAL;
            break;

        case CMPF_GREATER:
            converted = Ogre::CMPF_GREATER;
            break;

    }

    return converted;
}

compare_function enum_converter(Ogre::CompareFunction func)
{

    compare_function converted;

    switch(func)
    {
        case Ogre::CMPF_ALWAYS_FAIL:
            converted = CMPF_ALWAYS_FAIL;
            break;

        case Ogre::CMPF_ALWAYS_PASS:
            converted = CMPF_ALWAYS_PASS;
            break;

        case Ogre::CMPF_LESS:
            converted = CMPF_LESS;
            break;

        case Ogre::CMPF_LESS_EQUAL:
            converted = CMPF_LESS_EQUAL;
            break;

        case Ogre::CMPF_EQUAL:
            converted = CMPF_EQUAL;
            break;

        case Ogre::CMPF_NOT_EQUAL:
            converted = CMPF_NOT_EQUAL;
            break;

        case Ogre::CMPF_GREATER_EQUAL:
            converted = CMPF_GREATER_EQUAL;
            break;

        case Ogre::CMPF_GREATER:
            converted = CMPF_GREATER;
            break;

        
    }

    return converted;
}


Ogre::TextureFilterOptions enum_converter(texture_filter_options opts)
{
    Ogre::TextureFilterOptions converted;
    switch(opts)
    {
        case TFO_NONE:
            converted = Ogre::TFO_NONE;
            break;

        case TFO_BILINEAR:
            converted = Ogre::TFO_BILINEAR;
            break;

        case TFO_TRILINEAR:
            converted = Ogre::TFO_TRILINEAR;
            break;

        case TFO_ANISOTROPIC:
            converted = Ogre::TFO_ANISOTROPIC;
            break;


    }
    return converted;
}

texture_filter_options enum_converter(Ogre::TextureFilterOptions opts)
{
    texture_filter_options converted;

    switch(opts)
    {
        case Ogre::TFO_NONE:
            converted = TFO_NONE;
            break;

        case Ogre::TFO_BILINEAR:
            converted = TFO_BILINEAR;
            break;

        case Ogre::TFO_TRILINEAR:
            converted = TFO_TRILINEAR;
            break;

        case Ogre::TFO_ANISOTROPIC:
            converted = TFO_ANISOTROPIC;
            break;

    }
    return converted;
}


Ogre::FilterType enum_converter(filter_type ftype)
{
    Ogre::FilterType converted;

    switch(ftype)
    {
        case FT_MIN:
            converted = Ogre::FT_MIN;
            break;

        case FT_MAG:
            converted = Ogre::FT_MAG;
            break;

        case FT_MIP:
            converted = Ogre::FT_MIP;
            break;

    }

    return converted;
}

filter_type enum_converter(Ogre::FilterType ftype)
{
    filter_type converted;

    switch(ftype)
    {
        case Ogre::FT_MIN:
            converted = FT_MIN;
            break;

        case Ogre::FT_MAG:
            converted = FT_MAG;
            break;

        case Ogre::FT_MIP:
            converted = FT_MIP;
            break;
    }

    return converted;
}


Ogre::FilterOptions enum_converter(filter_options opts)
{
    Ogre::FilterOptions converted;

    switch(opts)
    {
        case FO_NONE:
            converted = Ogre::FO_NONE;
            break;

        case FO_POINT:
            converted = Ogre::FO_POINT;
            break;

        case FO_LINEAR:
            converted = Ogre::FO_LINEAR;
            break;

        case FO_ANISOTROPIC:
            converted = Ogre::FO_ANISOTROPIC;
            break;

    }
    return converted;
}

filter_options enum_converter(Ogre::FilterOptions opts)
{
    filter_options converted;
    switch(opts)
    {
        case Ogre::FO_NONE:
            converted = FO_NONE;
            break;

        case Ogre::FO_POINT:
            converted = FO_POINT;
            break;

        case Ogre::FO_LINEAR:
            converted = FO_LINEAR;
            break;

        case Ogre::FO_ANISOTROPIC:
            converted = FO_ANISOTROPIC;
            break;

    }
    return converted;
}

Ogre::CullingMode enum_converter(culling_mode arg)
{
    Ogre::CullingMode converted;
    switch(arg)
    {
        case CULL_NONE:
            converted = Ogre::CULL_NONE;
            break;

        case CULL_CLOCKWISE:
            converted = Ogre::CULL_CLOCKWISE;
            break;

        case CULL_ANTICLOCKWISE:
            converted = Ogre::CULL_ANTICLOCKWISE;
            break;

    }
    return converted;
}

culling_mode enum_converter(Ogre::CullingMode arg)
{
    culling_mode converted;
    switch(arg)
    {
        case Ogre::CULL_NONE:
            converted = CULL_NONE;
            break;

        case Ogre::CULL_CLOCKWISE:
            converted = CULL_CLOCKWISE;
            break;

        case Ogre::CULL_ANTICLOCKWISE:
            converted = CULL_ANTICLOCKWISE;
            break;

    }
    return converted;
}

Ogre::ManualCullingMode enum_converter(manual_culling_mode arg)
{
    Ogre::ManualCullingMode converted;
    switch(arg)
    {
        case MANUAL_CULL_NONE:
            converted = Ogre::MANUAL_CULL_NONE;
            break;

        case MANUAL_CULL_BACK:
            converted = Ogre::MANUAL_CULL_BACK;
            break;

        case MANUAL_CULL_FRONT:
            converted = Ogre::MANUAL_CULL_FRONT;
            break;
    }
    return converted;
}

manual_culling_mode enum_converter(Ogre::ManualCullingMode arg)
{
    manual_culling_mode converted;
    switch(arg)
    {
        case Ogre::MANUAL_CULL_NONE:
            converted = MANUAL_CULL_NONE;
            break;

        case Ogre::MANUAL_CULL_BACK:
            converted = MANUAL_CULL_BACK;
            break;

        case Ogre::MANUAL_CULL_FRONT:
            converted = MANUAL_CULL_FRONT;
            break;

    }
    return converted;
}

Ogre::ShadeOptions enum_converter(shade_options arg)
{
    Ogre::ShadeOptions converted;
    switch(arg)
    {
        case SO_FLAT:
            converted = Ogre::SO_FLAT;
            break;

        case SO_GOURAUD:
            converted = Ogre::SO_GOURAUD;
            break;

        case SO_PHONG:
            converted = Ogre::SO_PHONG;
            break;
    }
    return converted;
}

shade_options enum_converter(Ogre::ShadeOptions arg)
{
    shade_options converted;
    switch(arg)
    {
        case Ogre::SO_FLAT:
            converted = SO_FLAT;
            break;

        case Ogre::SO_GOURAUD:
            converted = SO_GOURAUD;
            break;

        case Ogre::SO_PHONG:
            converted = SO_PHONG;
            break;

    }
    return converted;
}

Ogre::FogMode enum_converter(fog_mode arg)
{
    Ogre::FogMode converted;
    switch(arg)
    {
        case FOG_NONE:
            converted = Ogre::FOG_NONE;
            break;

        case FOG_EXP:
            converted = Ogre::FOG_EXP;
            break;

        case FOG_EXP2:
            converted = Ogre::FOG_EXP2;
            break;

        case FOG_LINEAR:
            converted = Ogre::FOG_LINEAR;
            break;

    }
    return converted;
}

fog_mode enum_converter(Ogre::FogMode arg)
{
    fog_mode converted;
    switch(arg)
    {
        case Ogre::FOG_NONE:
            converted = FOG_NONE;
            break;

        case Ogre::FOG_EXP:
            converted = FOG_EXP;
            break;

        case Ogre::FOG_EXP2:
            converted = FOG_EXP2;
            break;

        case Ogre::FOG_LINEAR:
            converted = FOG_LINEAR;
            break;
    }
    return converted;
}

Ogre::PolygonMode enum_converter(polygon_mode arg)
{
    Ogre::PolygonMode converted;
    switch(arg)
    {
        case PM_POINTS:
            converted = Ogre::PM_POINTS;
            break;

        case PM_WIREFRAME:
            converted = Ogre::PM_WIREFRAME;
            break;

        case PM_SOLID:
            converted = Ogre::PM_SOLID;
            break;

    }
    return converted;
}

polygon_mode enum_converter(Ogre::PolygonMode arg)
{
    polygon_mode converted;

    switch(arg)
    {
        case Ogre::PM_POINTS:
            converted = PM_POINTS;
            break;

        case Ogre::PM_WIREFRAME:
            converted = PM_WIREFRAME;
            break;

        case Ogre::PM_SOLID:
            converted = PM_SOLID;
            break;

    }
    return converted;
}

