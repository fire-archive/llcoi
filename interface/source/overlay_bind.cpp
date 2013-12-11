/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "overlay_bind.h"
#include "binding_utils.h"
#include <Overlay/OgreOverlay.h>

const char* overlay_get_name(OverlayHandle handle)
{
    Ogre::Overlay* overlay = static_cast<Ogre::Overlay*>(handle);
    return overlay->getName().c_str();
}

void overlay_set_zorder(OverlayHandle handle, unsigned short zorder)
{
    Ogre::Overlay* overlay = static_cast<Ogre::Overlay*>(handle);
    overlay->setZOrder(zorder);
}

unsigned short overlay_get_zorder(OverlayHandle handle)
{
    Ogre::Overlay* overlay = static_cast<Ogre::Overlay*>(handle);
    return overlay->getZOrder();
}

int overlay_is_visible(OverlayHandle handle)
{
    Ogre::Overlay* overlay = static_cast<Ogre::Overlay*>(handle);
    return overlay->isVisible();
}

int overlay_is_initialised(OverlayHandle handle)
{
    Ogre::Overlay* overlay = static_cast<Ogre::Overlay*>(handle);
    return overlay->isInitialised();
}

void overlay_show(OverlayHandle handle)
{
    Ogre::Overlay* overlay = static_cast<Ogre::Overlay*>(handle);
    overlay->show();
}

void overlay_hide(OverlayHandle handle)
{
    Ogre::Overlay* overlay = static_cast<Ogre::Overlay*>(handle);
    overlay->hide();
}

void overlay_add_2d(OverlayHandle handle, OverlayContainerHandle c)
{
    Ogre::Overlay* overlay = static_cast<Ogre::Overlay*>(handle);
    Ogre::OverlayContainer* cont = static_cast<Ogre::OverlayContainer*>(c);

    overlay->add2D(cont);
}

void overlay_remove_2d(OverlayHandle handle, OverlayContainerHandle c)
{
    Ogre::Overlay* overlay = static_cast<Ogre::Overlay*>(handle);
    Ogre::OverlayContainer* cont = static_cast<Ogre::OverlayContainer*>(c);
    overlay->remove2D(cont);
}

void overlay_add_3d(OverlayHandle handle, SceneNodeHandle node_handle)
{
    Ogre::Overlay* overlay = static_cast<Ogre::Overlay*>(handle);
    Ogre::SceneNode* sn    = static_cast<Ogre::SceneNode*>(node_handle);
    overlay->add3D(sn);
}

void overlay_remove_3d(OverlayHandle handle, SceneNodeHandle node_handle)
{
    Ogre::Overlay* overlay = static_cast<Ogre::Overlay*>(handle);
    Ogre::SceneNode* sn    = static_cast<Ogre::SceneNode*>(node_handle);
    overlay->remove3D(sn);
}

void overlay_clear(OverlayHandle handle)
{
    Ogre::Overlay* overlay = static_cast<Ogre::Overlay*>(handle);
    overlay->clear();
}

void overlay_set_scroll(OverlayHandle handle, coiReal x, coiReal y)
{
    Ogre::Overlay* overlay = static_cast<Ogre::Overlay*>(handle);
    overlay->setScroll(x,y);
}

coiReal overlay_get_scroll_x(OverlayHandle handle)
{
    Ogre::Overlay* overlay = static_cast<Ogre::Overlay*>(handle);
    return overlay->getScrollX();
}

coiReal overlay_get_scroll_y(OverlayHandle handle)
{
    Ogre::Overlay* overlay = static_cast<Ogre::Overlay*>(handle);
    return overlay->getScrollY();
}

void overlay_scroll(OverlayHandle handle, coiReal x, coiReal y)
{
    Ogre::Overlay* overlay = static_cast<Ogre::Overlay*>(handle);
    overlay->scroll(x,y);
}

void overlay_set_rotate(OverlayHandle handle, coiReal angle)
{
    Ogre::Overlay* overlay = static_cast<Ogre::Overlay*>(handle);
    overlay->setRotate(Ogre::Radian(angle));
}

coiReal overlay_get_rotate(OverlayHandle handle)
{
    Ogre::Overlay* overlay = static_cast<Ogre::Overlay*>(handle);
    return overlay->getRotate().valueRadians();
}

void overlay_rotate(OverlayHandle handle, coiReal angle)
{
    Ogre::Overlay* overlay = static_cast<Ogre::Overlay*>(handle);
    overlay->rotate(Ogre::Radian(angle));
}

void overlay_set_scale(OverlayHandle handle, coiReal x, coiReal y)
{
    Ogre::Overlay* overlay = static_cast<Ogre::Overlay*>(handle);
    overlay->setScale(x,y);
}

coiReal overlay_get_scale_x(OverlayHandle handle)
{
    Ogre::Overlay* overlay = static_cast<Ogre::Overlay*>(handle);
    return overlay->getScaleX();
}

coiReal overlay_get_scale_y(OverlayHandle handle)
{
    Ogre::Overlay* overlay = static_cast<Ogre::Overlay*>(handle);
    return overlay->getScaleY();
}

void overlay_get_world_transforms(OverlayHandle handle, coiMatrix4* xform)
{
    Ogre::Overlay* overlay = static_cast<Ogre::Overlay*>(handle);
    Ogre::Matrix4 m;
    overlay->_getWorldTransforms(&m);
    ogre_matrix4_to_llcoi_matrix4(m, *xform);
}

const char * overlay_get_origin(OverlayHandle handle)
{
    Ogre::Overlay* overlay = static_cast<Ogre::Overlay*>(handle);
    return overlay->getOrigin().c_str();
}

void overlay_notify_origin(OverlayHandle handle, const char* origin)
{
    Ogre::Overlay* overlay = static_cast<Ogre::Overlay*>(handle);
    overlay->_notifyOrigin(Ogre::String(origin));
}
