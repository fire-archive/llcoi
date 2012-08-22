/******************************************************************************
 * overlay_bind.cpp  -  bindings for Ogre::Overlay
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

#include "overlay_bind.h"
#include "binding_utils.h"
#include <OgreOverlay.h>

#include <iostream>
using std::cerr;

const char* overlay_get_name(OverlayHandle handle)
{
    Ogre::Overlay* overlay = reinterpret_cast<Ogre::Overlay*>(handle);
    return overlay->getName().c_str();
}

void overlay_set_zorder(OverlayHandle handle, unsigned short zorder)
{
    Ogre::Overlay* overlay = reinterpret_cast<Ogre::Overlay*>(handle);
    overlay->setZOrder(zorder);
}

unsigned short overlay_get_zorder(OverlayHandle handle)
{
    Ogre::Overlay* overlay = reinterpret_cast<Ogre::Overlay*>(handle);
    return overlay->getZOrder();
}

int overlay_is_visible(OverlayHandle handle)
{
    Ogre::Overlay* overlay = reinterpret_cast<Ogre::Overlay*>(handle);
    return overlay->isVisible();
}

int overlay_is_initialised(OverlayHandle handle)
{
    Ogre::Overlay* overlay = reinterpret_cast<Ogre::Overlay*>(handle);
    return overlay->isInitialised();
}

void overlay_show(OverlayHandle handle)
{
    Ogre::Overlay* overlay = reinterpret_cast<Ogre::Overlay*>(handle);
    overlay->show();
}

void overlay_hide(OverlayHandle handle)
{
    Ogre::Overlay* overlay = reinterpret_cast<Ogre::Overlay*>(handle);
    overlay->hide();
}

void overlay_add_2d(OverlayHandle handle, OverlayContainerHandle c)
{
    cerr << "casting overlay\n";
    Ogre::Overlay* overlay = static_cast<Ogre::Overlay*>(handle);

    cerr << "casting container\n";
    Ogre::OverlayContainer* cont = static_cast<Ogre::OverlayContainer*>(c);
    cerr << "calling add2D\n";
    overlay->add2D(cont);
}

void overlay_remove_2d(OverlayHandle handle, OverlayContainerHandle c)
{
    Ogre::Overlay* overlay = reinterpret_cast<Ogre::Overlay*>(handle);
    Ogre::OverlayContainer* cont = reinterpret_cast<Ogre::OverlayContainer*>(c);
    overlay->remove2D(cont);
}

void overlay_add_3d(OverlayHandle handle, SceneNodeHandle node_handle)
{
    Ogre::Overlay* overlay = reinterpret_cast<Ogre::Overlay*>(handle);
    Ogre::SceneNode* sn    = reinterpret_cast<Ogre::SceneNode*>(node_handle);
    overlay->add3D(sn);
}

void overlay_remove_3d(OverlayHandle handle, SceneNodeHandle node_handle)
{
    Ogre::Overlay* overlay = reinterpret_cast<Ogre::Overlay*>(handle);
    Ogre::SceneNode* sn    = reinterpret_cast<Ogre::SceneNode*>(node_handle);
    overlay->remove3D(sn);
}

void overlay_clear(OverlayHandle handle)
{
    Ogre::Overlay* overlay = reinterpret_cast<Ogre::Overlay*>(handle);
    overlay->clear();
}

void overlay_set_scroll(OverlayHandle handle, coiReal x, coiReal y)
{
    Ogre::Overlay* overlay = reinterpret_cast<Ogre::Overlay*>(handle);
    overlay->setScroll(x,y);
}

coiReal overlay_get_scroll_x(OverlayHandle handle)
{
    Ogre::Overlay* overlay = reinterpret_cast<Ogre::Overlay*>(handle);
    return overlay->getScrollX();
}

coiReal overlay_get_scroll_y(OverlayHandle handle)
{
    Ogre::Overlay* overlay = reinterpret_cast<Ogre::Overlay*>(handle);
    return overlay->getScrollY();
}

void overlay_scroll(OverlayHandle handle, coiReal x, coiReal y)
{
    Ogre::Overlay* overlay = reinterpret_cast<Ogre::Overlay*>(handle);
    overlay->scroll(x,y);
}

void overlay_set_rotate(OverlayHandle handle, coiReal angle)
{
    Ogre::Overlay* overlay = reinterpret_cast<Ogre::Overlay*>(handle);
    overlay->setRotate(Ogre::Radian(angle));
}

coiReal overlay_get_rotate(OverlayHandle handle)
{
    Ogre::Overlay* overlay = reinterpret_cast<Ogre::Overlay*>(handle);
    return overlay->getRotate().valueRadians();
}

void overlay_rotate(OverlayHandle handle, coiReal angle)
{
    Ogre::Overlay* overlay = reinterpret_cast<Ogre::Overlay*>(handle);
    overlay->rotate(Ogre::Radian(angle));
}

void overlay_set_scale(OverlayHandle handle, coiReal x, coiReal y)
{
    Ogre::Overlay* overlay = reinterpret_cast<Ogre::Overlay*>(handle);
    overlay->setScale(x,y);
}

coiReal overlay_get_scale_x(OverlayHandle handle)
{
    Ogre::Overlay* overlay = reinterpret_cast<Ogre::Overlay*>(handle);
    return overlay->getScaleX();
}

coiReal overlay_get_scale_y(OverlayHandle handle)
{
    Ogre::Overlay* overlay = reinterpret_cast<Ogre::Overlay*>(handle);
    return overlay->getScaleY();
}

void overlay_get_world_transforms(OverlayHandle handle, coiMatrix4* xform)
{
    Ogre::Overlay* overlay = reinterpret_cast<Ogre::Overlay*>(handle);
    Ogre::Matrix4 m;
    overlay->_getWorldTransforms(&m);
    ogre_matrix4_to_llcoi_matrix4(m, *xform);
}

const char * overlay_get_origin(OverlayHandle handle)
{
    Ogre::Overlay* overlay = reinterpret_cast<Ogre::Overlay*>(handle);
    return overlay->getOrigin().c_str();
}

void overlay_notify_origin(OverlayHandle handle, const char* origin)
{
    Ogre::Overlay* overlay = reinterpret_cast<Ogre::Overlay*>(handle);
    overlay->_notifyOrigin(Ogre::String(origin));
}
