/******************************************************************************
 * overlay_bind.h -  bindings for Ogre::Overlay
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
#ifndef OVERLAY_BIND_H
#define OVERLAY_BIND_H

#include "ogre_interface.h"
#define OverlayHandle void*
#define SceneNodeHandle void*

//const String& getName(void) const;
DLL const char* overlay_get_name(OverlayHandle handle);
//void setZOrder(ushort zorder);
DLL void overlay_set_zorder(OverlayHandle handle, unsigned short zorder);
//ushort getZOrder(void) const;
DLL unsigned short overlay_get_zorder(OverlayHandle handle);
//bool isVisible(void) const;
DLL int overlay_is_visible(OverlayHandle handle);
//bool isInitialised(void) const;
DLL int overlay_is_initialised(OverlayHandle handle);
//void show(void);
DLL void overlay_show(OverlayHandle handle);
//void hide(void);
DLL void overlay_hide(OverlayHandle handle);
//void add2D(OverlayContainer* cont);
//void remove2D(OverlayContainer* cont);
//void add3D(SceneNode* node);
DLL void overlay_add_3d(OverlayHandle handle, SceneNodeHandle node_handle);
//void remove3D(SceneNode* node);
DLL void overlay_remove_3d(OverlayHandle handle, SceneNodeHandle node_handle);
// void clear();
DLL void overlay_clear(OverlayHandle handle);
//void setScroll(Real x, Real y);
DLL void overlay_set_scroll(OverlayHandle handle, coiReal x, coiReal y);
//Real getScrollX(void) const;
DLL coiReal overlay_get_scroll_x(OverlayHandle handle);
//Real getScrollY(void) const;
DLL coiReal overlay_get_scroll_y(OverlayHandle handle);
//void scroll(Real xoff, Real yoff);
DLL void overlay_scroll(OverlayHandle handle, coiReal x, coiReal y);
//void setRotate(const Radian& angle);
DLL void overlay_set_rotate(OverlayHandle handle, coiReal angle);
//const Radian &getRotate(void) const;
DLL coiReal overlay_get_rotate(OverlayHandle handle);
//void rotate(const Radian& angle);
DLL void overlay_rotate(OverlayHandle handle, coiReal angle);
//void setScale(Real x, Real y);
DLL void overlay_set_scale(OverlayHandle handle, coiReal x, coiReal y);
//Real getScaleX(void) const;
DLL coiReal overlay_get_scale_x(OverlayHandle handle);
//Real getScaleY(void) const;
DLL coiReal overlay_get_scale_y(OverlayHandle handle);
//void _getWorldTransforms(Matrix4* xform) const;
DLL void overlay_get_world_transforms(OverlayHandle handle, coiMatrix4* xform);
//void _findVisibleObjects(Camera* cam, RenderQueue* queue);
//OverlayElement* findElementAt(Real x, Real y);
//const String& getOrigin(void) const;
DLL const char * overlay_get_origin(OverlayHandle handle);
//void _notifyOrigin(const String& origin);
DLL void overlay_notify_origin(OverlayHandle handle, const char* origin);


#endif
