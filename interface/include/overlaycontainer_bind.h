/******************************************************************************
 * overlaycontainer_bind.h -  bindings for Ogre::OverlayContainer
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
#ifndef OVERLAYCONTAINER_BIND_H
#define OVERLAYCONTAINER_BIND_H

#include "ogre_interface.h"
#define OverlayContainerHandle void*
#define OverlayElementHandle void*
#define OverlayHandle void*

//~OverlayContainer();
DLL void destroy_overlaycontainer(OverlayContainerHandle handle);
// overlaycontainer_
//void addChild(OverlayElement* elem);
DLL void overlaycontainer_add_child(OverlayContainerHandle handle, OverlayElementHandle child_handle);
//void addChildImpl(OverlayElement* elem);
DLL void overlaycontainer_add_child_impl(OverlayContainerHandle handle, OverlayElementHandle child_handle);
//void addChildImpl(OverlayContainer* cont);
DLL void overlaycontainer_add_child_container_impl(OverlayContainerHandle handle, OverlayContainerHandle child_handle);
//void removeChild(const String& name);
DLL void overlaycontainer_remove_child(OverlayContainerHandle handle, const char* name);
//OverlayElement* getChild(const String& name);
DLL OverlayElementHandle overlaycontainer_get_child(OverlayContainerHandle handle, const char* name);
//void initialise(void);
DLL void overlaycontainer_initialise(OverlayContainerHandle handle);
//void _addChild(OverlayElement* elem);
DLL void overlaycontainer__add_child(OverlayContainerHandle handle, OverlayElementHandle elem);
//void _removeChild(OverlayElement* elem);
DLL void overlaycontainer__remove_child(OverlayContainerHandle handle, OverlayElementHandle elem);
//void _removeChild(const String& name);
DLL void overlaycontainer__remove_child_by_name(OverlayContainerHandle handle, const char* name);
//TODO: ChildIterator getChildIterator(void);
//TODO: ChildContainerIterator getChildContainerIterator(void);
//void _positionsOutOfDate(void);
//void _update(void);
//ushort _notifyZOrder(ushort newZOrder);
//void _notifyViewport();
//void _notifyWorldTransforms(const Matrix4& xform);
//void _notifyParent(OverlayContainer* parent, Overlay* overlay);
// TODO: void _updateRenderQueue(RenderQueue* queue);
//bool isContainer() const;
//bool isChildrenProcessEvents() const;
//void setChildrenProcessEvents(bool val);
//OverlayElement* findElementAt(Real x, Real y);
//void copyFromTemplate(OverlayElement* templateOverlay);
//virtual OverlayElement* clone(const String& instanceName);
#endif
