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
DLL void overlaycontainer__positions_out_of_date(OverlayContainerHandle handle);
//void _update(void);
DLL void overlaycontainer__update(OverlayContainerHandle handle);
//ushort _notifyZOrder(ushort newZOrder);
DLL unsigned short overlaycontainer__notify_zorder(OverlayContainerHandle handle, unsigned short new_zorder);
//void _notifyViewport();
DLL void overlaycontainer__notify_viewport(OverlayContainerHandle handle);
//void _notifyWorldTransforms(const Matrix4& xform);
DLL void overlaycontainer__notify_world_transforms(OverlayContainerHandle handle, const coiMatrix4* xform);
//void _notifyParent(OverlayContainer* parent, Overlay* overlay);
DLL void overlaycontainer__notify_parent(OverlayContainerHandle handle, OverlayContainerHandle parent_handle, OverlayHandle overlay_handle);
// TODO: void _updateRenderQueue(RenderQueue* queue);
//bool isContainer() const;
DLL int overlaycontainer_is_container(OverlayContainerHandle handle);
//bool isChildrenProcessEvents() const;
DLL int overlaycontainer_is_children_process_events(OverlayContainerHandle handle);
//void setChildrenProcessEvents(bool val);
DLL void overlaycontainer_set_children_process_events(OverlayContainerHandle handle, int val);
//OverlayElement* findElementAt(Real x, Real y);
DLL OverlayElementHandle overlaycontainer_find_element_at(OverlayContainerHandle handle, coiReal x, coiReal y);
//void copyFromTemplate(OverlayElement* templateOverlay);
DLL void overlaycontainer_copy_from_template(OverlayContainerHandle handle, OverlayElementHandle template_overlay);
//virtual OverlayElement* clone(const String& instanceName);
DLL OverlayElementHandle overlaycontainer_clone(OverlayContainerHandle handle, const char* instance_name);
#endif
