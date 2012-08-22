/******************************************************************************
 * overlaymanager_bind.h -  bindings for Ogre::OverlayManager
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
#ifndef OVERLAYMANAGER_BIND_H
#define OVERLAYMANAGER_BIND_H

#include "ogre_interface.h"
#define OverlayManagerHandle void*
#define OverlayHandle void*

// overlaymanager_
//OverlayManager();
DLL OverlayManagerHandle create_overlaymanager();
//~OverlayManager();
DLL void destroy_overlaymanager(OverlayManagerHandle handle);
//TODO const StringVector& getScriptPatterns(void) const;
//TODO void parseScript(DataStreamPtr& stream, const String& groupName);
//Real getLoadingOrder(void) const;
DLL coiReal overlaymanager_get_loading_order(OverlayManagerHandle handle);
//Overlay* create(const String& name);
DLL OverlayHandle overlaymanager_create(OverlayManagerHandle handle, const char* name);
//Overlay* getByName(const String& name);
DLL OverlayHandle overlaymanager_get_by_name(OverlayManagerHandle handle, const char* name);
//void destroy(const String& name);
DLL void overlaymanager_destroy_by_name(OverlayManagerHandle handle, const char* name);
//void destroy(Overlay* overlay);
DLL void overlaymanager_destroy(OverlayManagerHandle handle, OverlayHandle overlay_handle);
//void destroyAll(void);
DLL void overlaymanager_destroy_all(OverlayManagerHandle handle);
//typedef MapIterator<OverlayMap> OverlayMapIterator;
//TODO OverlayMapIterator getOverlayIterator(void);
//TODO void _queueOverlaysForRendering(Camera* cam, RenderQueue* pQueue, Viewport *vp);
//bool hasViewportChanged(void) const;
DLL int overlaymanager_has_viewport_changed(OverlayManagerHandle handle);
//int getViewportHeight(void) const;
DLL int overlaymanager_get_viewport_height(OverlayManagerHandle handle);
//int getViewportWidth(void) const;
DLL int overlaymanager_get_viewport_width(OverlayManagerHandle handle);
//Real getViewportAspectRatio(void) const;
DLL coiReal overlaymanager_get_viewport_aspect_ratio(OverlayManagerHandle handle);
//OrientationMode getViewportOrientationMode(void) const;
DLL orientation_mode overlaymanager_get_viewport_orientation_mode(OverlayManagerHandle handle);
//TODO OverlayElement* createOverlayElement(const String& typeName, const String& instanceName, bool isTemplate = false);
//TODO OverlayElement* getOverlayElement(const String& name, bool isTemplate = false);
//bool hasOverlayElement(const String& name, bool isTemplate = false);
DLL int overlaymanager_has_overlay_element(OverlayManagerHandle handle, const char* name, int is_template);
//void destroyOverlayElement(const String& instanceName, bool isTemplate = false);
DLL void overlaymanager_destroy_overlay_element(OverlayManagerHandle handle, const char* name, int is_template);
//TODO: void destroyOverlayElement(OverlayElement* pInstance, bool isTemplate = false);
//void destroyAllOverlayElements(bool isTemplate = false);
DLL void overlaymanager_destroy_all_overlay_elements(OverlayManagerHandle handle);
//TODO: OverlayElement* createOverlayElementFromTemplate(const String& templateName, const String& typeName, const String& instanceName, bool isTemplate = false);
//TODO: OverlayElement* cloneOverlayElementFromTemplate(const String& templateName, const String& instanceName);
//TODO: OverlayElement* createOverlayElementFromFactory(const String& typeName, const String& instanceName);
//bool isTemplate (String strName) const
DLL int overlaymanager_is_template(OverlayManagerHandle handle, const char* name);
//static OverlayManager* getSingletonPtr(void);
DLL OverlayManagerHandle overlaymanager_get_singleton_ptr();
DLL OverlayManagerHandle overlaymanager_get_singleton();


#endif
