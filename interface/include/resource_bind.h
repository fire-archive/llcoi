/******************************************************************************
 * resource_bind.h - bindings for Ogre::Resource
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
#ifndef RESOURCE_BIND_H
#define RESOURCE_BIND_H

#include "ogre_interface.h"

// NB: referring to our handle as coiResourceHandle as Ogre already typedef'd 'ResourceHandle'
#define coiResourceHandle void*
#define ResourceManagerHandle void*
#define ManualResourceLoaderHandle void*
#define ResourceListenerHandle void*

typedef void (*loadingCompleteCB)(coiResourceHandle handle, void* userdata);
typedef void (*preparingCompleteCB)(coiResourceHandle handle, void* userdata);
typedef void (*unloadingCompleteCB)(coiResourceHandle handle, void* userdata);


DLL ResourceListenerHandle create_resourcelistener(loadingCompleteCB loading_cb, preparingCompleteCB preparing_cb, unloadingCompleteCB unloading_cb, void* userdata);
DLL void destroy_resourcelistener(ResourceListenerHandle handle);

DLL void destroy_resource(coiResourceHandle handle);
//void prepare(bool backgroundThread = false)
DLL void resource_prepare(coiResourceHandle handle, int background_thread);
//void load(bool backgroundThread = false)
DLL void resource_load(coiResourceHandle handle, int background_thread);
//void reload()
DLL void resource_reload(coiResourceHandle handle);
//bool isReloadable(void) const
DLL int resource_is_reloadable(const coiResourceHandle handle);
//bool isManuallyLoaded(void) const
DLL int resource_is_manually_loaded(const coiResourceHandle handle);
//void unload(void)
DLL void resource_unload(coiResourceHandle handle);
//size_t getSize(void) const
DLL size_t resource_get_size(const coiResourceHandle handle);
//void touch(void)
DLL void resource_touch(coiResourceHandle handle);
//const String& getName(void) const 
DLL const char* resource_get_name(const coiResourceHandle handle);
//ResourceHandle getHandle(void) const
DLL ResourceHandle resource_get_handle(const coiResourceHandle handle);
//bool isPrepared(void) const 
DLL int resource_is_prepared(const coiResourceHandle handle);
//bool isLoaded(void) const 
DLL int resource_is_loaded(const coiResourceHandle handle);
//bool isLoading() const
DLL int resource_is_loading(const coiResourceHandle handle);
//LoadingState getLoadingState() const
DLL loading_state resource_get_loading_state(const coiResourceHandle handle);
//bool isBackgroundLoaded(void) const
DLL int resource_is_background_loaded(const coiResourceHandle handle);
//void setBackgroundLoaded(bool bl)
DLL void resource_set_background_loaded(coiResourceHandle handle, int bl);
//void escalateLoading()
DLL void resource_escalate_loading(coiResourceHandle handle);
//void addListener(Listener* lis)
DLL void resource_add_listener(coiResourceHandle handle, ResourceListenerHandle listener);
//void removeListener(Listener* lis)
DLL void resource_remove_listener(coiResourceHandle handle, ResourceListenerHandle listener);
//const String& getGroup(void) const
DLL const char* resource_get_group(const coiResourceHandle handle);
//void changeGroupOwnership(const String& newGroup)
DLL void  resource_change_group_ownership(coiResourceHandle handle, const char* new_group);
//ResourceManager* getCreator(void)
DLL ResourceManagerHandle resource_get_creator(coiResourceHandle handle);
//const String& getOrigin(void) const
DLL const char* resource_get_origin(const coiResourceHandle handle);
//void _notifyOrigin(const String& origin)
DLL void resource__notify_origin(coiResourceHandle handle, const char* origin);
//size_t getStateCount() const
DLL size_t resource_get_state_count(const coiResourceHandle handle);
//void _dirtyState()
DLL void resource__dirty_state(coiResourceHandle handle);
//void _fireLoadingComplete(bool wasBackgroundLoaded)
DLL void resource__fire_loading_complete(coiResourceHandle handle, int was_background_loaded);
//void _firePreparingComplete(bool wasBackgroundLoaded)
DLL void resource__fire_preparing_complete(coiResourceHandle handle, int was_background_loaded);
//void _fireUnloadingComplete(void)
DLL void resource_fire_unloading_complete(coiResourceHandle handle);
//typedef SharedPtr<Resource> ResourcePtr



#endif
