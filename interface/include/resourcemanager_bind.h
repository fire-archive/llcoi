/******************************************************************************
 * resourcemanager_bind.h - bindings for Ogre::ResourceManager
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

// NB: referring to our handle as coiResourceHandle as Ogre already typedef'd 'ResourceHandle'
typedef void* coiResourceHandle;
typedef void* ResourcePtrHandle;
typedef void* ResourceManagerHandle;
typedef void* ManualResourceLoaderHandle;

#include "ogre_interface.h"

//~ResourceManager();
DLL void destroy_resourcemanager(ResourceManagerHandle handle);
//ResourcePtr create(const String& name, const String& group, bool isManual = false, ManualResourceLoader* loader = 0, const NameValuePairList* createParams = 0)
DLL ResourcePtrHandle resourcemanager_create(ResourceManagerHandle handle, const char* name, const char* group, int is_manual, ManualResourceLoaderHandle loader, const NameValuePairListHandle create_params);
//typedef std::pair<ResourcePtr, bool> ResourceCreateOrRetrieveResult
//TODO: ResourceCreateOrRetrieveResult createOrRetrieve(const String& name, const String& group, bool isManual = false, ManualResourceLoader* loader = 0, const NameValuePairList* createParams = 0)
//void setMemoryBudget( size_t bytes)
DLL void resourcemanager_set_memory_budget(ResourceManagerHandle handle, size_t bytes);
//size_t getMemoryBudget(void) const
DLL size_t resourcemanager_get_memory_budget(const ResourceManagerHandle handle);
//size_t getMemoryUsage(void) const
DLL size_t resourcemanager_get_memory_usage(const ResourceManagerHandle handle);
//void unload(const String& name)
DLL void resourcemanager_unload_by_name(ResourceManagerHandle handle, const char* name);
//void unload(ResourceHandle handle)
DLL void resourcemanager_unload_by_handle(ResourceManagerHandle handle, ResourceHandle res_handle);
//void unloadAll(bool reloadableOnly = true)
DLL void resourcemanager_unload_all(ResourceManagerHandle handle, int reloadable_only);
//void reloadAll(bool reloadableOnly = true)
DLL void resourcemanager_reload_all(ResourceManagerHandle handle, int reloadable_only);
//void unloadUnreferencedResources(bool reloadableOnly = true)
DLL void resourcemanager_unload_unreferenced_resources(ResourceManagerHandle handle, int reloadable_only);
//void reloadUnreferencedResources(bool reloadableOnly = true)
DLL void resourcemanager_reload_unreferenced_resources(ResourceManagerHandle handle, int reloadable_only);
//void remove(ResourcePtr& r)
DLL void resourcemanager_remove_by_ptr(ResourceManagerHandle handle, ResourcePtrHandle resource_ptr);
//void remove(const String& name)
DLL void resourcemanager_remove_by_name(ResourceManagerHandle handle, const char* name);
//void remove(ResourceHandle handle)
DLL void resourcemanager_remove_by_handle(ResourceManagerHandle handle, ResourceHandle res_handle);
//void removeAll(void)
DLL void resourcemanager_remove_all(ResourceManagerHandle handle);
//void removeUnreferencedResources(bool reloadableOnly = true)
DLL void resourcemanager_remove_unreferenced_resources(ResourceManagerHandle handle, int reloadable_only);
//ResourcePtr getByName(const String& name, const String& groupName = ResourceGroupManager::AUTODETECT_RESOURCE_GROUP_NAME)
DLL ResourcePtrHandle resourcemanager_get_by_name(ResourceManagerHandle handle, const char* name, const char* group_name);
//ResourcePtr getByHandle(ResourceHandle handle)
DLL ResourcePtrHandle resourcemanager_get_by_handle(ResourceManagerHandle handle, ResourceHandle res_handle);
//bool resourceExists(const String& name)
DLL bool resourcemanager_exists_by_name(ResourceManagerHandle handle, const char* name);
//bool resourceExists(ResourceHandle handle)
DLL bool resourcemanager_exists_by_handle(ResourceManagerHandle handle, ResourceHandle res_handle);
//void _notifyResourceTouched(Resource* res)
DLL void resourcemanager__notify_resource_touched(ResourceManagerHandle handle, coiResourceHandle res);
//void _notifyResourceLoaded(Resource* res)
DLL void resourcemanager__notify_resource_loaded(ResourceManagerHandle handle, coiResourceHandle res);
//void _notifyResourceUnloaded(Resource* res)
DLL void resourcemanager__notify_resource_unloaded(ResourceManagerHandle handle, coiResourceHandle res);
//ResourcePtr prepare(const String& name, const String& group, bool isManual = false, ManualResourceLoader* loader = 0, const NameValuePairList* loadParams = 0, bool backgroundThread = false)
DLL ResourcePtrHandle resourcemanager_prepare(ResourceManagerHandle handle, const char* name, const char* group, int is_manual, ManualResourceLoaderHandle loader, NameValuePairListHandle load_params, int background_thread);
//ResourcePtr load(const String& name, const String& group, bool isManual = false, ManualResourceLoader* loader = 0, const NameValuePairList* loadParams = 0, bool backgroundThread = false)
DLL ResourcePtrHandle resourcemanager_load(ResourceManagerHandle handle, const char* name, const char* group, int is_manual, ManualResourceLoaderHandle loader, NameValuePairListHandle load_params, int background_thread);
//TODO:const StringVector& getScriptPatterns(void) const
//TODO: void parseScript(DataStreamPtr& stream, const String& groupName)
//Real getLoadingOrder(void) const
DLL coiReal resourcemanager_get_loading_order(const ResourceManagerHandle handle);
//const String& getResourceType(void) const
DLL const char* resourcemanager_get_resource_type(const ResourceManagerHandle handle);
//void setVerbose(bool v)
DLL void resourcemanager_set_verbose(ResourceManagerHandle handle, int v);
//bool getVerbose(void)
DLL int resourcemanager_get_verbose(ResourceManagerHandle handle);
