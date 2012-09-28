/******************************************************************************
 * resourcemanager_bind.cpp - bindings for Ogre::ResourceManager
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

#include "resourcemanager_bind.h"
#include <OgreResourceManager.h>


//~ResourceManager()
void destroy_resourcemanager(ResourceManagerHandle handle)
{
    Ogre::ResourceManager* manager = static_cast<Ogre::ResourceManager*>(handle);
    delete manager;
}

//ResourcePtr create(const String& name, const String& group, bool isManual = false, ManualResourceLoader* loader = 0, const NameValuePairList* createParams = 0)
ResourcePtrHandle resourcemanager_create(ResourceManagerHandle handle, const char* name, const char* group, int is_manual, ManualResourceLoaderHandle l, const NameValuePairListHandle nvp)
{
    Ogre::ResourceManager* manager = static_cast<Ogre::ResourceManager*>(handle); 
    Ogre::ManualResourceLoader* loader = static_cast<Ogre::ManualResourceLoader*>(l);
    const Ogre::NameValuePairList* params = static_cast<const Ogre::NameValuePairList*>(nvp);

    Ogre::ResourcePtr result = manager->create(Ogre::String(name), Ogre::String(group), is_manual, loader, params);

    return static_cast<ResourcePtrHandle>(result.get());
}

//typedef std::pair<ResourcePtr, bool> ResourceCreateOrRetrieveResult
//TODO: ResourceCreateOrRetrieveResult createOrRetrieve(const String& name, const String& group, bool isManual = false, ManualResourceLoader* loader = 0, const NameValuePairList* createParams = 0)

//void setMemoryBudget( size_t bytes)
void resourcemanager_set_memory_budget(ResourceManagerHandle handle, size_t bytes)
{
    Ogre::ResourceManager* manager = static_cast<Ogre::ResourceManager*>(handle);
    return manager->setMemoryBudget(bytes);
}

//size_t getMemoryBudget(void) const
size_t resourcemanager_get_memory_budget(const ResourceManagerHandle handle)
{
    const Ogre::ResourceManager* manager = static_cast<const Ogre::ResourceManager*>(handle);
    return manager->getMemoryBudget();
}

//size_t getMemoryUsage(void) const
size_t resourcemanager_get_memory_usage(const ResourceManagerHandle handle)
{
    const Ogre::ResourceManager* manager = static_cast<const Ogre::ResourceManager*>(handle);
    return manager->getMemoryUsage();
}

//void unload(const String& name)
void resourcemanager_unload_by_name(ResourceManagerHandle handle, const char* name)
{
    Ogre::ResourceManager* manager = static_cast<Ogre::ResourceManager*>(handle);
    manager->unload(Ogre::String(name));
}

//void unload(ResourceHandle handle)
void resourcemanager_unload_by_handle(ResourceManagerHandle handle, ResourceHandle res_handle)
{
    Ogre::ResourceManager* manager = static_cast<Ogre::ResourceManager*>(handle);
    manager->unload(res_handle);
}

//void unloadAll(bool reloadableOnly = true)
void resourcemanager_unload_all(ResourceManagerHandle handle, int reloadable_only)
{
    Ogre::ResourceManager* manager = static_cast<Ogre::ResourceManager*>(handle);
    manager->unloadAll(reloadable_only);
}

//void reloadAll(bool reloadableOnly = true)
void resourcemanager_reload_all(ResourceManagerHandle handle, int reloadable_only)
{
    Ogre::ResourceManager* manager = static_cast<Ogre::ResourceManager*>(handle);
    manager->reloadAll(reloadable_only);
}

//void unloadUnreferencedResources(bool reloadableOnly = true)
void resourcemanager_unload_unreferenced_resources(ResourceManagerHandle handle, int reloadable_only)
{
    Ogre::ResourceManager* manager = static_cast<Ogre::ResourceManager*>(handle);
    manager->unloadUnreferencedResources(reloadable_only);
}

//void reloadUnreferencedResources(bool reloadableOnly = true)
void resourcemanager_reload_unreferenced_resources(ResourceManagerHandle handle, int reloadable_only)
{
    Ogre::ResourceManager* manager = static_cast<Ogre::ResourceManager*>(handle);
    manager->reloadUnreferencedResources(reloadable_only);
}

//void remove(ResourcePtr& r)
void resourcemanager_remove_by_ptr(ResourceManagerHandle handle, ResourcePtrHandle resource_ptr)
{
    Ogre::ResourceManager* manager = static_cast<Ogre::ResourceManager*>(handle);
    Ogre::ResourcePtr ptr(static_cast<Ogre::Resource*>(resource_ptr));
    manager->remove(ptr);
}

//void remove(const String& name)
void resourcemanager_remove_by_name(ResourceManagerHandle handle, const char* name)
{
    Ogre::ResourceManager* manager = static_cast<Ogre::ResourceManager*>(handle);
    manager->remove(Ogre::String(name));
}

//void remove(ResourceHandle handle)
void resourcemanager_remove_by_handle(ResourceManagerHandle handle, ResourceHandle res_handle)
{
    Ogre::ResourceManager* manager = static_cast<Ogre::ResourceManager*>(handle);
    manager->remove(res_handle);

}

//void removeAll(void)
void resourcemanager_remove_all(ResourceManagerHandle handle)
{
    Ogre::ResourceManager* manager = static_cast<Ogre::ResourceManager*>(handle);
    manager->removeAll();
}

//void removeUnreferencedResources(bool reloadableOnly = true)
void resourcemanager_remove_unreferenced_resources(ResourceManagerHandle handle, int reloadable_only)
{
    Ogre::ResourceManager* manager = static_cast<Ogre::ResourceManager*>(handle);
    manager->removeUnreferencedResources(reloadable_only);
}

//ResourcePtr getByName(const String& name, const String& groupName = ResourceGroupManager::AUTODETECT_RESOURCE_GROUP_NAME)
ResourcePtrHandle resourcemanager_get_by_name(ResourceManagerHandle handle, const char* name, const char* group_name)
{
    Ogre::ResourceManager* manager = static_cast<Ogre::ResourceManager*>(handle);
    Ogre::ResourcePtr ptr(manager->getByName(Ogre::String(name), Ogre::String(group_name)));
    return static_cast<ResourcePtrHandle>(ptr.get());
}

//ResourcePtr getByHandle(ResourceHandle handle)
ResourcePtrHandle resourcemanager_get_by_handle(ResourceManagerHandle handle, ResourceHandle res_handle)
{
    Ogre::ResourceManager* manager = static_cast<Ogre::ResourceManager*>(handle);
    Ogre::ResourcePtr ptr(manager->getByHandle(res_handle));
    return static_cast<ResourcePtrHandle>(ptr.get());
}

//bool resourceExists(const String& name)
int resourcemanager_exists_by_name(ResourceManagerHandle handle, const char* name)
{
    Ogre::ResourceManager* manager = static_cast<Ogre::ResourceManager*>(handle);
    return manager->resourceExists(Ogre::String(name));
}

//bool resourceExists(ResourceHandle handle)
int resourcemanager_exists_by_handle(ResourceManagerHandle handle, ResourceHandle res_handle)
{
    Ogre::ResourceManager* manager = static_cast<Ogre::ResourceManager*>(handle);
    return manager->resourceExists(res_handle);
}

//void _notifyResourceTouched(Resource* res)
void resourcemanager__notify_resource_touched(ResourceManagerHandle handle, coiResourceHandle res)
{
    Ogre::ResourceManager* manager = static_cast<Ogre::ResourceManager*>(handle);
    Ogre::Resource* resource = static_cast<Ogre::Resource*>(res);
    manager->_notifyResourceTouched(resource);
}

//void _notifyResourceLoaded(Resource* res)
void resourcemanager__notify_resource_loaded(ResourceManagerHandle handle, coiResourceHandle res)
{
    Ogre::ResourceManager* manager = static_cast<Ogre::ResourceManager*>(handle);
    Ogre::Resource* resource = static_cast<Ogre::Resource*>(res);
    manager->_notifyResourceLoaded(resource);
}

//void _notifyResourceUnloaded(Resource* res)
void resourcemanager__notify_resource_unloaded(ResourceManagerHandle handle, coiResourceHandle res)
{
    Ogre::ResourceManager* manager = static_cast<Ogre::ResourceManager*>(handle);
    Ogre::Resource* resource = static_cast<Ogre::Resource*>(res);
    manager->_notifyResourceUnloaded(resource);
}

//ResourcePtr prepare(const String& name, const String& group, bool isManual = false, ManualResourceLoader* loader = 0, const NameValuePairList* loadParams = 0, bool backgroundThread = false)
ResourcePtrHandle resourcemanager_prepare(ResourceManagerHandle handle, const char* name, const char* group, int is_manual, ManualResourceLoaderHandle l, NameValuePairListHandle nvp, int background_thread)
{
    Ogre::ResourceManager* manager = static_cast<Ogre::ResourceManager*>(handle);
    Ogre::ManualResourceLoader* loader = static_cast<Ogre::ManualResourceLoader*>(l);
    const Ogre::NameValuePairList* params = static_cast<const Ogre::NameValuePairList*>(nvp);

    Ogre::ResourcePtr ptr(
        manager->prepare(Ogre::String(name), Ogre::String(group), is_manual, loader, params, background_thread)
    );
    return static_cast<ResourcePtrHandle>(ptr.get());
}

//ResourcePtr load(const String& name, const String& group, bool isManual = false, ManualResourceLoader* loader = 0, const NameValuePairList* loadParams = 0, bool backgroundThread = false)
ResourcePtrHandle resourcemanager_load(ResourceManagerHandle handle, const char* name, const char* group, int is_manual, ManualResourceLoaderHandle l, NameValuePairListHandle nvp, int background_thread)
{

    Ogre::ResourceManager* manager = static_cast<Ogre::ResourceManager*>(handle);
    Ogre::ManualResourceLoader* loader = static_cast<Ogre::ManualResourceLoader*>(l);
    const Ogre::NameValuePairList* params = static_cast<const Ogre::NameValuePairList*>(nvp);

    Ogre::ResourcePtr ptr(
        manager->load(Ogre::String(name), Ogre::String(group), is_manual, loader, params, background_thread)
    );

    return static_cast<ResourcePtrHandle>(ptr.get());
}

//TODO:const StringVector& getScriptPatterns(void) const
//TODO: void parseScript(DataStreamPtr& stream, const String& groupName)
//Real getLoadingOrder(void) const
coiReal resourcemanager_get_loading_order(const ResourceManagerHandle handle)
{
    const Ogre::ResourceManager* manager = static_cast<const Ogre::ResourceManager*>(handle);
    return manager->getLoadingOrder();
}

//const String& getResourceType(void) const
const char* resourcemanager_get_resource_type(const ResourceManagerHandle handle)
{
    const Ogre::ResourceManager* manager = static_cast<const Ogre::ResourceManager*>(handle);
    return manager->getResourceType().c_str();
}

//void setVerbose(bool v)
void resourcemanager_set_verbose(ResourceManagerHandle handle, int v)
{
    Ogre::ResourceManager* manager = static_cast<Ogre::ResourceManager*>(handle);
    manager->setVerbose(v);
}

//bool getVerbose(void)
int resourcemanager_get_verbose(ResourceManagerHandle handle)
{
    Ogre::ResourceManager* manager = static_cast<Ogre::ResourceManager*>(handle);
    return manager->getVerbose();
}

