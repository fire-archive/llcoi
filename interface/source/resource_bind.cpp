/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "resource_bind.h"
#include "binding_utils.h"
#include <OgreResource.h>


class ResourceListener : public Ogre::Resource::Listener
{
public:

    ResourceListener(loadingCompleteCB loading_cb, preparingCompleteCB preparing_cb, unloadingCompleteCB unloading_cb, void* data) : loadingCB(loading_cb), 
                                                                                                                                     preparingCB(preparing_cb),
                                                                                                                                     unloadingCB(unloading_cb),
                                                                                                                                     userdata(data)
    {
    }

    virtual ~ResourceListener() {}

    void loadingComplete(Ogre::Resource* res)
    {
        loadingCB(static_cast<coiResourceHandle>(res), userdata);
    }

    void preparingComplete(Ogre::Resource* res)
    {
        preparingCB(static_cast<coiResourceHandle>(res), userdata);
    }

    void unloadingComplete(Ogre::Resource* res)
    {
        unloadingCB(static_cast<coiResourceHandle>(res), userdata);
    }

    loadingCompleteCB loadingCB;
    preparingCompleteCB preparingCB;
    unloadingCompleteCB unloadingCB;
    void* userdata;
};


ResourceListenerHandle create_resourcelistener(loadingCompleteCB loading_cb, preparingCompleteCB preparing_cb, unloadingCompleteCB unloading_cb, void* userdata)
{
    ResourceListener* listener = new ResourceListener(loading_cb, preparing_cb, unloading_cb, userdata);
    return static_cast<ResourceListenerHandle>(listener);
}

void destroy_resourcelistener(ResourceListenerHandle handle)
{
    ResourceListener* listener = static_cast<ResourceListener*>(handle);
    delete listener;
}

void destroy_resource(coiResourceHandle handle)
{
    Ogre::Resource* res = static_cast<Ogre::Resource*>(handle);
    delete res;
}

//void prepare(bool backgroundThread = false)
void resource_prepare(coiResourceHandle handle, int background_thread)
{
    Ogre::Resource* res = static_cast<Ogre::Resource*>(handle);
    res->prepare(background_thread);
}

//void load(bool backgroundThread = false)
void resource_load(coiResourceHandle handle, int background_thread)
{
    Ogre::Resource* res = static_cast<Ogre::Resource*>(handle);
    res->load(background_thread);
}

//void reload()
void resource_reload(coiResourceHandle handle)
{
    Ogre::Resource* res = static_cast<Ogre::Resource*>(handle);
    res->reload();
}

//bool isReloadable(void) const
int resource_is_reloadable(const coiResourceHandle handle)
{
    const Ogre::Resource* res = static_cast<const Ogre::Resource*>(handle);
    return res->isReloadable();
}

//bool isManuallyLoaded(void) const
int resource_is_manually_loaded(const coiResourceHandle handle)
{
    const Ogre::Resource* res = static_cast<const Ogre::Resource*>(handle);
    return res->isManuallyLoaded();
}

//void unload(void)
void resource_unload(coiResourceHandle handle)
{
    Ogre::Resource* res = static_cast<Ogre::Resource*>(handle);
    res->unload();
}

//size_t getSize(void) const
size_t resource_get_size(const coiResourceHandle handle)
{
    const Ogre::Resource* res = static_cast<const Ogre::Resource*>(handle);
    return res->getSize();
}

//void touch(void)
void resource_touch(coiResourceHandle handle)
{
    Ogre::Resource* res = static_cast<Ogre::Resource*>(handle);
    res->touch();
}

//const String& getName(void) const 
const char* resource_get_name(const coiResourceHandle handle)
{
    const Ogre::Resource* res = static_cast<const Ogre::Resource*>(handle);
    return res->getName().c_str();
}

//ResourceHandle getHandle(void) const
ResourceHandle resource_get_handle(const coiResourceHandle handle)
{
    const Ogre::Resource* res = static_cast<const Ogre::Resource*>(handle);
    return res->getHandle();
}

//bool isPrepared(void) const 
int resource_is_prepared(const coiResourceHandle handle)
{
    const Ogre::Resource* res = static_cast<const Ogre::Resource*>(handle); 
    return res->isPrepared();
}

//bool isLoaded(void) const 
int resource_is_loaded(const coiResourceHandle handle)
{
    const Ogre::Resource* res = static_cast<const Ogre::Resource*>(handle);
    return res->isLoaded();
}

//bool isLoading() const
int resource_is_loading(const coiResourceHandle handle)
{
    const Ogre::Resource* res = static_cast<const Ogre::Resource*>(handle);
    return res->isLoading();
}

//LoadingState getLoadingState() const
loading_state resource_get_loading_state(const coiResourceHandle handle)
{
    const Ogre::Resource* res = static_cast<const Ogre::Resource*>(handle);
    Ogre::Resource::LoadingState state = res->getLoadingState();
    return ogre_loading_state_to_llcoi(state);
}

//bool isBackgroundLoaded(void) const
int resource_is_background_loaded(const coiResourceHandle handle)
{
    const Ogre::Resource* res = static_cast<const Ogre::Resource*>(handle);
    return res->isBackgroundLoaded();
}

//void setBackgroundLoaded(bool bl)
void resource_set_background_loaded(coiResourceHandle handle, int bl)
{
    Ogre::Resource* res = static_cast<Ogre::Resource*>(handle);
    res->setBackgroundLoaded(bl);
}

//void escalateLoading()
void resource_escalate_loading(coiResourceHandle handle)
{
    Ogre::Resource* res = static_cast<Ogre::Resource*>(handle);
    res->escalateLoading();
}

//void addListener(Listener* lis)
void resource_add_listener(coiResourceHandle handle, ResourceListenerHandle listener)
{
    Ogre::Resource* res = static_cast<Ogre::Resource*>(handle);
    ResourceListener* l = static_cast<ResourceListener*>(listener);
    res->addListener(l);
}

//void removeListener(Listener* lis)
void resource_remove_listener(coiResourceHandle handle, ResourceListenerHandle listener)
{
    Ogre::Resource* res = static_cast<Ogre::Resource*>(handle);
    ResourceListener* l = static_cast<ResourceListener*>(listener);
    res->removeListener(l);
}

//const String& getGroup(void) const
const char* resource_get_group(const coiResourceHandle handle)
{
    const Ogre::Resource* res = static_cast<const Ogre::Resource*>(handle);
    return res->getGroup().c_str();
}

//void changeGroupOwnership(const String& newGroup)
void  resource_change_group_ownership(coiResourceHandle handle, const char* new_group)
{
    Ogre::Resource* res = static_cast<Ogre::Resource*>(handle);
    res->changeGroupOwnership(Ogre::String(new_group));
}

//ResourceManager* getCreator(void)
ResourceManagerHandle resource_get_creator(coiResourceHandle handle)
{
    Ogre::Resource* res = static_cast<Ogre::Resource*>(handle);
    Ogre::ResourceManager* manager = res->getCreator();
    return static_cast<ResourceManagerHandle>(manager);
}

//const String& getOrigin(void) const
const char* resource_get_origin(const coiResourceHandle handle)
{
    const Ogre::Resource* res = static_cast<const Ogre::Resource*>(handle);
    return res->getOrigin().c_str();
}

//void _notifyOrigin(const String& origin)
void resource__notify_origin(coiResourceHandle handle, const char* origin)
{
    Ogre::Resource* res = static_cast<Ogre::Resource*>(handle);
    res->_notifyOrigin(Ogre::String(origin));
}

//size_t getStateCount() const
size_t resource_get_state_count(const coiResourceHandle handle)
{
    const Ogre::Resource* res = static_cast<const Ogre::Resource*>(handle);
    return res->getStateCount();
}

//void _dirtyState()
void resource__dirty_state(coiResourceHandle handle)
{
    Ogre::Resource* res = static_cast<Ogre::Resource*>(handle);
    res->_dirtyState();
}

//void _fireLoadingComplete(bool wasBackgroundLoaded)
void resource__fire_loading_complete(coiResourceHandle handle, int was_background_loaded)
{
    Ogre::Resource* res = static_cast<Ogre::Resource*>(handle);
    res->_fireLoadingComplete(was_background_loaded);
}

//void _firePreparingComplete(bool wasBackgroundLoaded)
void resource__fire_preparing_complete(coiResourceHandle handle, int was_background_loaded)
{
    Ogre::Resource* res = static_cast<Ogre::Resource*>(handle);
    res->_firePreparingComplete(was_background_loaded);
}

//void _fireUnloadingComplete(void)
void resource__fire_unloading_complete(coiResourceHandle handle)
{
    Ogre::Resource* res = static_cast<Ogre::Resource*>(handle);
    res->_fireUnloadingComplete();
}

//Ogre::ManualResourceLoader

//~ManualResourceLoader()
void destroy_manualresourceloader(ManualResourceLoaderHandle handle)
{
    Ogre::ManualResourceLoader* loader = static_cast<Ogre::ManualResourceLoader*>(handle);
    delete loader;
}

//void prepareResource(Resource* resource)
void manualresourceloader_prepare_resource(ManualResourceLoaderHandle handle, coiResourceHandle resource)
{
    Ogre::ManualResourceLoader* loader = static_cast<Ogre::ManualResourceLoader*>(handle);
    Ogre::Resource* res = static_cast<Ogre::Resource*>(resource);
    loader->prepareResource(res);
}

//void loadResource(Resource* resource)
void manualresourceloader_load_resource(ManualResourceLoaderHandle handle, coiResourceHandle resource)
{
    Ogre::ManualResourceLoader* loader = static_cast<Ogre::ManualResourceLoader*>(handle);
    Ogre::Resource* res = static_cast<Ogre::Resource*>(resource);
    loader->loadResource(res);
}

