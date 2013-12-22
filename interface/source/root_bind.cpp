/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "root_bind.h"

#include <OgreRoot.h>
#include <OgreRenderWindow.h>
#include <OgreWindowEventUtilities.h>
#include <OgreConfigFile.h>

RootHandle root_singleton()
{
  return reinterpret_cast<RootHandle>(Ogre::Root::getSingletonPtr());
}

RootHandle create_root(const char* pluginFileName, const char* configFileName, const char* logFileName)
{
  Ogre::Root* root = new Ogre::Root(Ogre::String(pluginFileName), Ogre::String(configFileName), Ogre::String(logFileName));
  return reinterpret_cast<RootHandle>(root);
}

void delete_root(RootHandle root_handle)
{
  Ogre::Root* root = reinterpret_cast<Ogre::Root*>(root_handle);
  delete root;
}

RenderWindowHandle root_initialise(RootHandle root_handle, int auto_create_window, const char* render_window_title)
{
  Ogre::Root* root = reinterpret_cast<Ogre::Root*>(root_handle);
  Ogre::RenderWindow* window = root->initialise(auto_create_window, render_window_title);
  return reinterpret_cast<RenderWindowHandle>(window);
}


RenderWindowHandle create_render_window(RootHandle root_handle, const char* name, const int width, const int height, const int full_screen, NameValuePairListHandle params)
{
  Ogre::Root* root = reinterpret_cast<Ogre::Root*>(root_handle);
    Ogre::RenderWindow* window = root->createRenderWindow(name, width, height, full_screen, reinterpret_cast<Ogre::NameValuePairList>(&params));
  return reinterpret_cast<RenderWindowHandle>(window);
}

RenderWindowHandle create_render_window_hwnd(RootHandle root_handle, const char* name, const int width, const int height, const int full_screen, unsigned long hwnd)
{
  Ogre::Root* root = reinterpret_cast<Ogre::Root*>(root_handle);
  Ogre::NameValuePairList misc;
  misc["parentWindowHandle"] = Ogre::StringConverter::toString(hwnd);
  Ogre::RenderWindow* window = root->createRenderWindow(name, width, height, full_screen, &misc);
  window->setActive(true);
  return reinterpret_cast<RenderWindowHandle>(window);
}

// Tell Ogre to use the current GL context.  This works on Linux/GLX but
// you *will* need something different on Windows or Mac.
RenderWindowHandle create_render_window_gl_context(RootHandle root_handle, const char* name, const int width, const int height, const int full_screen)
{
  Ogre::Root* root = reinterpret_cast<Ogre::Root*>(root_handle);
  Ogre::NameValuePairList misc;
  misc["currentGLContext"] = Ogre::String("True");
  Ogre::RenderWindow* window = root->createRenderWindow(name, width, height, full_screen, &misc);
  return reinterpret_cast<RenderWindowHandle>(window);
}

int root_is_initialised(RootHandle root_handle)
{
  Ogre::Root* root = reinterpret_cast<Ogre::Root*>(root_handle);
  if(root->isInitialised())
    return 1;
  return 0;
}

void save_config(RootHandle root_handle)
{
  Ogre::Root* root = reinterpret_cast<Ogre::Root*>(root_handle);
  root->saveConfig();
}

int restore_config(RootHandle root_handle)
{
  Ogre::Root* root = reinterpret_cast<Ogre::Root*>(root_handle);
  if(root->restoreConfig())
    return 1;
  return 0;
}

int show_config_dialog(RootHandle root_handle)
{
  Ogre::Root* root = reinterpret_cast<Ogre::Root*>(root_handle);
  if(root->showConfigDialog())
    return 1;
  return 0;
}

void add_render_system(RootHandle root_handle, RenderSystemHandle render_system)
{
  Ogre::Root* root = reinterpret_cast<Ogre::Root*>(root_handle);
  Ogre::RenderSystem* rs = reinterpret_cast<Ogre::RenderSystem*>(render_system);
  root->addRenderSystem(rs);
}

void set_render_system(RootHandle root_handle, RenderSystemHandle render_system)
{
  Ogre::Root* root = reinterpret_cast<Ogre::Root*>(root_handle);
  Ogre::RenderSystem* rs = reinterpret_cast<Ogre::RenderSystem*>(render_system);
  root->setRenderSystem(rs);
}

RenderSystemHandle get_render_system(RootHandle root_handle)
{
  Ogre::Root* root = reinterpret_cast<Ogre::Root*>(root_handle);
  return reinterpret_cast<RenderSystemHandle>(root->getRenderSystem());
}

RenderSystemHandle get_render_system_by_name(RootHandle root_handle, const char* render_system_name)
{
  Ogre::Root* root = reinterpret_cast<Ogre::Root*>(root_handle);
  Ogre::RenderSystem* rs = root->getRenderSystemByName(render_system_name);
  return reinterpret_cast<RenderSystemHandle>(rs);
}

void load_ogre_plugin(RootHandle root_handle, const char* plugin)
{
  Ogre::Root* root = reinterpret_cast<Ogre::Root*>(root_handle);
#if defined( WIN32 ) || defined( _WINDOWS )
  Ogre::String pluginString(plugin);
#ifdef _DEBUG
  root->loadPlugin(pluginString + Ogre::String("_d"));
#else
  root->loadPlugin(plugin);
#endif
#else
  root->loadPlugin( Ogre::String(plugin));
#endif
}		   

int render_one_frame(RootHandle root_handle)
{
  Ogre::Root* root = reinterpret_cast<Ogre::Root*>(root_handle);
  if(root->renderOneFrame())
    return 1;
  return 0;
}

int render_one_frame_ex(RootHandle root_handle, float time_since_last_frame)
{
  Ogre::Root* root = reinterpret_cast<Ogre::Root*>(root_handle);
  if(root->renderOneFrame(time_since_last_frame))
    return 1;
  return 0;
}

SceneManagerHandle create_scene_manager(RootHandle root_handle, const char* type_name, const char* instance_name)
{
  Ogre::Root* root = reinterpret_cast<Ogre::Root*>(root_handle);
  Ogre::SceneManager* sm = root->createSceneManager(Ogre::String(type_name), Ogre::String(instance_name));
  return reinterpret_cast<SceneManagerHandle>(sm);
}

SceneManagerHandle get_scene_manager_by_name(RootHandle root_handle, const char* scene_manager_instance_name)
{
  Ogre::Root* root = reinterpret_cast<Ogre::Root*>(root_handle);
  Ogre::SceneManager* sm = root->getSceneManager(scene_manager_instance_name);
  return reinterpret_cast<SceneManagerHandle>(sm);
}

// Ogre::Root::getAvailableRenderers
RenderSystemListHandle root_get_available_renderers(RootHandle root_handle)
{
  Ogre::Root* root = reinterpret_cast<Ogre::Root*>(root_handle);
  const Ogre::RenderSystemList& rslist = root->getAvailableRenderers();
  Ogre::RenderSystemList *l = new Ogre::RenderSystemList(rslist);
  return reinterpret_cast<RenderSystemListHandle>(l);
}


SceneManagerHandle root_create_scene_manager_by_mask(RootHandle root_handle, SceneTypeMask type_mask, const char* instance_name)
{
  Ogre::Root* root = reinterpret_cast<Ogre::Root*>(root_handle);
  Ogre::SceneManager* sm = root->createSceneManager(type_mask, Ogre::String(instance_name));
  return reinterpret_cast<SceneManagerHandle>(sm);
}

TimerHandle root_get_timer(RootHandle root_handle)
{
  Ogre::Root* root = reinterpret_cast<Ogre::Root*>(root_handle);
  Ogre::Timer* timer = root->getTimer();
  return reinterpret_cast<TimerHandle>(timer);
}

/*
  Ogre::Root::~Root()
  Ogre::Root::saveConfig()
  Ogre::Root::addRenderSystem(Ogre::RenderSystem*)
  Ogre::Root::getRenderSystemByName(std::string const&)
  Ogre::Root::setRenderSystem(Ogre::RenderSystem*)
  Ogre::Root::getRenderSystem()
  Ogre::Root::useCustomRenderSystemCapabilities(Ogre::RenderSystemCapabilities*)
  Ogre::Root::getRemoveRenderQueueStructuresOnClear() const
  Ogre::Root::setRemoveRenderQueueStructuresOnClear(bool)
  Ogre::Root::addSceneManagerFactory(Ogre::SceneManagerFactory*)
  Ogre::Root::removeSceneManagerFactory(Ogre::SceneManagerFactory*)
  Ogre::Root::getSceneManagerMetaData(std::string const&) const
  Ogre::Root::getSceneManagerMetaDataIterator() const
  Ogre::Root::createSceneManager(std::string const&, std::string const&)
  Ogre::Root::createSceneManager(unsigned short, std::string const&)
  Ogre::Root::destroySceneManager(Ogre::SceneManager*)
  Ogre::Root::getSceneManager(std::string const&) const
  Ogre::Root::hasSceneManager(std::string const&) const
  Ogre::Root::getSceneManagerIterator()
  Ogre::Root::getTextureManager()
  Ogre::Root::getMeshManager()
  Ogre::Root::getErrorDescription(long)
  Ogre::Root::addFrameListener(Ogre::FrameListener*)
  Ogre::Root::removeFrameListener(Ogre::FrameListener*)
  Ogre::Root::queueEndRendering()
  Ogre::Root::startRendering()
  Ogre::Root::shutdown()
  Ogre::Root::addResourceLocation(std::string const&, std::string const&, std::string const&, bool)
  Ogre::Root::removeResourceLocation(std::string const&, std::string const&)
  Ogre::Root::createFileStream(std::string const&, std::string const&, bool, std::string const&)
  Ogre::Root::openFileStream(std::string const&, std::string const&, std::string const&)
  Ogre::Root::convertColourValue(Ogre::ColourValue const&, unsigned int*)
  Ogre::Root::getAutoCreatedWindow()
  Ogre::Root::createRenderWindow(std::string const&, unsigned int, unsigned int, bool, std::map<std::string, std::string, std::less<std::string>, Ogre::STLAllocator<std::pair<std::string const, std::string>, Ogre::CategorisedAllocPolicy<(Ogre::MemoryCategory)0> > > const*)
  Ogre::Root::createRenderWindows(std::vector<Ogre::RenderWindowDescription, Ogre::STLAllocator<Ogre::RenderWindowDescription, Ogre::CategorisedAllocPolicy<(Ogre::MemoryCategory)0> > > const&, std::vector<Ogre::RenderWindow*, Ogre::STLAllocator<Ogre::RenderWindow*, Ogre::CategorisedAllocPolicy<(Ogre::MemoryCategory)0> > >&)
  Ogre::Root::detachRenderTarget(Ogre::RenderTarget*)
  Ogre::Root::detachRenderTarget(std::string const&)
  Ogre::Root::getRenderTarget(std::string const&)
  Ogre::Root::unloadPlugin(std::string const&)
  Ogre::Root::installPlugin(Ogre::Plugin*)
  Ogre::Root::uninstallPlugin(Ogre::Plugin*)
  Ogre::Root::getInstalledPlugins() const
  Ogre::Root::getTimer()
  Ogre::Root::_fireFrameStarted(Ogre::FrameEvent&)
  Ogre::Root::_fireFrameRenderingQueued(Ogre::FrameEvent&)
  Ogre::Root::_fireFrameEnded(Ogre::FrameEvent&)
  Ogre::Root::_fireFrameStarted()
  Ogre::Root::_fireFrameRenderingQueued()
  Ogre::Root::_fireFrameEnded()
  Ogre::Root::getNextFrameNumber() const
  Ogre::Root::_getCurrentSceneManager() const
  Ogre::Root::_pushCurrentSceneManager(Ogre::SceneManager*)
  Ogre::Root::_popCurrentSceneManager(Ogre::SceneManager*)
  Ogre::Root::_updateAllRenderTargets()
  Ogre::Root::_updateAllRenderTargets(Ogre::FrameEvent&)
  Ogre::Root::createRenderQueueInvocationSequence(std::string const&)
  Ogre::Root::getRenderQueueInvocationSequence(std::string const&)
  Ogre::Root::destroyRenderQueueInvocationSequence(std::string const&)
  Ogre::Root::destroyAllRenderQueueInvocationSequences()
  Ogre::Root::getSingleton()
  Ogre::Root::getSingletonPtr()
  Ogre::Root::clearEventTimes()
  Ogre::Root::setFrameSmoothingPeriod(float)
  Ogre::Root::getFrameSmoothingPeriod() const
  Ogre::Root::addMovableObjectFactory(Ogre::MovableObjectFactory*, bool)
  Ogre::Root::removeMovableObjectFactory(Ogre::MovableObjectFactory*)
  Ogre::Root::hasMovableObjectFactory(std::string const&) const
  Ogre::Root::getMovableObjectFactory(std::string const&)
  Ogre::Root::_allocateNextMovableObjectTypeFlag()
  Ogre::Root::getMovableObjectFactoryIterator() const
  Ogre::Root::getDisplayMonitorCount() const
  Ogre::Root::getWorkQueue() const
  Ogre::Root::setWorkQueue(Ogre::WorkQueue*)
  Ogre::Root::setBlendIndicesGpuRedundant(bool)
  Ogre::Root::isBlendIndicesGpuRedundant() const
  Ogre::Root::setBlendWeightsGpuRedundant(bool)
  Ogre::Root::isBlendWeightsGpuRedundant() const
*/
