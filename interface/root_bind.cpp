/******************************************************************************
 * root_bind.cpp - bindings for Ogre::Root
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

RenderWindowHandle create_render_window(RootHandle root_handle, const char* name, const int width, const int height, const int full_screen)
{
  Ogre::Root* root = reinterpret_cast<Ogre::Root*>(root_handle);
  Ogre::RenderWindow* window = root->createRenderWindow(name, width, height, full_screen);
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

SceneManagerHandle get_scene_manager_by_name(const char* scene_manager_instance_name)
{
  Ogre::SceneManager* sm = Ogre::Root::getSingletonPtr()->getSceneManager(scene_manager_instance_name);
  return reinterpret_cast<SceneManagerHandle>(sm);
}


// ******** convenience/boilerplate helpers ***********

/*
SceneManagerHandle get_scene_manager(RootHandle root_handle)
{
  Ogre::Root* root = reinterpret_cast<Ogre::Root*>(root_handle);
  Ogre::SceneManager* sm = Ogre::Root::getSingletonPtr()->getSceneManager(OgreManager::getSingletonPtr()->get_active_scene_manager_name());
  return reinterpret_cast<SceneManagerHandle>(sm);
}



  void default_engine_options(engine_options* options)
  {
  options->renderer_s = "OpenGL";
  #ifdef PLATFORM_WIN
  options->plugin_folder_s = ".";
  #else
  options->plugin_folder_s = "/usr/local/lib/OGRE";
  #endif
  options->window_title = "Renderwindow";
  options->width = 800;
  options->height = 600;
  options->auto_window = 1;
  options->log_name = "Ogre.log";
  }

  RootHandle init_engine(const engine_options options)
  {
  // suppress console logging
  Ogre::LogManager * log_man = new Ogre::LogManager();
  Ogre::Log * vge_log = log_man->createLog(options.log_name, true, false);
  Ogre::Root * root = new Ogre::Root("", "", "");

  // default
  const char * renderer = "OpenGL Rendering Subsystem";
  const char * render_plugin = "RenderSystem_GL";

  if (strstr(options.renderer_s,"Direct") || strstr(options.renderer_s,"D3D")) {
  renderer = "Direct3D9 Rendering Subsystem";
  render_plugin = "RenderSystem_Direct3D9";
  } else if (!strstr(options.renderer_s,"GL"))
  Ogre::LogManager::getSingleton().logMessage(
  "Can't parse renderer string, using default (OpenGL)");

  load_ogre_plugin(render_plugin);
  Ogre::RenderSystem* rs = root->getRenderSystemByName( Ogre::String(renderer) );
  rs->setConfigOption("Full Screen", "No");
  rs->setConfigOption("VSync", "No");
  rs->setConfigOption("Video Mode", Ogre::StringConverter::toString(options.width) + " x " +
  Ogre::StringConverter::toString(options.height) + " @ 32-bit");

  root->setRenderSystem(rs);

  load_ogre_plugin("Plugin_OctreeSceneManager");

  Ogre::SceneManager * scene_manager =
  root->createSceneManager(Ogre::ST_GENERIC, "scene-manager");

  root->initialise(options.auto_window, options.window_title)

  return reinterpret_cast<RootHandle>(root);
  }

  void release_engine(RootHandle root_handle)
  {
  Ogre::Root* r = reinterpret_cast<Ogre::Root*>(root_handle);
  delete r;
  }



  void current_window_update(int swap_buffers)
  {
  OgreManager::getSingletonPtr()->getActiveRenderWindow()->update(swap_buffers);
  }

*/

// this is not needed - apps typically have their own loop
/*
  static bool do_render = 1;
  void render_loop(RootHandle root_handle, RenderWindowHandle render_window_handle)
  {
  Ogre::Root* root = reinterpret_cast<Ogre::Root*>(root_handle);
  Ogre::Renderwindow* window = reinterpret_cast<Ogre::Renderwindow*>(render_window_handle);

  while (do_render)
  {
  // Pump window messages for nice behaviour
  pump_messages();

  // Render a frame
  if(!root->renderOneFrame())
  {
  do_render = 0;
  }

  if (window->isClosed())
  {
  do_render = 0;
  }
  }
  }
*/

/*
  Ogre::Root::~Root()
  Ogre::Root::saveConfig()
  Ogre::Root::addRenderSystem(Ogre::RenderSystem*)
  Ogre::Root::getAvailableRenderers()
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
