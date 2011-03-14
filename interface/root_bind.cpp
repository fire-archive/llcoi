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
#include "ogre_interface.h"

#include <OgreRoot.h>
#include <OgreRenderWindow.h>
#include <OgreWindowEventUtilities.h>
#include <OgreConfigFile.h>
#include "ogre_manager.h"

template<> OgreManager* Ogre::Singleton<OgreManager>::ms_Singleton = 0;

void load_ogre_plugin(const char* plugin);

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

void init_engine(const engine_options options)
{
    new OgreManager();
    
    OgreManager::getSingletonPtr()->set_plugin_folder(options.plugin_folder_s);
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
    //rs->setConfigOption("Video Mode", "800 x 600 @ 32-bit");
    rs->setConfigOption("Video Mode", Ogre::StringConverter::toString(options.width) + " x " +
                        Ogre::StringConverter::toString(options.height) + " @ 32-bit");

    root->setRenderSystem(rs);

    load_ogre_plugin("Plugin_OctreeSceneManager");

    Ogre::SceneManager * scene_manager =
        root->createSceneManager(Ogre::ST_GENERIC, "scene-manager");

    if (options.auto_window) {
        OgreManager::getSingletonPtr()->setActiveRenderWindow(root->initialise(true , options.window_title));
    } else {
        root->initialise(false , options.window_title);
    }
}

void release_engine()
{
    delete Ogre::Root::getSingletonPtr();
}

// Ogre::Root::initialise(bool, std::string const&, std::string const&)
RenderWindowHandle root_initialise(int auto_create_window, const char* render_window_title)
{
    Ogre::RenderWindow* window = Ogre::Root::getSingletonPtr()->initialise(auto_create_window, render_window_title);
    if(auto_create_window)
        OgreManager::getSingletonPtr()->setActiveRenderWindow(window);
    return reinterpret_cast<RenderWindowHandle>(window);
}

// Ogre::Root::isInitialised() const
DLL int root_is_initialised()
{
    if(Ogre::Root::getSingletonPtr()->isInitialised())
        return 1;
    return 0;
}

// Ogre::Root::Root(std::string const&, std::string const&, std::string const&)
RootHandle create_root(const char* pluginFileName, const char* configFileName, const char* logFileName)
{
    new OgreManager();
    Ogre::Root* root = new Ogre::Root(Ogre::String(pluginFileName), Ogre::String(configFileName), Ogre::String(logFileName));
    return reinterpret_cast<RootHandle>(root);
}

void save_config()
{
    Ogre::Root::getSingletonPtr()->saveConfig();
}

// Ogre::Root::restoreConfig()
int restore_config()
{
    if(Ogre::Root::getSingletonPtr()->restoreConfig())
        return 1;
    return 0;
}

// Ogre::Root::showConfigDialog()
int show_config_dialog()
{
    if(Ogre::Root::getSingletonPtr()->showConfigDialog())
        return 1;
    return 0;
}

// Ogre::Root::loadPlugin(std::string const&)
void load_ogre_plugin(const char* plugin)
{
#if defined( WIN32 ) || defined( _WINDOWS )
    Ogre::String pluginString(plugin);
#ifdef _DEBUG
    Ogre::Root::getSingleton().loadPlugin(pluginString + Ogre::String("_d"));
#else
    Ogre::Root::getSingleton().loadPlugin(plugin);
#endif
#else
    Ogre::Root::getSingleton().loadPlugin( Ogre::String(OgreManager::getSingletonPtr()->get_plugin_folder()) + "/" + plugin );
#endif
}

// Ogre::Root::renderOneFrame()
int render_one_frame()
{
    if(Ogre::Root::getSingletonPtr()->renderOneFrame())
        return 1;
    return 0;
}

// Ogre::Root::renderOneFrame(float)
int render_one_frame_ex(float time_since_last_frame)
{
    if(Ogre::Root::getSingletonPtr()->renderOneFrame(time_since_last_frame))
        return 1;
    return 0;
}

void pump_messages()
{
	Ogre::WindowEventUtilities::messagePump();
}

static bool do_render = 1;

void render_loop()
{
    while (do_render)
    {
        // Pump window messages for nice behaviour
        Ogre::WindowEventUtilities::messagePump();

        // Render a frame
        if(!Ogre::Root::getSingletonPtr()->renderOneFrame())
        {
            do_render = 0;
        }

        if (OgreManager::getSingletonPtr()->getActiveRenderWindow()->isClosed())
        {
            do_render = 0;
        }
    }
}

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
