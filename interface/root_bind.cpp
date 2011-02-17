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

extern const char* active_scene_manager_name;
extern const char * plugin_folder;

extern Ogre::RenderWindow* activeRenderWindow;
extern bool initialized;


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
    plugin_folder = options.plugin_folder_s;
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
        activeRenderWindow = root->initialise(true , options.window_title);
    } else {
        root->initialise(false , options.window_title);
    }
}

void release_engine()
{
    delete Ogre::Root::getSingletonPtr();
}

RenderWindowHandle root_initialise(int auto_create_window, const char* render_window_title)
{
    Ogre::RenderWindow* window = Ogre::Root::getSingletonPtr()->initialise(auto_create_window, render_window_title);
    if(auto_create_window)
        activeRenderWindow = window;
    return reinterpret_cast<RenderWindowHandle>(window);
}

DLL int root_is_initialised()
{
    if(Ogre::Root::getSingletonPtr()->isInitialised())
        return 1;
    return 0;
}

RootHandle create_root(const char* pluginFileName, const char* configFileName, const char* logFileName)
{
    Ogre::Root* root = new Ogre::Root(Ogre::String(pluginFileName), Ogre::String(configFileName), Ogre::String(logFileName));
    return reinterpret_cast<RootHandle>(root);
}

void save_config()
{
    Ogre::Root::getSingletonPtr()->saveConfig();
}

int restore_config()
{
    if(Ogre::Root::getSingletonPtr()->restoreConfig())
        return 1;
    return 0;
}

int show_config_dialog()
{
    if(Ogre::Root::getSingletonPtr()->showConfigDialog())
        return 1;
    return 0;
}

void add_render_system(RenderSystemHandle render_system)
{
    Ogre::RenderSystem* rs = reinterpret_cast<Ogre::RenderSystem*>(render_system);
    Ogre::Root::getSingletonPtr()->addRenderSystem(rs);
}

void set_render_system(RenderSystemHandle render_system)
{
    Ogre::RenderSystem* rs = reinterpret_cast<Ogre::RenderSystem*>(render_system);
    Ogre::Root::getSingletonPtr()->setRenderSystem(rs);
}

RenderSystemHandle get_render_system_by_name(const char* render_system_name)
{
    Ogre::RenderSystem* rs = Ogre::Root::getSingletonPtr()->getRenderSystemByName(render_system_name);
    return reinterpret_cast<RenderSystemHandle>(rs);
}

RenderSystemHandle get_render_system()
{
    return reinterpret_cast<RenderSystemHandle>(Ogre::Root::getSingletonPtr()->getRenderSystem());
}

void render_system_set_config_option(RenderSystemHandle render_system_handle, const char* option, const char* value)
{
    Ogre::RenderSystem* rs = reinterpret_cast<Ogre::RenderSystem*>(render_system_handle);
    rs->setConfigOption(option, value);
}

RenderWindowHandle create_render_window(const char* name, const int width, const int height, const int full_screen)
{
    Ogre::RenderWindow* window = Ogre::Root::getSingletonPtr()->createRenderWindow(name, width, height, full_screen);
    activeRenderWindow = window;
    return reinterpret_cast<RenderWindowHandle>(window);
}

RenderWindowHandle create_render_window_ex(const char* name, const int width, const int height, const int full_screen, const char* misc_param, const char* misc_value)
{
    Ogre::NameValuePairList misc;
    // Tell Ogre to use the current GL context.  This works on Linux/GLX but
    // you *will* need something different on Windows or Mac.
    misc["currentGLContext"] = Ogre::String("True");
    Ogre::RenderWindow* window = Ogre::Root::getSingletonPtr()->createRenderWindow(name, width, height, full_screen, &misc);
    activeRenderWindow = window;
    return reinterpret_cast<RenderWindowHandle>(window);
}

SceneManagerHandle create_scene_manager(const char* type_name, const char* instance_name)
{
/*    va_list arg_list;
    const char* instance_name = NULL;
    va_start(arg_list, type_name);
    instance_name = va_arg(arg_list, const char*);
    va_end(arg_list);

    if(instance_name == NULL) instance_name = "default";*/
    
    active_scene_manager_name = instance_name;
    
    Ogre::SceneManager* sm = Ogre::Root::getSingletonPtr()->createSceneManager(Ogre::String(type_name), Ogre::String(instance_name));
    return reinterpret_cast<SceneManagerHandle>(sm);
}

SceneManagerHandle get_scene_manager()
{
    Ogre::SceneManager* sm = Ogre::Root::getSingletonPtr()->getSceneManager(active_scene_manager_name);
    return reinterpret_cast<SceneManagerHandle>(sm);
}

SceneManagerHandle get_scene_manager_by_name(const char* scene_manager_instance_name)
{
    Ogre::SceneManager* sm = Ogre::Root::getSingletonPtr()->getSceneManager(scene_manager_instance_name);
    return reinterpret_cast<SceneManagerHandle>(sm);
}

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
    Ogre::Root::getSingleton().loadPlugin( Ogre::String(plugin_folder) + "/" + plugin );
#endif
}

int render_one_frame()
{
    if(Ogre::Root::getSingletonPtr()->renderOneFrame())
        return 1;
    return 0;
}

int render_one_frame_custom(float time_since_last_frame)
{
    if(Ogre::Root::getSingletonPtr()->renderOneFrame(time_since_last_frame))
        return 1;
    return 0;
}

static bool do_render = 1;

void render_loop()
{
    while (do_render)
    {
        // Pump window messages for nice behaviour
        Ogre::WindowEventUtilities::messagePump();

        // Render a frame
        Ogre::Root::getSingletonPtr()->renderOneFrame();

        if (Ogre::Root::getSingletonPtr()->getAutoCreatedWindow()->isClosed())
        {
            do_render = 0;
        }
    }
}
