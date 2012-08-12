/******************************************************************************
 * root_bind.h - bindings for Ogre::Root
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
#ifndef ROOT_BIND_H
#define ROOT_BIND_H

#include "ogre_interface.h"

// Ghetto forward declarations :D
#define RootHandle void*
#define SceneManagerHandle void*
#define TimerHandle void*
#define RenderWindowHandle void*
#define RenderSystemListHandle void*
#define RenderSystemHandle void*

typedef struct
{
    const char* renderer_s;
    const char* plugin_folder_s;
    const char* window_title;
    const char* log_name;
    int width, height, auto_window;
} engine_options;

// Root functions
DLL RootHandle create_root(const char* pluginFileName, const char* configFileName, const char* logFileName);
DLL void release_engine();
DLL void default_engine_options(engine_options* options);
DLL void init_engine(const engine_options options);
DLL RenderWindowHandle root_initialise(int auto_create_window, const char* render_window_title);
DLL TimerHandle root_get_timer();
DLL RenderWindowHandle create_render_window(const char* name, const int width, const int height, const int full_screen);
DLL RenderWindowHandle create_render_window_gl_context(const char* name, const int width, const int height, const int full_screen);
DLL RenderWindowHandle create_render_window_hwnd(const char* name, const int width, const int height, const int full_screen, unsigned long hwnd);
DLL unsigned int render_window_get_hwnd(RenderWindowHandle window_handle);
DLL void render_window_set_visible(RenderWindowHandle window_handle, int visible);
DLL void render_window_update(RenderWindowHandle window_handle, int swap_buffers);
DLL void current_window_update(int swap_buffers);
DLL void render_window_resize(unsigned int width, unsigned int height);
DLL void render_window_moved_or_resized();
DLL int render_window_closed();
DLL int root_is_initialised();
DLL void save_config();
DLL int restore_config();
DLL int show_config_dialog();
DLL void load_ogre_plugin(const char * plugin);
// Doesn't use OgreManager. Can still throw if type_name doesn't exist.
DLL SceneManagerHandle root_create_scene_manager(const char* type_name, const char* instance_name);
// Doesn't use OgreManager. If a specific scene manager is not found,
// the default implementation is always returned.
DLL SceneManagerHandle root_create_scene_manager_by_mask(SceneTypeMask type_mask, const char* instance_name);
// Does use OgreManager.
DLL SceneManagerHandle create_scene_manager(const char* type_name, const char* instance_name);
DLL SceneManagerHandle get_scene_manager();
DLL SceneManagerHandle get_scene_manager_by_name(const char* scene_manager_instance_name);
DLL int render_one_frame();
DLL int render_one_frame_ex(float time_since_last_frame);
DLL void render_loop();
DLL void pump_messages();
DLL void log_message(const char* message);
DLL RenderWindowHandle root_create_render_window(const char* name, unsigned int width, unsigned int height, int fullscreen, NameValuePairListHandle params);
DLL RenderSystemListHandle root_get_available_renderers();

#endif
