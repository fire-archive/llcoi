/******************************************************************************
 * rendersystem_bind.h - bindings for Ogre::RenderSystem
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
#ifndef RENDERSYSTEM_BIND_H
#define RENDERSYSTEM_BIND_H

#include "ogre_interface.h"

#define RenderSystemHandle void*
#define RenderSystemListHandle void*


// RenderSystem functions
DLL void set_render_system(RenderSystemHandle render_system);
DLL void add_render_system(RenderSystemHandle render_system);
DLL RenderSystemHandle get_render_system();
DLL RenderSystemHandle get_render_system_by_name(const char* render_system_name);
DLL const char * render_system_get_name(RenderSystemHandle handle);
DLL void render_system_set_config_option(RenderSystemHandle render_system_handle, const char* option, const char* value);
DLL unsigned int render_system_list_size(RenderSystemListHandle list_handle);
DLL RenderSystemHandle render_system_list_get(RenderSystemListHandle list_handle, unsigned int index);
DLL void destroy_render_system_list(RenderSystemListHandle handle);

#endif
