/******************************************************************************
 * renderwindow_bind.h - bindings for Ogre::RenderWindow
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
#ifndef RENDERWINDOW_BIND_H
#define RENDERWINDOW_BIND_H

#include "ogre_interface.h"

#define RenderWindowHandle void*
#define ViewportHandle void*
#define CameraHandle void*

// RenderWindow
DLL ViewportHandle render_window_add_viewport(RenderWindowHandle window_handle, CameraHandle camera_handle, int zorder, float left, float top, float width, float height);
DLL int render_window_is_closed(RenderWindowHandle handle);
DLL void render_window_set_active(RenderWindowHandle handle, int state);
DLL void render_window_swap_buffers(RenderWindowHandle handle, int wait_for_vsync);
DLL void render_window_get_custom_attribute(RenderWindowHandle handle, const char* attribute, void* pdata);
DLL unsigned int render_window_get_width(RenderWindowHandle handle);
DLL unsigned int render_window_get_height(RenderWindowHandle handle);
DLL void renderwindow_get_statistics(RenderWindowHandle handle, FrameStats* stats);
DLL void renderwindow_get_statistics_ex(RenderWindowHandle handle, float* lastFPS, float* avgFPS, float* bestFPS, float* worstFPS);

#endif
