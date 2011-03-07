/******************************************************************************
 * renderwindow_bind.cpp - bindings for Ogre::RenderWindow
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

#include "ogre_manager.h"

void render_window_set_visible(RenderWindowHandle window_handle, int visible)
{
    Ogre::RenderWindow* window = reinterpret_cast<Ogre::RenderWindow*>(window_handle);
    window->setActive(true);
    window->setVisible(visible);
}

size_t render_window_get_hwnd(RenderWindowHandle window_handle)
{
	size_t windowHnd = 0;
	Ogre::RenderWindow* window = reinterpret_cast<Ogre::RenderWindow*>(window_handle);
	window->getCustomAttribute("WINDOW", &windowHnd);
	return windowHnd;
}

void render_window_update(RenderWindowHandle window_handle, int swap_buffers)
{
    Ogre::RenderWindow* window = reinterpret_cast<Ogre::RenderWindow*>(window_handle);
    window->update(swap_buffers);
}

void current_window_update(int swap_buffers)
{
    OgreManager::getSingletonPtr()->getActiveRenderWindow()->update(swap_buffers);
}

RenderWindowHandle create_render_window(const char* name, const int width, const int height, const int full_screen)
{
    Ogre::RenderWindow* window = Ogre::Root::getSingletonPtr()->createRenderWindow(name, width, height, full_screen);
    OgreManager::getSingletonPtr()->setActiveRenderWindow(window);
    return reinterpret_cast<RenderWindowHandle>(window);
}

DLL RenderWindowHandle create_render_window_hwnd(const char* name, const int width, const int height, const int full_screen, unsigned long hwnd)
{
    Ogre::NameValuePairList misc;
    misc["parentWindowHandle"] = Ogre::StringConverter::toString(hwnd);
    Ogre::RenderWindow* window = Ogre::Root::getSingletonPtr()->createRenderWindow(name, width, height, full_screen, &misc);
    OgreManager::getSingletonPtr()->setActiveRenderWindow(window);
	window->setActive(true);
    return reinterpret_cast<RenderWindowHandle>(window);
}

RenderWindowHandle create_render_window_gl_context(const char* name, const int width, const int height, const int full_screen)
{
    Ogre::NameValuePairList misc;
    // Tell Ogre to use the current GL context.  This works on Linux/GLX but
    // you *will* need something different on Windows or Mac.
    misc["currentGLContext"] = Ogre::String("True");
    Ogre::RenderWindow* window = Ogre::Root::getSingletonPtr()->createRenderWindow(name, width, height, full_screen, &misc);
    OgreManager::getSingletonPtr()->setActiveRenderWindow(window);
    return reinterpret_cast<RenderWindowHandle>(window);
}

void render_window_resize(unsigned int width, unsigned int height)
{
	OgreManager::getSingletonPtr()->getActiveRenderWindow()->resize(width, height);
}

void render_window_moved_or_resized()
{
	OgreManager::getSingletonPtr()->getActiveRenderWindow()->windowMovedOrResized();
}

int render_window_closed()
{
    if(OgreManager::getSingletonPtr()->getActiveRenderWindow()->isClosed())
        return 1;
    return 0;
}
