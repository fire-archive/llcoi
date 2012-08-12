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
#include "renderwindow_bind.h"

#include <OgreRoot.h>
#include <OgreRenderWindow.h>

#include "ogre_manager.h"

void render_window_set_visible(RenderWindowHandle window_handle, int visible)
{
    Ogre::RenderWindow* window = reinterpret_cast<Ogre::RenderWindow*>(window_handle);
    window->setActive(true);
    window->setVisible(visible);
}

unsigned int render_window_get_hwnd(RenderWindowHandle window_handle)
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

// Rendertarget->addViewport 
ViewportHandle render_window_add_viewport(RenderWindowHandle window_handle, CameraHandle camera_handle, int zorder, float left, float top, float width, float height)
{
    Ogre::RenderWindow* window = reinterpret_cast<Ogre::RenderWindow*>(window_handle);
    Ogre::Camera* camera = reinterpret_cast<Ogre::Camera*>(camera_handle);
    return reinterpret_cast<ViewportHandle>(window->addViewport(camera, zorder, left, top, width, height));
}

// RenderWindow::isClosed
int render_window_is_closed(RenderWindowHandle handle)
{
    Ogre::RenderWindow* window = reinterpret_cast<Ogre::RenderWindow*>(handle);
    if (window->isClosed())
        return 1;
    return 0;
}

void render_window_set_active(RenderWindowHandle handle, int state)
{
    Ogre::RenderWindow* window = reinterpret_cast<Ogre::RenderWindow*>(handle);
    window->setActive(state);
}

void render_window_swap_buffers(RenderWindowHandle handle, int wait_for_vsync)
{
    Ogre::RenderWindow* window = reinterpret_cast<Ogre::RenderWindow*>(handle);
    window->swapBuffers(wait_for_vsync);
}

void render_window_get_custom_attribute(RenderWindowHandle handle, const char* attribute, void* pdata)
{
    Ogre::RenderWindow* window = reinterpret_cast<Ogre::RenderWindow*>(handle);
    window->getCustomAttribute(attribute, pdata);
}

unsigned int render_window_get_width(RenderWindowHandle handle)
{
    Ogre::RenderWindow* window = reinterpret_cast<Ogre::RenderWindow*>(handle);
    return window->getWidth();
}

unsigned int render_window_get_height(RenderWindowHandle handle)
{
    Ogre::RenderWindow* window = reinterpret_cast<Ogre::RenderWindow*>(handle);
    return window->getHeight();
}

void renderwindow_get_statistics(RenderWindowHandle handle, FrameStats* stats)
{
    Ogre::RenderWindow* window = reinterpret_cast<Ogre::RenderWindow*>(handle);
    const Ogre::RenderTarget::FrameStats& fs = window->getStatistics();

    stats->lastFPS         = fs.lastFPS;
    stats->avgFPS          = fs.avgFPS;
    stats->bestFPS         = fs.bestFPS;
    stats->worstFPS        = fs.worstFPS;
    stats->bestFrameTime   = fs.bestFrameTime;
    stats->worstFrameTime  = fs.worstFrameTime;
    stats->triangleCount   = fs.triangleCount;
    stats->batchCount      = fs.batchCount;
}

void renderwindow_get_statistics_ex(RenderWindowHandle handle, float* lastFPS, float* avgFPS, float* bestFPS, float* worstFPS)
{
    Ogre::RenderWindow* window = reinterpret_cast<Ogre::RenderWindow*>(handle);
    window->getStatistics(*lastFPS, *avgFPS, *bestFPS, *worstFPS);
}

/*
Ogre::RenderWindow::operator=(Ogre::RenderWindow const&)
Ogre::RenderWindow::RenderWindow(Ogre::RenderWindow const&)
Ogre::RenderWindow::RenderWindow()
Ogre::RenderWindow::create(std::string const&, unsigned int, unsigned int, bool, std::map<std::string, std::string, std::less<std::string>, Ogre::STLAllocator<std::pair<std::string const, std::string>, Ogre::CategorisedAllocPolicy<(Ogre::MemoryCategory)0> > > const*)
Ogre::RenderWindow::setFullscreen(bool, unsigned int, unsigned int)
Ogre::RenderWindow::destroy()
Ogre::RenderWindow::resize(unsigned int, unsigned int)
Ogre::RenderWindow::windowMovedOrResized()
Ogre::RenderWindow::reposition(int, int)
Ogre::RenderWindow::isVisible() const
Ogre::RenderWindow::setVisible(bool)
Ogre::RenderWindow::isHidden() const
Ogre::RenderWindow::setHidden(bool)
Ogre::RenderWindow::setVSyncEnabled(bool)
Ogre::RenderWindow::isVSyncEnabled() const
Ogre::RenderWindow::setVSyncInterval(unsigned int)
Ogre::RenderWindow::getVSyncInterval() const
Ogre::RenderWindow::isActive() const
Ogre::RenderWindow::isClosed() const
Ogre::RenderWindow::isPrimary() const
Ogre::RenderWindow::isFullScreen() const
Ogre::RenderWindow::getMetrics(unsigned int&, unsigned int&, unsigned int&, int&, int&)
Ogre::RenderWindow::suggestPixelFormat() const
Ogre::RenderWindow::isDeactivatedOnFocusChange() const
Ogre::RenderWindow::setDeactivateOnFocusChange(bool)
Ogre::RenderWindow::~RenderWindow()
*/
