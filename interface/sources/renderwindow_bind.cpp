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
 * See https://bitbucket.org/galaktor/llcoi for more information.
 *
 * Copyright (c) 2011-2012, Llcoi Team
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
#include "renderwindow_bind.h"

#include <OgreRoot.h>
#include <OgreRenderWindow.h>

ViewportHandle add_viewport(RenderWindowHandle window_handle, CameraHandle camera_handle)
{
  Ogre::RenderWindow* window = reinterpret_cast<Ogre::RenderWindow*>(window_handle);
  Ogre::Camera* camera = reinterpret_cast<Ogre::Camera*>(camera_handle);
  Ogre::Viewport* vp = window->addViewport(camera);
  return reinterpret_cast<ViewportHandle>(vp);
}

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


void render_window_resize(RenderWindowHandle window_handle, unsigned int width, unsigned int height)
{
  Ogre::RenderWindow* window = reinterpret_cast<Ogre::RenderWindow*>(window_handle);
  window->resize(width, height);
}

void render_window_moved_or_resized(RenderWindowHandle window_handle)
{
  Ogre::RenderWindow* window = reinterpret_cast<Ogre::RenderWindow*>(window_handle);
  window->windowMovedOrResized();
}

int render_window_closed(RenderWindowHandle window_handle)
{
  Ogre::RenderWindow* window = reinterpret_cast<Ogre::RenderWindow*>(window_handle);
  if(window->isClosed())
    return 1;
  return 0;
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
