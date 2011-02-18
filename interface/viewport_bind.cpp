/******************************************************************************
 * viewport_bind.cpp - bindings for Ogre::Viewport
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
#include <OgreViewport.h>
#include <OgreRenderWindow.h>
#include <OgreCamera.h>

extern Ogre::RenderWindow* activeRenderWindow;

ViewportHandle add_viewport(CameraHandle camera_handle)
{
    Ogre::Camera* camera = reinterpret_cast<Ogre::Camera*>(camera_handle);
    Ogre::RenderWindow* window = activeRenderWindow;
    Ogre::Viewport* vp = window->addViewport(camera);
    return reinterpret_cast<ViewportHandle>(vp);
}

void viewport_set_background_colour(ViewportHandle viewport_handle, float r, float g, float b)
{
    Ogre::Viewport* vp = reinterpret_cast<Ogre::Viewport*>(viewport_handle);
    vp->setBackgroundColour(Ogre::ColourValue(r, g, b));
}

float viewport_get_width(ViewportHandle viewport_handle)
{
    Ogre::Viewport* vp = reinterpret_cast<Ogre::Viewport*>(viewport_handle);
    return vp->getWidth();
}

float viewport_get_height(ViewportHandle viewport_handle)
{
    Ogre::Viewport* vp = reinterpret_cast<Ogre::Viewport*>(viewport_handle);
    return vp->getHeight();
}

