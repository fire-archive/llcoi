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
#include "viewport_bind.h"

#include <OgreViewport.h>

void viewport_set_background_colour(ViewportHandle viewport_handle, float r, float g, float b, float a)
{
    Ogre::Viewport* vp = reinterpret_cast<Ogre::Viewport*>(viewport_handle);
    vp->setBackgroundColour(Ogre::ColourValue(r, g, b, a));
}

//Ogre::Viewport::getBackgroundColour() const
void viewport_get_background_colour(ViewportHandle handle, coiColourValue* cv)
{
    Ogre::Viewport* vp = reinterpret_cast<Ogre::Viewport*>(handle);
    const Ogre::ColourValue &c = vp->getBackgroundColour();

    cv->r = c.r;
    cv->b = c.b;
    cv->g = c.g;
    cv->a = c.a;
}

void viewport_set_background_colour_cv(ViewportHandle viewport_handle, coiColourValue* cv)
{
    Ogre::Viewport* vp = reinterpret_cast<Ogre::Viewport*>(viewport_handle);
    Ogre::ColourValue c(cv->r, cv->b, cv->g, cv->a);
    vp->setBackgroundColour(c);
}

float viewport_get_top(ViewportHandle handle)
{
    Ogre::Viewport* vp = reinterpret_cast<Ogre::Viewport*>(handle);
    return vp->getTop();
}

float viewport_get_left(ViewportHandle handle)
{
    Ogre::Viewport* vp = reinterpret_cast<Ogre::Viewport*>(handle);
    return vp->getLeft();
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



int viewport_get_actual_top(ViewportHandle handle)
{
    Ogre::Viewport* vp = reinterpret_cast<Ogre::Viewport*>(handle);
    return vp->getActualTop();
}

int viewport_get_actual_left(ViewportHandle handle)
{
    Ogre::Viewport* vp = reinterpret_cast<Ogre::Viewport*>(handle);
    return vp->getActualLeft();
}

int viewport_get_actual_width(ViewportHandle handle)
{
    Ogre::Viewport* vp = reinterpret_cast<Ogre::Viewport*>(handle);
    return vp->getActualWidth();
}

int viewport_get_actual_height(ViewportHandle handle)
{
    Ogre::Viewport* vp = reinterpret_cast<Ogre::Viewport*>(handle);
    return vp->getActualHeight();
}


//Ogre::Viewport::setAutoUpdated(bool)
void viewport_set_auto_updated(ViewportHandle handle, int autoupdate)
{
    Ogre::Viewport* vp = reinterpret_cast<Ogre::Viewport*>(handle);
    vp->setAutoUpdated(autoupdate);
}

//Ogre::Viewport::isAutoUpdated() const
int viewport_is_auto_updated(ViewportHandle handle)
{
    Ogre::Viewport* vp = reinterpret_cast<Ogre::Viewport*>(handle);
    return vp->isAutoUpdated();
}

//Ogre::Viewport::setDimensions(float, float, float, float)
void viewport_set_dimensions(ViewportHandle handle, coiReal left, coiReal top, coiReal width, coiReal height)
{
    Ogre::Viewport* vp = reinterpret_cast<Ogre::Viewport*>(handle);
    vp->setDimensions(left, top, width, height);
    
}

//Ogre::Viewport::getActualDimensions(int&, int&, int&, int&) const
void viewport_get_actual_dimensions(ViewportHandle handle, int* left, int* top, int* width, int* height)
{
    Ogre::Viewport* vp = reinterpret_cast<Ogre::Viewport*>(handle);
    vp->getActualDimensions(*left, *top, *width, *height);
}




/*
Ogre::Viewport::Listener
Ogre::Viewport::operator=(Ogre::Viewport const&)
Ogre::Viewport::Viewport(Ogre::Viewport const&)
Ogre::Viewport::Viewport(Ogre::Camera*, Ogre::RenderTarget*, float, float, float, float, int)
Ogre::Viewport::~Viewport()
Ogre::Viewport::_updateDimensions()
Ogre::Viewport::update()
Ogre::Viewport::clear(unsigned int, Ogre::ColourValue const&, float, unsigned short)
Ogre::Viewport::getTarget() const
Ogre::Viewport::getCamera() const
Ogre::Viewport::setCamera(Ogre::Camera*)
Ogre::Viewport::getZOrder() const
Ogre::Viewport::setOrientationMode(Ogre::OrientationMode, bool)
Ogre::Viewport::getOrientationMode() const
Ogre::Viewport::setDefaultOrientationMode(Ogre::OrientationMode)
Ogre::Viewport::getDefaultOrientationMode()
Ogre::Viewport::setBackgroundColour(Ogre::ColourValue const&)
Ogre::Viewport::setDepthClear(float)
Ogre::Viewport::getDepthClear() const
Ogre::Viewport::setClearEveryFrame(bool, unsigned int)
Ogre::Viewport::getClearEveryFrame() const
Ogre::Viewport::getClearBuffers() const
Ogre::Viewport::setMaterialScheme(std::string const&)
Ogre::Viewport::getMaterialScheme() const
Ogre::Viewport::_isUpdated() const
Ogre::Viewport::_clearUpdatedFlag()
Ogre::Viewport::_getNumRenderedFaces() const
Ogre::Viewport::_getNumRenderedBatches() const
Ogre::Viewport::setOverlaysEnabled(bool)
Ogre::Viewport::getOverlaysEnabled() const
Ogre::Viewport::setSkiesEnabled(bool)
Ogre::Viewport::getSkiesEnabled() const
Ogre::Viewport::setShadowsEnabled(bool)
Ogre::Viewport::getShadowsEnabled() const
Ogre::Viewport::setVisibilityMask(unsigned int)
Ogre::Viewport::getVisibilityMask() const
Ogre::Viewport::setRenderQueueInvocationSequenceName(std::string const&)
Ogre::Viewport::getRenderQueueInvocationSequenceName() const
Ogre::Viewport::_getRenderQueueInvocationSequence()
Ogre::Viewport::pointOrientedToScreen(Ogre::Vector2 const&, int, Ogre::Vector2&)
Ogre::Viewport::pointOrientedToScreen(float, float, int, float&, float&)
Ogre::Viewport::addListener(Ogre::Viewport::Listener*)
Ogre::Viewport::removeListener(Ogre::Viewport::Listener*)
Ogre::Viewport::Listener::operator=(Ogre::Viewport::Listener const&)
Ogre::Viewport::Listener::Listener(Ogre::Viewport::Listener const&)
Ogre::Viewport::Listener::Listener()
Ogre::Viewport::Listener::~Listener()
Ogre::Viewport::Listener::viewportCameraChanged(Ogre::Viewport*)
Ogre::Viewport::Listener::viewportDimensionsChanged(Ogre::Viewport*)
Ogre::Viewport::Listener::viewportDestroyed(Ogre::Viewport*)
*/
