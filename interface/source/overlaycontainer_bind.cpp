/******************************************************************************
 * overlaycontainer_bind.cpp  -  bindings for Ogre::OverlayContainer
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

#include "overlaycontainer_bind.h"
#include <OgreOverlayContainer.h>

void destroy_overlaycontainer(OverlayContainerHandle handle)
{
    Ogre::OverlayContainer* oc = reinterpret_cast<Ogre::OverlayContainer*>(handle);
    delete oc;
}

void overlaycontainer_add_child(OverlayContainerHandle handle, OverlayElementHandle child_handle)
{
    Ogre::OverlayContainer* oc = reinterpret_cast<Ogre::OverlayContainer*>(handle);
    Ogre::OverlayElement* oe   = reinterpret_cast<Ogre::OverlayElement*>(child_handle);
    oc->addChild(oe);
}

void overlaycontainer_add_child_impl(OverlayContainerHandle handle, OverlayElementHandle child_handle)
{
    Ogre::OverlayContainer* oc = reinterpret_cast<Ogre::OverlayContainer*>(handle);
    Ogre::OverlayElement* oe   = reinterpret_cast<Ogre::OverlayElement*>(child_handle);
    oc->addChildImpl(oe);
}

void overlaycontainer_add_child_container_impl(OverlayContainerHandle handle, OverlayContainerHandle child_handle)
{
    Ogre::OverlayContainer* oc = reinterpret_cast<Ogre::OverlayContainer*>(handle);
    Ogre::OverlayContainer* child = reinterpret_cast<Ogre::OverlayContainer*>(child_handle);
    oc->addChildImpl(child);
}

void overlaycontainer_remove_child(OverlayContainerHandle handle, const char* name)
{
    Ogre::OverlayContainer* oc = reinterpret_cast<Ogre::OverlayContainer*>(handle);
    oc->removeChild(Ogre::String(name));
}

OverlayElementHandle overlaycontainer_get_child(OverlayContainerHandle handle, const char* name)
{
    Ogre::OverlayContainer* oc = reinterpret_cast<Ogre::OverlayContainer*>(handle);
    Ogre::OverlayElement* oe = oc->getChild(Ogre::String(name));
    return reinterpret_cast<OverlayElementHandle>(oe);
}

void overlaycontainer_initialise(OverlayContainerHandle handle)
{
    Ogre::OverlayContainer* oc = reinterpret_cast<Ogre::OverlayContainer*>(handle);
    oc->initialise();
}

void overlaycontainer__add_child(OverlayContainerHandle handle, OverlayElementHandle elem)
{
    Ogre::OverlayContainer* oc = reinterpret_cast<Ogre::OverlayContainer*>(handle);
    Ogre::OverlayElement* oe   = reinterpret_cast<Ogre::OverlayElement*>(elem);
    oc->_addChild(oe);
}

void overlaycontainer__remove_child(OverlayContainerHandle handle, OverlayElementHandle elem)
{
    Ogre::OverlayContainer* oc = reinterpret_cast<Ogre::OverlayContainer*>(handle);
    Ogre::OverlayElement* oe   = reinterpret_cast<Ogre::OverlayElement*>(elem);
    oc->_removeChild(oe);
}

void overlaycontainer__remove_child_by_name(OverlayContainerHandle handle, const char* name)
{
    Ogre::OverlayContainer* oc = reinterpret_cast<Ogre::OverlayContainer*>(handle);
    oc->_removeChild(Ogre::String(name));
}
