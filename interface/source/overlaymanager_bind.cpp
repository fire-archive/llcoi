/******************************************************************************
 * overlaymanager_bind.cpp  -  bindings for Ogre::OverlayManager
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

#include "overlaymanager_bind.h"
#include "binding_utils.h"
#include <OgreOverlayManager.h>

#include <iostream>
using std::cerr;

OverlayManagerHandle create_overlaymanager()
{
    Ogre::OverlayManager* ovm = new Ogre::OverlayManager;
    return reinterpret_cast<OverlayManagerHandle>(ovm);
}

void destroy_overlaymanager(OverlayManagerHandle handle)
{
    Ogre::OverlayManager* ovm = reinterpret_cast<Ogre::OverlayManager*>(handle);
    delete ovm;
}

coiReal overlaymanager_get_loading_order(OverlayManagerHandle handle)
{
    Ogre::OverlayManager* ovm = reinterpret_cast<Ogre::OverlayManager*>(handle);
    return ovm->getLoadingOrder();
}

OverlayHandle overlaymanager_create(OverlayManagerHandle handle, const char* name)
{
    cerr << "cast\n";
    Ogre::OverlayManager* ovm = reinterpret_cast<Ogre::OverlayManager*>(handle);
    cerr << "name\n";
    const Ogre::String n(name);
    cerr << "create\n";
    Ogre::Overlay* o = ovm->create(n);
    cerr << "return\n";
    return reinterpret_cast<OverlayHandle>(ovm->create(Ogre::String(name)));
}

OverlayHandle overlaymanager_get_by_name(OverlayManagerHandle handle, const char* name)
{
    Ogre::OverlayManager* ovm = reinterpret_cast<Ogre::OverlayManager*>(handle);
    return reinterpret_cast<OverlayHandle>(ovm->getByName(Ogre::String(name)));
}

void overlaymanager_destroy_by_name(OverlayManagerHandle handle, const char* name)
{
    Ogre::OverlayManager* ovm = reinterpret_cast<Ogre::OverlayManager*>(handle);
    ovm->destroy(Ogre::String(name));
}

void overlaymanager_destroy(OverlayManagerHandle handle, OverlayHandle overlay_handle)
{
    Ogre::OverlayManager* ovm = reinterpret_cast<Ogre::OverlayManager*>(handle);
    Ogre::Overlay* overlay = reinterpret_cast<Ogre::Overlay*>(overlay_handle);
    ovm->destroy(overlay);
}

void overlaymanager_destroy_all(OverlayManagerHandle handle)
{
    Ogre::OverlayManager* ovm = reinterpret_cast<Ogre::OverlayManager*>(handle);
    ovm->destroyAll();
}

int overlaymanager_has_viewport_changed(OverlayManagerHandle handle)
{
    Ogre::OverlayManager* ovm = reinterpret_cast<Ogre::OverlayManager*>(handle);
    return ovm->hasViewportChanged();
}

int overlaymanager_get_viewport_height(OverlayManagerHandle handle)
{
    Ogre::OverlayManager* ovm = reinterpret_cast<Ogre::OverlayManager*>(handle);
    return ovm->getViewportHeight();
}

int overlaymanager_get_viewport_width(OverlayManagerHandle handle)
{
    Ogre::OverlayManager* ovm = reinterpret_cast<Ogre::OverlayManager*>(handle);
    return ovm->getViewportWidth();
}

coiReal overlaymanager_get_viewport_aspect_ratio(OverlayManagerHandle handle)
{
    Ogre::OverlayManager* ovm = reinterpret_cast<Ogre::OverlayManager*>(handle);
    return ovm->getViewportAspectRatio();
}

orientation_mode overlaymanager_get_viewport_orientation_mode(OverlayManagerHandle handle)
{
    Ogre::OverlayManager* ovm = reinterpret_cast<Ogre::OverlayManager*>(handle);
    Ogre::OrientationMode mode = ovm->getViewportOrientationMode();
    return ogre_orientation_mode_to_llcoi_orientation_mode(mode);
}

int overlaymanager_has_overlay_element(OverlayManagerHandle handle, const char* name, int is_template)
{
    Ogre::OverlayManager* ovm = reinterpret_cast<Ogre::OverlayManager*>(handle);
    return ovm->hasOverlayElement(Ogre::String(name), is_template);
}

void overlaymanager_destroy_overlay_element(OverlayManagerHandle handle, const char* name, int is_template)
{
    Ogre::OverlayManager* ovm = reinterpret_cast<Ogre::OverlayManager*>(handle);
    ovm->destroyOverlayElement(Ogre::String(name), is_template);
}

void overlaymanager_destroy_all_overlay_elements(OverlayManagerHandle handle)
{
    Ogre::OverlayManager* ovm = reinterpret_cast<Ogre::OverlayManager*>(handle);
    ovm->destroyAllOverlayElements();
}

int overlaymanager_is_template(OverlayManagerHandle handle, const char* name)
{
    Ogre::OverlayManager* ovm = reinterpret_cast<Ogre::OverlayManager*>(handle);
    return ovm->isTemplate(Ogre::String(name));
}

OverlayManagerHandle overlaymanager_get_singleton_ptr()
{
    Ogre::OverlayManager* ovm = Ogre::OverlayManager::getSingletonPtr();
    return reinterpret_cast<OverlayManagerHandle>(
        ovm
    );
}

OverlayManagerHandle overlaymanager_get_singleton()
{
    const Ogre::OverlayManager& o = Ogre::OverlayManager::getSingleton();
    return reinterpret_cast<OverlayManagerHandle>(
        Ogre::OverlayManager::getSingletonPtr()
    );
}
