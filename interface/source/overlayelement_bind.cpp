/******************************************************************************
 * overlayelement_bind.cpp  -  bindings for Ogre::OverlayElement
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

#include "overlayelement_bind.h"
#include "binding_utils.h"
#include <OgreOverlayElement.h>

void destroy_overlayelement(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    delete oe;
}

void overlayelement_initialise(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    oe->initialise();
}

const char* overlayelement_get_name(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    return oe->getName().c_str();
}

void overlayelement_show(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    oe->show();
}

void overlayelement_hide(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    oe->hide();
}

int overlayelement_is_visible(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    return oe->isVisible();
}

int overlayelement_is_enabled(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    return oe->isEnabled();
}

void overlayelement_set_enabled(OverlayElementHandle handle, int b)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    oe->setEnabled(b);
}

void overlayelement_set_dimensions(OverlayElementHandle handle, coiReal width, coiReal height)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    oe->setDimensions(width, height);
}

void overlayelement_set_position(OverlayElementHandle handle, coiReal left, coiReal top)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    oe->setPosition(left, top);
}

void overlayelement_set_width(OverlayElementHandle handle, coiReal width)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    oe->setWidth(width);
}

coiReal overlayelement_get_width(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    return oe->getWidth();
}

void overlayelement_set_height(OverlayElementHandle handle, coiReal height)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    oe->setHeight(height);
}

coiReal overlayelement_get_height(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    return oe->getHeight();
}

void overlayelement_set_left(OverlayElementHandle handle, coiReal left)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    oe->setLeft(left);
}

coiReal overlayelement_get_left(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    return oe->getLeft();
}

void overlayelement_set_top(OverlayElementHandle handle, coiReal top)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    oe->setTop(top);
}

coiReal overlayelement_get_top(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    return oe->getTop();
}

coiReal overlayelement__get_left(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    return oe->_getLeft();
}

coiReal overlayelement__get_top(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    return oe->_getTop();
}

coiReal overlayelement__get_width(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    return oe->_getWidth();
}

coiReal overlayelement__get_height(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    return oe->_getHeight();
}

void overlayelement__set_left(OverlayElementHandle handle, coiReal left)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    oe->_setLeft(left);
}

void overlayelement__set_top(OverlayElementHandle handle, coiReal top)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    oe->_setTop(top);
}

void overlayelement__set_width(OverlayElementHandle handle, coiReal width)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    oe->_setWidth(width);
}

void overlayelement__set_height(OverlayElementHandle handle, coiReal height)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    oe->_setHeight(height);
}

void overlayelement__set_position(OverlayElementHandle handle, coiReal left, coiReal top)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    oe->_setPosition(left, top);
}

void overlayelement__set_dimensions(OverlayElementHandle handle, coiReal width, coiReal height)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    oe->_setDimensions(width, height);
}

const char* overlayelement_get_material_name(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    return oe->getMaterialName().c_str();
}

void overlayelement_set_material_name(OverlayElementHandle handle, const char* name)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    oe->setMaterialName(Ogre::String(name));
}

void overlayelement_get_world_transforms(OverlayElementHandle handle, coiMatrix4* xform)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    Ogre::Matrix4 getter;
    oe->getWorldTransforms(&getter);
    ogre_matrix4_to_llcoi_matrix4(getter, *xform);
}

void overlayelement__positions_out_of_date(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    oe->_positionsOutOfDate();
}

void overlayelement__update(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    oe->_update();
}

void overlayelement__update_from_parent(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    oe->_updateFromParent();
}

coiReal overlayelement__get_derived_left(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    return oe->_getDerivedLeft();
}

coiReal overlayelement__get_derived_top(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    return oe->_getDerivedTop();
}

coiReal overlayelement__get_relative_width(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    return oe->_getRelativeWidth();
}

coiReal overlayelement__get_relative_height(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    return oe->_getRelativeHeight();
}

unsigned short overlayelement__notify_zorder(OverlayElementHandle handle, unsigned short new_zorder)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    return oe->_notifyZOrder(new_zorder);
}

void overlayelement__notify_world_transforms(OverlayElementHandle handle, const coiMatrix4* xform)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    Ogre::Matrix4 m;
    llcoi_matrix4_to_ogre_matrix4(*xform, m);
    oe->_notifyWorldTransforms(m);
}

void overlayelement__notify_viewport(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = reinterpret_cast<Ogre::OverlayElement*>(handle);
    oe->_notifyViewport();
}
