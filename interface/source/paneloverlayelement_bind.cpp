/******************************************************************************
 * paneloverlayelement_bind.cpp  -  bindings for Ogre::PanelOverlayElement
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

#include "paneloverlayelement_bind.h"
#include "binding_utils.h"
#include <OgrePanelOverlayElement.h>

//PanelOverlayElement(const String& name);
PanelOverlayElementHandle create_paneloverlayelement(const char* name)
{
    Ogre::PanelOverlayElement* poe = new Ogre::PanelOverlayElement(Ogre::String(name));
    return static_cast<PanelOverlayElementHandle>(poe);
}

//~PanelOverlayElement();
void destroy_paneloverlayelement(PanelOverlayElementHandle handle)
{
    Ogre::PanelOverlayElement* poe = static_cast<Ogre::PanelOverlayElement*>(handle);
    delete poe;
}

//void initialise(void);
void paneloverlayelement_initialise(PanelOverlayElementHandle handle)
{
    Ogre::PanelOverlayElement* poe = static_cast<Ogre::PanelOverlayElement*>(handle);
    poe->initialise();
}

//void setTiling(Real x, Real y, ushort layer = 0);
void paneloverlayelement_set_tiling(PanelOverlayElementHandle handle, coiReal x, coiReal y, unsigned short layer)
{
    Ogre::PanelOverlayElement* poe = static_cast<Ogre::PanelOverlayElement*>(handle);
    poe->setTiling(x,y,layer);
}

//Real getTileX(ushort layer = 0) const;
coiReal paneloverlayelement_get_tile_x(const PanelOverlayElementHandle handle, unsigned short layer)
{
    const Ogre::PanelOverlayElement* poe = static_cast<const Ogre::PanelOverlayElement*>(handle);
    return poe->getTileX(layer);
}

//Real getTileY(ushort layer = 0) const;
coiReal paneloverlayelement_get_tile_y(const PanelOverlayElementHandle handle, unsigned short layer)
{
    const Ogre::PanelOverlayElement* poe = static_cast<const Ogre::PanelOverlayElement*>(handle);
    return poe->getTileY(layer);
}

//void setUV(Real u1, Real v1, Real u2, Real v2);
void paneloverlayelement_set_uv(PanelOverlayElementHandle handle, coiReal u1, coiReal v1, coiReal u2, coiReal v2)
{
    Ogre::PanelOverlayElement* poe = static_cast<Ogre::PanelOverlayElement*>(handle);
    poe->setUV(u1, v1, u2, v2);
}

//void getUV(Real& u1, Real& v1, Real& u2, Real& v2) const;
void paneloverlayelement_get_uv(const PanelOverlayElementHandle handle, coiReal* u1, coiReal* v1, coiReal* u2, coiReal* v2)
{
    const Ogre::PanelOverlayElement* poe = static_cast<const Ogre::PanelOverlayElement*>(handle);
    poe->getUV(*u1, *v1, *u2, *v2);
}

//void setTransparent(bool isTransparent);
void paneloverlayelement_set_transparent(PanelOverlayElementHandle handle, int is_transparent)
{
    Ogre::PanelOverlayElement* poe = static_cast<Ogre::PanelOverlayElement*>(handle);
    poe->setTransparent(is_transparent);
}

//bool isTransparent(void) const;
int paneloverlayelement_is_transparent(const PanelOverlayElementHandle handle)
{
    const Ogre::PanelOverlayElement* poe = static_cast<const Ogre::PanelOverlayElement*>(handle);
    return poe->isTransparent();
}

//const String& getTypeName(void) const;
const char* paneloverlayelement_get_type_name(const PanelOverlayElementHandle handle)
{
    const Ogre::PanelOverlayElement* poe = static_cast<const Ogre::PanelOverlayElement*>(handle);
    return poe->getTypeName().c_str();
}

//TODO: void getRenderOperation(RenderOperation& op);
//void setMaterialName(const String& matName);
void paneloverlayelement_set_material_name(PanelOverlayElementHandle handle, const char* mat_name)
{
    Ogre::PanelOverlayElement* poe = static_cast<Ogre::PanelOverlayElement*>(handle);
    poe->setMaterialName(Ogre::String(mat_name));
}

//TODO: void _updateRenderQueue(RenderQueue* queue);

