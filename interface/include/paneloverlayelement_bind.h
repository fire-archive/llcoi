/******************************************************************************
 * paneloverlayelement_bind.h -  bindings for Ogre::PanelOverlayElement
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
#ifndef PANELOVERLAYELEMENT_BIND_H
#define PANELOVERLAYELEMENT_BIND_H
#include "ogre_interface.h"

#define PanelOverlayElementHandle void*

//PanelOverlayElement(const String& name);
DLL PanelOverlayElementHandle create_paneloverlayelement(const char* name);
//~PanelOverlayElement();
DLL void destroy_paneloverlayelement(PanelOverlayElementHandle handle);
//void initialise(void);
DLL void paneloverlayelement_initialise(PanelOverlayElementHandle handle);
//void setTiling(Real x, Real y, ushort layer = 0);
DLL void paneloverlayelement_set_tiling(PanelOverlayElementHandle handle, coiReal x, coiReal y, unsigned short layer);
//Real getTileX(ushort layer = 0) const;
DLL coiReal paneloverlayelement_get_tile_x(const PanelOverlayElementHandle handle, unsigned short layer);
//Real getTileY(ushort layer = 0) const;
DLL coiReal paneloverlayelement_get_tile_y(const PanelOverlayElementHandle handle, unsigned short layer);
//void setUV(Real u1, Real v1, Real u2, Real v2);
DLL void paneloverlayelement_set_uv(PanelOverlayElementHandle handle, coiReal u1, coiReal v1, coiReal u2, coiReal v2);
//void getUV(Real& u1, Real& v1, Real& u2, Real& v2) const;
DLL void paneloverlayelement_get_uv(const PanelOverlayElementHandle handle, coiReal* u1, coiReal* v1, coiReal* u2, coiReal* v2);
//void setTransparent(bool isTransparent);
DLL void paneloverlayelement_set_transparent(PanelOverlayElementHandle handle, int is_transparent);
//bool isTransparent(void) const;
DLL int paneloverlayelement_is_transparent(const PanelOverlayElementHandle handle);
//const String& getTypeName(void) const;
DLL const char* paneloverlayelement_get_type_name(const PanelOverlayElementHandle handle);
//TODO: void getRenderOperation(RenderOperation& op);
//void setMaterialName(const String& matName);
DLL void paneloverlayelement_set_material_name(PanelOverlayElementHandle handle, const char* mat_name);
//TODO: void _updateRenderQueue(RenderQueue* queue);


#endif
