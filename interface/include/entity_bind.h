/******************************************************************************
 * entity_bind.h - bindings for Ogre::Entity
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
#ifndef ENTITY_BIND_H
#define ENTITY_BIND_H
#include "ogre_interface.h"

#define EntityHandle void*
#define AxisAlignedBoxHandle void*

// Entity
DLL EntityHandle create_entity(const char* entity_name, const char* mesh_file);
DLL void entity_set_cast_shadows(EntityHandle handle, int enabled);
DLL int entity_get_cast_shadows(EntityHandle handle);
DLL int entity_get_receives_shadows(EntityHandle handle);
DLL void entity_set_material_name(EntityHandle handle, const char* material_name, const char* group_name);
//Ogre::Entity::getBoundingBox() const
DLL AxisAlignedBoxHandle entity_get_bounding_box(EntityHandle handle);
//Ogre::Entity::getBoundingRadius() const
DLL coiReal entity_get_bounding_radius(EntityHandle handle);
#endif
