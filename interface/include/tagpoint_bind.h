/******************************************************************************
 * tagpoint_bind.h - bindings for Ogre::TagPoint
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
#ifndef TAGPOINT_BIND_H
#define TAGPOINT_BIND_H

#include "ogre_interface.h"
#define TagPointHandle void*
#define SkeletonHandle void*
#define MovableObjectHandle void*
#define EntityHandle void*


//TagPoint(unsigned short handle, Skeleton* creator);
DLL TagPointHandle create_tagpoint(unsigned short bone_handle, SkeletonHandle creator);
//~TagPoint();
DLL void destroy_tagpoint(TagPointHandle handle);
//Entity *getParentEntity(void) const;
DLL EntityHandle tagpoint_get_parent_entity(const TagPointHandle handle);
//MovableObject* getChildObject(void) const;
DLL MovableObjectHandle tagpoint_get_child_object(const TagPointHandle handle);
//void setParentEntity(Entity *pEntity);
DLL void tagpoint_set_parent_entity(TagPointHandle handle, EntityHandle entity);
//void setChildObject(MovableObject *pObject);
DLL void tagpoint_set_child_object(TagPointHandle handle, MovableObjectHandle obj);
//void setInheritParentEntityOrientation(bool inherit);
DLL void tagpoint_set_inherit_parent_entity_orientation(TagPointHandle handle, int inherit);
//bool getInheritParentEntityOrientation(void) const;
DLL int tagpoint_get_inherit_parent_entity_orientation(const TagPointHandle handle);
//void setInheritParentEntityScale(bool inherit);
DLL void tagpoint_set_inherit_parent_entity_scale(TagPointHandle handle, int inherit);
//bool getInheritParentEntityScale(void) const;
DLL int tagpoint_get_inherit_parent_entity_scale(const TagPointHandle handle);
//const Matrix4& getParentEntityTransform(void) const;
DLL void tagpoint_get_parent_entity_transform(const TagPointHandle handle, coiMatrix4* result);
//const Matrix4& _getFullLocalTransform(void) const;
DLL void tagpoint__get_full_local_transform(const TagPointHandle handle, coiMatrix4* result);
//void needUpdate(bool forceParentUpdate = false);
DLL void tagpoint_need_update(TagPointHandle handle, int force_parent_update);
//void updateFromParentImpl(void) const;
DLL void tagpoint_update_from_parent_impl(const TagPointHandle handle);
//TODO: const LightList& getLights(void) const;

#endif
