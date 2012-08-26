/******************************************************************************
 * tagpoint_bind.cpp - bindings for Ogre::TagPoint
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
#include "tagpoint_bind.h"
#include "binding_utils.h"

#include <OgreTagPoint.h>
#include <OgreBone.h>
#include <OgreSkeleton.h>



//TagPoint(unsigned short handle, Skeleton* creator);
TagPointHandle create_tagpoint(unsigned short bone_handle, SkeletonHandle creator)
{
    Ogre::Skeleton* skeleton = static_cast<Ogre::Skeleton*>(creator);
    Ogre::TagPoint* tag = new Ogre::TagPoint(bone_handle, skeleton);
    return static_cast<TagPointHandle>(tag);
}

//~TagPoint();
void destroy_tagpoint(TagPointHandle handle)
{
    Ogre::TagPoint* tag = static_cast<Ogre::TagPoint*>(handle); 
    delete tag;
}


//Entity *getParentEntity(void) const;
EntityHandle tagpoint_get_parent_entity(const TagPointHandle handle)
{
    const Ogre::TagPoint* tag = static_cast<const Ogre::TagPoint*>(handle); 
    Ogre::Entity* ent = tag->getParentEntity();
    return static_cast<EntityHandle>(ent);
}


//MovableObject* getChildObject(void) const;
MovableObjectHandle tagpoint_get_child_object(const TagPointHandle handle)
{
    const Ogre::TagPoint* tag = static_cast<const Ogre::TagPoint*>(handle);
    Ogre::MovableObject* obj = tag->getChildObject();
    return static_cast<MovableObjectHandle>(obj);
}


//void setParentEntity(Entity *pEntity);
void tagpoint_set_parent_entity(TagPointHandle handle, EntityHandle entity)
{
    Ogre::TagPoint* tag = static_cast<Ogre::TagPoint*>(handle);
    Ogre::Entity* ent = static_cast<Ogre::Entity*>(entity);
    tag->setParentEntity(ent);
}


//void setChildObject(MovableObject *pObject);
void tagpoint_set_child_object(TagPointHandle handle, MovableObjectHandle obj)
{
    Ogre::TagPoint* tag = static_cast<Ogre::TagPoint*>(handle);
    Ogre::MovableObject* movableObj = static_cast<Ogre::MovableObject*>(obj);
    tag->setChildObject(movableObj);
}


//void setInheritParentEntityOrientation(bool inherit);
void tagpoint_set_inherit_parent_entity_orientation(TagPointHandle handle, int inherit)
{
    Ogre::TagPoint* tag = static_cast<Ogre::TagPoint*>(handle);
    tag->setInheritParentEntityOrientation(inherit);
}


//bool getInheritParentEntityOrientation(void) const;
int tagpoint_get_inherit_parent_entity_orientation(const TagPointHandle handle)
{
    const Ogre::TagPoint* tag = static_cast<const Ogre::TagPoint*>(handle);
    return tag->getInheritParentEntityOrientation();
}


//void setInheritParentEntityScale(bool inherit);
void tagpoint_set_inherit_parent_entity_scale(TagPointHandle handle, int inherit)
{
    Ogre::TagPoint* tag = static_cast<Ogre::TagPoint*>(handle);
    tag->setInheritParentEntityScale(inherit);
}


//bool getInheritParentEntityScale(void) const;
int tagpoint_get_inherit_parent_entity_scale(const TagPointHandle handle)
{
    const Ogre::TagPoint* tag = static_cast<const Ogre::TagPoint*>(handle);
    return tag->getInheritParentEntityScale();
}


//const Matrix4& getParentEntityTransform(void) const;
void tagpoint_get_parent_entity_transform(const TagPointHandle handle, coiMatrix4* result)
{
    const Ogre::TagPoint* tag = static_cast<const Ogre::TagPoint*>(handle);
    const Ogre::Matrix4& m4 = tag->getParentEntityTransform();
    ogre_matrix4_to_llcoi_matrix4(m4, *result);
}


//const Matrix4& _getFullLocalTransform(void) const;
void tagpoint__get_full_local_transform(const TagPointHandle handle, coiMatrix4* result)
{
    const Ogre::TagPoint* tag = static_cast<const Ogre::TagPoint*>(handle);
    const Ogre::Matrix4& m4 = tag->_getFullLocalTransform();
    ogre_matrix4_to_llcoi_matrix4(m4, *result);
}


//void needUpdate(bool forceParentUpdate = false);
void tagpoint_need_update(TagPointHandle handle, int force_parent_update)
{
    Ogre::TagPoint* tag = static_cast<Ogre::TagPoint*>(handle);
    tag->needUpdate(force_parent_update);
}


//void updateFromParentImpl(void) const;
void tagpoint_update_from_parent_impl(const TagPointHandle handle)
{
    const Ogre::TagPoint* tag = static_cast<const Ogre::TagPoint*>(handle);
    tag->updateFromParentImpl();
}

//TODO: const LightList& getLights(void) const;
