/******************************************************************************
 * skeletoninstance_bind.cpp - bindings for Ogre::SkeletonInstance
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

#include "skeletoninstance_bind.h"

#include <OgreTagPoint.h>
#include <OgreBone.h>
#include <OgreSkeletonInstance.h>


//~SkeletonInstance();
void destroy_skeletoninstance(SkeletonInstanceHandle handle)
{
    Ogre::SkeletonInstance* inst = static_cast<Ogre::SkeletonInstance*>(handle);
    delete inst;
}

//unsigned short getNumAnimations(void) const;
unsigned short skeletoninstance_get_num_animations(const SkeletonInstanceHandle handle)
{
    const Ogre::SkeletonInstance* inst = static_cast<const Ogre::SkeletonInstance*>(handle);
    return inst->getNumAnimations();
}

//TODO: Animation* getAnimation(unsigned short index) const;
//TODO: Animation* _getAnimationImpl(const String& name,  const LinkedSkeletonAnimationSource** linker = 0) const;
//TODO: Animation* createAnimation(const String& name, Real length);
//TODO: Animation* getAnimation(const String& name,  const LinkedSkeletonAnimationSource** linker = 0) const;

//void removeAnimation(const String& name);
void skeletoninstance_remove_animation(SkeletonInstanceHandle handle, const char* name)
{
    Ogre::SkeletonInstance* inst = static_cast<Ogre::SkeletonInstance*>(handle);
    inst->removeAnimation(Ogre::String(name));
}

//TagPoint* createTagPointOnBone(Bone* bone, const Quaternion &offsetOrientation = Quaternion::IDENTITY,const Vector3 &offsetPosition = Vector3::ZERO);
TagPointHandle skeletoninstance_create_tag_point_on_bone(SkeletonInstanceHandle handle, BoneHandle bone_handle, const coiQuaternion* q, const coiVector3* v)
{
    Ogre::SkeletonInstance* inst = static_cast<Ogre::SkeletonInstance*>(handle);
    Ogre::Bone* bone = static_cast<Ogre::Bone*>(bone_handle);
    const Ogre::Quaternion orientation(q->w, q->x, q->y, q->z);
    const Ogre::Vector3 position(v->x, v->y, v->z);

    Ogre::TagPoint* tag = inst->createTagPointOnBone(bone, orientation, position);

    return static_cast<TagPointHandle>(tag);
}

//void freeTagPoint(TagPoint* tagPoint);
void skeletoninstance_free_tag_point(SkeletonInstanceHandle handle, TagPointHandle tag_point)
{
    Ogre::SkeletonInstance* inst = static_cast<Ogre::SkeletonInstance*>(handle);
    Ogre::TagPoint* tag = static_cast<Ogre::TagPoint*>(tag_point);
    inst->freeTagPoint(tag);
}

//void addLinkedSkeletonAnimationSource(const String& skelName, Real scale = 1.0f);
void skeletoninstance_add_linked_skeleton_animation_source(SkeletonInstanceHandle handle, const char* skel_name, coiReal scale)
{
    Ogre::SkeletonInstance* inst = static_cast<Ogre::SkeletonInstance*>(handle);
    inst->addLinkedSkeletonAnimationSource(Ogre::String(skel_name), scale);
}

//void removeAllLinkedSkeletonAnimationSources(void);
void skeletoninstance_remove_all_linked_skeleton_animation_sources(SkeletonInstanceHandle handle)
{
    Ogre::SkeletonInstance* inst = static_cast<Ogre::SkeletonInstance*>(handle);
    inst->removeAllLinkedSkeletonAnimationSources();
}

//TODO: LinkedSkeletonAnimSourceIterator getLinkedSkeletonAnimationSourceIterator(void) const;
//TODO: void _initAnimationState(AnimationStateSet* animSet);
//TODO: void _refreshAnimationState(AnimationStateSet* animSet);

//const String& getName(void) const;
const char* skeletoninstance_get_name(const SkeletonInstanceHandle handle)
{
    const Ogre::SkeletonInstance* inst = static_cast<const Ogre::SkeletonInstance*>(handle);
    return inst->getName().c_str();
}

//TODO: ResourceHandle getHandle(void) const;

//const String& getGroup(void);
const char* skeletoninstance_get_group(SkeletonInstanceHandle handle)
{
    Ogre::SkeletonInstance* inst = static_cast<Ogre::SkeletonInstance*>(handle);
    return inst->getGroup().c_str();
}


