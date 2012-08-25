/******************************************************************************
 * bone_bind.cpp - bindings for Ogre::Bone
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

#include "bone_bind.h"
#include "binding_utils.h"
#include <OgreBone.h>
#include <OgreSkeleton.h>


//Bone(unsigned short handle, Skeleton* creator);
BoneHandle create_bone(unsigned short handle, SkeletonHandle creator)
{
    Ogre::Skeleton* skeleton = static_cast<Ogre::Skeleton*>(creator);
    Ogre::Bone* bone = new Ogre::Bone(handle, skeleton);
    return static_cast<BoneHandle>(bone);
}

//Bone(const String& name, unsigned short handle, Skeleton* creator);
BoneHandle create_named_bone(const char* name, unsigned short handle, SkeletonHandle creator)
{
    Ogre::Skeleton* skeleton = static_cast<Ogre::Skeleton*>(creator);
    Ogre::Bone* bone = new Ogre::Bone(Ogre::String(name), handle, skeleton);
    return static_cast<BoneHandle>(bone);
}

//~Bone();
void destroy_bone(BoneHandle handle)
{
    Ogre::Bone* bone = static_cast<Ogre::Bone*>(handle);
    delete bone;
}

//Bone* createChild(unsigned short handle, const Vector3& translate = Vector3::ZERO, const Quaternion& rotate = Quaternion::IDENTITY);
BoneHandle bone_create_child(BoneHandle obj, unsigned short handle, const coiVector3* t, const coiQuaternion* r)
{
    Ogre::Bone* bone = static_cast<Ogre::Bone*>(obj);

    const Ogre::Vector3 translate(t->x, t->y, t->z);
    const Ogre::Quaternion rotate(r->w, r->x, r->y, r->z);

    Ogre::Bone* child = bone->createChild(handle, translate, rotate);
    return static_cast<BoneHandle>(child);
}

//unsigned short getHandle(void) const;
unsigned short bone_get_handle(const BoneHandle handle)
{
    const Ogre::Bone* bone = static_cast<const Ogre::Bone*>(handle); 
    return bone->getHandle();
}

//void setBindingPose(void);
void bone_set_binding_pose(BoneHandle handle)
{
    Ogre::Bone* bone = static_cast<Ogre::Bone*>(handle);
    bone->setBindingPose();
}

//void reset(void);
void bone_reset(BoneHandle handle)
{
    Ogre::Bone* bone = static_cast<Ogre::Bone*>(handle);
    bone->reset();
}

//void setManuallyControlled(bool manuallyControlled);
void bone_set_manually_controlled(BoneHandle handle, int manually_controlled)
{
    Ogre::Bone* bone = static_cast<Ogre::Bone*>(handle); 
    bone->setManuallyControlled(manually_controlled);
}

//bool isManuallyControlled() const;
int bone_is_manually_controlled(const BoneHandle handle)
{
    const Ogre::Bone* bone = static_cast<const Ogre::Bone*>(handle); 
    return bone->isManuallyControlled();
}

//void _getOffsetTransform(Matrix4& m) const;
void bone__get_offset_transform(const BoneHandle handle, coiMatrix4* m)
{
    const Ogre::Bone* bone = static_cast<const Ogre::Bone*>(handle); 
    Ogre::Matrix4 result;
    bone->_getOffsetTransform(result);
    ogre_matrix4_to_llcoi_matrix4(result, *m);
}

//const Vector3& _getBindingPoseInverseScale(void) const;
void bone__get_binding_pose_inverse_scale(const BoneHandle handle, coiVector3* result)
{
    const Ogre::Bone* bone = static_cast<const Ogre::Bone*>(handle); 
    const Ogre::Vector3& v = bone->_getBindingPoseInverseScale();

    result->x = v.x;
    result->y = v.y;
    result->z = v.z;
}

//const Vector3& _getBindingPoseInversePosition(void) const;
void bone__get_binding_pose_inverse_position(const BoneHandle handle, coiVector3* result)
{
    const Ogre::Bone* bone = static_cast<const Ogre::Bone*>(handle); 
    const Ogre::Vector3& v = bone->_getBindingPoseInversePosition();

    result->x = v.x;
    result->y = v.y;
    result->z = v.z;
}

//const Quaternion& _getBindingPoseInverseOrientation(void) const;
void bone__get_binding_pose_inverse_orientation(const BoneHandle handle, coiQuaternion* result)
{

    const Ogre::Bone* bone = static_cast<const Ogre::Bone*>(handle); 
    const Ogre::Quaternion& q = bone->_getBindingPoseInverseOrientation();

    result->w = q.w;
    result->x = q.x;
    result->y = q.y;
    result->z = q.z;
}

//void needUpdate(bool forceParentUpdate = false);
void bone_need_update(BoneHandle handle, int force_parent_update)
{
    Ogre::Bone* bone = static_cast<Ogre::Bone*>(handle);
    bone->needUpdate(force_parent_update);
}
