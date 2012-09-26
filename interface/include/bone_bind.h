/******************************************************************************
 * bone_bind.h - bindings for Ogre::Bone
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

typedef void* BoneHandle;
typedef void* SkeletonHandle;

#include "ogre_interface.h"

//Bone(unsigned short handle, Skeleton* creator);
DLL BoneHandle create_bone(unsigned short handle, SkeletonHandle creator);
//Bone(const String& name, unsigned short handle, Skeleton* creator);
DLL BoneHandle create_named_bone(const char* name, unsigned short handle, SkeletonHandle creator);
//~Bone();
DLL void destroy_bone(BoneHandle obj);
//Bone* createChild(unsigned short handle, const Vector3& translate = Vector3::ZERO, const Quaternion& rotate = Quaternion::IDENTITY);
DLL BoneHandle bone_create_child(BoneHandle obj, unsigned short handle, const coiVector3* translate, const coiQuaternion* rotate);
//unsigned short getHandle(void) const;
DLL unsigned short bone_get_handle(const BoneHandle handle);
//void setBindingPose(void);
DLL void bone_set_binding_pose(BoneHandle handle);
//void reset(void);
DLL void bone_reset(BoneHandle handle);
//void setManuallyControlled(bool manuallyControlled);
DLL void bone_set_manually_controlled(BoneHandle handle, int manually_controlled);
//bool isManuallyControlled() const;
DLL int bone_is_manually_controlled(const BoneHandle handle);
//void _getOffsetTransform(Matrix4& m) const;
DLL void bone__get_offset_transform(const BoneHandle handle, coiMatrix4* m);
//const Vector3& _getBindingPoseInverseScale(void) const;
DLL void bone__get_binding_pose_inverse_scale(const BoneHandle handle, coiVector3* result);
//const Vector3& _getBindingPoseInversePosition(void) const;
DLL void bone__get_binding_pose_inverse_position(const BoneHandle handle, coiVector3* result);
//const Quaternion& _getBindingPoseInverseOrientation(void) const;
DLL void bone__get_binding_pose_inverse_orientation(const BoneHandle handle, coiQuaternion* result);
//void needUpdate(bool forceParentUpdate = false);
DLL void bone_need_update(BoneHandle handle, int force_parent_update);
