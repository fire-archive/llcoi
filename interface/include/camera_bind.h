/******************************************************************************
 * camera_bind.h - bindings for Ogre::Camera
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
#ifndef CAMERA_BIND_H
#define CAMERA_BIND_H

#include "ogre_interface.h"

#define CameraHandle void*
#define SceneManagerHandle void*
#define SceneNodeHandle void*
#define RayHandle void*

// Camera
DLL CameraHandle create_camera(const char* camera_name);

DLL CameraHandle get_camera(const char* camera_name);

DLL void camera_move(CameraHandle handle, const float x, const float y, const float z);

DLL void camera_move_relative(CameraHandle handle, const float x, const float y, const float z);

DLL void camera_set_direction(CameraHandle handle, const float x, const float y, const float z);

DLL void camera_get_direction(CameraHandle handle, coiVector3* v3);

DLL void camera_get_up(CameraHandle handle, coiVector3* up);

DLL void camera_get_right(CameraHandle handle, coiVector3* right);

DLL void camera_set_near_clip_distance(CameraHandle camera_handle, float d);

DLL void camera_set_far_clip_distance(CameraHandle camera_handle, float d);

DLL void camera_set_aspect_ratio(CameraHandle camera_handle, float w, float h);

DLL void camera_set_aspect_ratio_ex(CameraHandle handle, float ratio);

DLL float camera_get_aspect_ratio(CameraHandle handle);

DLL void camera_set_auto_aspect_ratio(CameraHandle camera_handle, int on);

DLL void camera_set_fovy(CameraHandle camera_handle, float angle);

DLL void camera_set_frustum_offset(CameraHandle camera_handle, const int offset_x, const int offset_y);

DLL void camera_set_focal_length(CameraHandle camera_handle, float fl);

DLL void camera_set_position(CameraHandle camera_handle, const float x, const float y, const float z);

DLL void camera_get_position(CameraHandle handle, coiVector3* result);


DLL void camera_lookat(CameraHandle camera_handle, const float x, const float y, const float z);

DLL void camera_roll(CameraHandle handle, coiReal angle);

DLL void camera_yaw(CameraHandle handle, coiReal angle);

DLL void camera_pitch(CameraHandle handle, coiReal angle);

DLL void camera_rotate(CameraHandle handle, const coiVector3* axis, coiReal angle);

DLL void camera_rotate_q(CameraHandle handle, const coiQuaternion* q);
//Ogre::Camera::setFixedYawAxis(bool, Ogre::Vector3 const&)
DLL void camera_set_fixed_yaw_axis(CameraHandle handle, int on, const coiVector3* fixed_axis);
//Ogre::Camera::getOrientation() const
DLL void camera_get_orientation(CameraHandle handle, coiQuaternion* orientation);
//Ogre::Camera::setOrientation(Ogre::Quaternion const&)
DLL void camera_set_orientation(CameraHandle handle, const coiQuaternion* orientation);

//Ogre::Camera::getDerivedOrientation() const
DLL void camera_get_derived_orientation(CameraHandle handle, coiQuaternion* orientation);
//Ogre::Camera::getDerivedPosition() const
DLL void camera_get_derived_position(CameraHandle handle, coiVector3* position);
//Ogre::Camera::getDerivedDirection() const
DLL void camera_get_derived_direction(CameraHandle handle, coiVector3* direction);
//Ogre::Camera::getDerivedUp() const
DLL void camera_get_derived_up(CameraHandle handle, coiVector3* up);
//Ogre::Camera::getDerivedRight() const
DLL void camera_get_derived_right(CameraHandle handle, coiVector3* right);
//Ogre::Camera::setAutoTracking(bool, Ogre::SceneNode*, Ogre::Vector3 const&)
DLL void camera_set_autotracking(CameraHandle handle, int on, SceneNodeHandle sn_handle, const coiVector3* offset);
//Ogre::Camera::setLodBias(float)
DLL void camera_set_lod_bias(CameraHandle handle, coiReal factor);
//Ogre::Camera::getLodBias() const
DLL coiReal camera_get_lod_bias(CameraHandle handle);
//Ogre::Camera::getCameraToViewportRay(float, float, Ogre::Ray*) const
DLL void camera_get_camera_to_viewport_ray(CameraHandle handle, coiReal screenx, coiReal screeny, RayHandle ray);
//Ogre::Camera::setWindow(float, float, float, float)
DLL void camera_set_window(CameraHandle handle, coiReal left, coiReal top, coiReal right, coiReal bottom);



DLL SceneManagerHandle camera_get_scenemanager(CameraHandle handle);


#endif
