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

// Camera
DLL CameraHandle create_camera(const char* camera_name);

DLL CameraHandle get_camera(const char* camera_name);

DLL void camera_move(CameraHandle handle, const float x, const float y, const float z);

DLL void camera_move_relative(CameraHandle handle, const float x, const float y, const float z);

DLL void camera_set_direction(CameraHandle handle, const float x, const float y, const float z);

DLL void camera_get_direction(CameraHandle handle, coiVector3* v3);

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

#endif
