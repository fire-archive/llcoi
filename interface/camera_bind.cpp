/******************************************************************************
 * camera_bind.cpp - bindings for Ogre::Camera
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
#include "ogre_interface.h"

#include <OgreRoot.h>
#include <OgreRenderWindow.h>
#include <OgreCamera.h>

extern const char* active_scene_manager_name;
extern Ogre::RenderWindow* activeRenderWindow;

void camera_set_near_clip_distance(CameraHandle camera_handle, float d)
{
    Ogre::Camera* camera = reinterpret_cast<Ogre::Camera*>(camera_handle);
    camera->setNearClipDistance( d );
}

void camera_set_far_clip_distance(CameraHandle camera_handle, float d)
{
    Ogre::Camera* camera = reinterpret_cast<Ogre::Camera*>(camera_handle);
    camera->setFarClipDistance( d );
}

void camera_set_aspect_ratio(CameraHandle camera_handle, float w, float h)
{
    Ogre::Camera* camera = reinterpret_cast<Ogre::Camera*>(camera_handle);
    camera->setAspectRatio(Ogre::Real(Ogre::Real(w)/Ogre::Real(h)));
}

void camera_set_auto_aspect_ratio(CameraHandle camera_handle, bool on)
{
    Ogre::Camera* camera = reinterpret_cast<Ogre::Camera*>(camera_handle);
    camera->setAutoAspectRatio(on);
}

void camera_set_fovy(CameraHandle camera_handle, float angle)
{
    Ogre::Camera* camera = reinterpret_cast<Ogre::Camera*>(camera_handle);
    camera->setFOVy((Ogre::Radian)angle);
}

void camera_set_frustum_offset(CameraHandle camera_handle, const int offset_x, const int offset_y)
{
    Ogre::Camera* camera = reinterpret_cast<Ogre::Camera*>(camera_handle);
    camera->setFrustumOffset(Ogre::Vector2(offset_x, offset_y));
}

void camera_set_focal_length(CameraHandle camera_handle, float fl)
{
    Ogre::Camera* camera = reinterpret_cast<Ogre::Camera*>(camera_handle);
    camera->setFocalLength(fl);
}

void camera_set_position(CameraHandle camera_handle, const float x, const float y, const float z)
{
    Ogre::Camera* camera = reinterpret_cast<Ogre::Camera*>(camera_handle);
    camera->setPosition(Ogre::Vector3(x, y, z));
}

void camera_lookat(CameraHandle camera_handle, const float x, const float y, const float z)
{
    Ogre::Camera* camera = reinterpret_cast<Ogre::Camera*>(camera_handle);
    camera->lookAt(Ogre::Vector3(x, y, z));
}

CameraHandle create_camera(const char* camera_name)
{
    Ogre::Camera* camera = Ogre::Root::getSingletonPtr()->getSceneManager(active_scene_manager_name)->createCamera(camera_name);
    return reinterpret_cast<CameraHandle>(camera);
}

CameraHandle get_camera(const char* camera_name)
{
    Ogre::Camera* camera =  Ogre::Root::getSingletonPtr()->getSceneManager(active_scene_manager_name)->getCamera(camera_name);
    return reinterpret_cast<CameraHandle>(camera);
}
