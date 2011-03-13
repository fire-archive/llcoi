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
#include "ogre_manager.h"

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

// Ogre::Camera::setPosition(float, float, float)
// Ogre::Camera::setPosition(Ogre::Vector3 const&)
void camera_set_position(CameraHandle camera_handle, const float x, const float y, const float z)
{
    Ogre::Camera* camera = reinterpret_cast<Ogre::Camera*>(camera_handle);
    camera->setPosition(Ogre::Vector3(x, y, z));
}

// Ogre::Camera::lookAt(Ogre::Vector3 const&)
// Ogre::Camera::lookAt(float, float, float)
void camera_lookat(CameraHandle camera_handle, const float x, const float y, const float z)
{
    Ogre::Camera* camera = reinterpret_cast<Ogre::Camera*>(camera_handle);
    camera->lookAt(Ogre::Vector3(x, y, z));
}

CameraHandle create_camera(const char* camera_name)
{
    Ogre::Camera* camera = Ogre::Root::getSingletonPtr()->getSceneManager(OgreManager::getSingletonPtr()->get_active_scene_manager_name())->createCamera(camera_name);
    return reinterpret_cast<CameraHandle>(camera);
}

CameraHandle get_camera(const char* camera_name)
{
    Ogre::Camera* camera =  Ogre::Root::getSingletonPtr()->getSceneManager(OgreManager::getSingletonPtr()->get_active_scene_manager_name())->getCamera(camera_name);
    return reinterpret_cast<CameraHandle>(camera);
}

/*
Ogre::Camera::Listener
Ogre::Camera::Camera(std::string const&, Ogre::SceneManager*)
Ogre::Camera::~Camera()
Ogre::Camera::addListener(Ogre::Camera::Listener*)
Ogre::Camera::removeListener(Ogre::Camera::Listener*)
Ogre::Camera::getSceneManager() const
Ogre::Camera::setPolygonMode(Ogre::PolygonMode)
Ogre::Camera::getPolygonMode() const
Ogre::Camera::getPosition() const
Ogre::Camera::move(Ogre::Vector3 const&)
Ogre::Camera::moveRelative(Ogre::Vector3 const&)
Ogre::Camera::setDirection(float, float, float)
Ogre::Camera::setDirection(Ogre::Vector3 const&)
Ogre::Camera::getDirection() const
Ogre::Camera::getUp() const
Ogre::Camera::getRight() const
Ogre::Camera::roll(Ogre::Radian const&)
Ogre::Camera::yaw(Ogre::Radian const&)
Ogre::Camera::pitch(Ogre::Radian const&)
Ogre::Camera::rotate(Ogre::Vector3 const&, Ogre::Radian const&)
Ogre::Camera::rotate(Ogre::Quaternion const&)
Ogre::Camera::setFixedYawAxis(bool, Ogre::Vector3 const&)
Ogre::Camera::getOrientation() const
Ogre::Camera::setOrientation(Ogre::Quaternion const&)
Ogre::Camera::_renderScene(Ogre::Viewport*, bool)
Ogre::Camera::_notifyRenderedFaces(unsigned int)
Ogre::Camera::_notifyRenderedBatches(unsigned int)
Ogre::Camera::_getNumRenderedFaces() const
Ogre::Camera::_getNumRenderedBatches() const
Ogre::Camera::getDerivedOrientation() const
Ogre::Camera::getDerivedPosition() const
Ogre::Camera::getDerivedDirection() const
Ogre::Camera::getDerivedUp() const
Ogre::Camera::getDerivedRight() const
Ogre::Camera::getRealOrientation() const
Ogre::Camera::getRealPosition() const
Ogre::Camera::getRealDirection() const
Ogre::Camera::getRealUp() const
Ogre::Camera::getRealRight() const
Ogre::Camera::getMovableType() const
Ogre::Camera::setAutoTracking(bool, Ogre::SceneNode*, Ogre::Vector3 const&)
Ogre::Camera::setLodBias(float)
Ogre::Camera::getLodBias() const
Ogre::Camera::setLodCamera(Ogre::Camera const*)
Ogre::Camera::getLodCamera() const
Ogre::Camera::getCameraToViewportRay(float, float) const
Ogre::Camera::getCameraToViewportRay(float, float, Ogre::Ray*) const
Ogre::Camera::getCameraToViewportBoxVolume(float, float, float, float, bool)
Ogre::Camera::getCameraToViewportBoxVolume(float, float, float, float, Ogre::PlaneBoundedVolume*, bool)
Ogre::Camera::_getLodBiasInverse() const
Ogre::Camera::_autoTrack()
Ogre::Camera::setWindow(float, float, float, float)
Ogre::Camera::resetWindow()
Ogre::Camera::isWindowSet() const
Ogre::Camera::getWindowPlanes() const
Ogre::Camera::getBoundingRadius() const
Ogre::Camera::getAutoTrackTarget() const
Ogre::Camera::getAutoTrackOffset() const
Ogre::Camera::getViewport() const
Ogre::Camera::_notifyViewport(Ogre::Viewport*)
Ogre::Camera::setAutoAspectRatio(bool)
Ogre::Camera::getAutoAspectRatio() const
Ogre::Camera::setCullingFrustum(Ogre::Frustum*)
Ogre::Camera::getCullingFrustum() const
Ogre::Camera::forwardIntersect(Ogre::Plane const&, std::vector<Ogre::Vector4, Ogre::STLAllocator<Ogre::Vector4, Ogre::CategorisedAllocPolicy<(Ogre::MemoryCategory)0> > >*) const
Ogre::Camera::isVisible(Ogre::AxisAlignedBox const&, Ogre::FrustumPlane*) const
Ogre::Camera::isVisible(Ogre::Sphere const&, Ogre::FrustumPlane*) const
Ogre::Camera::isVisible(Ogre::Vector3 const&, Ogre::FrustumPlane*) const
Ogre::Camera::getWorldSpaceCorners() const
Ogre::Camera::getFrustumPlane(unsigned short) const
Ogre::Camera::projectSphere(Ogre::Sphere const&, float*, float*, float*, float*) const
Ogre::Camera::getNearClipDistance() const
Ogre::Camera::getFarClipDistance() const
Ogre::Camera::getViewMatrix() const
Ogre::Camera::getViewMatrix(bool) const
Ogre::Camera::setUseRenderingDistance(bool)
Ogre::Camera::getUseRenderingDistance() const
Ogre::Camera::synchroniseBaseSettingsWith(Ogre::Camera const*)
Ogre::Camera::getPositionForViewUpdate() const
Ogre::Camera::getOrientationForViewUpdate() const
Ogre::Camera::Listener::operator=(Ogre::Camera::Listener const&)
Ogre::Camera::Listener::Listener(Ogre::Camera::Listener const&)
Ogre::Camera::Listener::Listener()
Ogre::Camera::Listener::~Listener()
Ogre::Camera::Listener::cameraPreRenderScene(Ogre::Camera*)
Ogre::Camera::Listener::cameraPostRenderScene(Ogre::Camera*)
Ogre::Camera::Listener::cameraDestroyed(Ogre::Camera*)
*/