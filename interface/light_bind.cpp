/******************************************************************************
 * light_bind.cpp - bindings for Ogre::Light
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
#include <OgreLight.h>
#include "ogre_manager.h"

LightHandle create_light(const char* light_name)
{
    Ogre::Light* light = Ogre::Root::getSingletonPtr()->getSceneManager(OgreManager::getSingletonPtr()->get_active_scene_manager_name())->createLight(light_name);
    return reinterpret_cast<LightHandle>(light);
}

void light_set_position(LightHandle light_handle, const float x, const float y, const float z)
{
    Ogre::Light* light = reinterpret_cast<Ogre::Light*>(light_handle);
    light->setPosition(Ogre::Vector3(x, y, z));
}

/*
Ogre::Light::operator=(Ogre::Light const&)
Ogre::Light::Light(Ogre::Light const&)
Ogre::Light::_calcTempSquareDist(Ogre::Vector3 const&)
Ogre::Light::Light()
Ogre::Light::Light(std::string const&)
Ogre::Light::~Light()
Ogre::Light::setType(Ogre::Light::LightTypes)
Ogre::Light::getType() const
Ogre::Light::setDiffuseColour(float, float, float)
Ogre::Light::setDiffuseColour(Ogre::ColourValue const&)
Ogre::Light::getDiffuseColour() const
Ogre::Light::setSpecularColour(float, float, float)
Ogre::Light::setSpecularColour(Ogre::ColourValue const&)
Ogre::Light::getSpecularColour() const
Ogre::Light::setAttenuation(float, float, float, float)
Ogre::Light::getAttenuationRange() const
Ogre::Light::getAttenuationConstant() const
Ogre::Light::getAttenuationLinear() const
Ogre::Light::getAttenuationQuadric() const
Ogre::Light::setPosition(float, float, float)
Ogre::Light::setPosition(Ogre::Vector3 const&)
Ogre::Light::getPosition() const
Ogre::Light::setDirection(float, float, float)
Ogre::Light::setDirection(Ogre::Vector3 const&)
Ogre::Light::getDirection() const
Ogre::Light::setSpotlightRange(Ogre::Radian const&, Ogre::Radian const&, float)
Ogre::Light::getSpotlightInnerAngle() const
Ogre::Light::getSpotlightOuterAngle() const
Ogre::Light::getSpotlightFalloff() const
Ogre::Light::setSpotlightInnerAngle(Ogre::Radian const&)
Ogre::Light::setSpotlightOuterAngle(Ogre::Radian const&)
Ogre::Light::setSpotlightFalloff(float)
Ogre::Light::setPowerScale(float)
Ogre::Light::getPowerScale() const
Ogre::Light::_notifyAttached(Ogre::Node*, bool)
Ogre::Light::_notifyMoved()
Ogre::Light::getBoundingBox() const
Ogre::Light::_updateRenderQueue(Ogre::RenderQueue*)
Ogre::Light::getMovableType() const
Ogre::Light::getDerivedPosition(bool) const
Ogre::Light::getDerivedDirection() const
Ogre::Light::setVisible(bool)
Ogre::Light::getBoundingRadius() const
Ogre::Light::getAs4DVector(bool) const
Ogre::Light::_getNearClipVolume(Ogre::Camera const*) const
Ogre::Light::_getFrustumClipVolumes(Ogre::Camera const*) const
Ogre::Light::getTypeFlags() const
Ogre::Light::createAnimableValue(std::string const&)
Ogre::Light::setCustomShadowCameraSetup(Ogre::SharedPtr<Ogre::ShadowCameraSetup> const&)
Ogre::Light::resetCustomShadowCameraSetup()
Ogre::Light::getCustomShadowCameraSetup() const
Ogre::Light::visitRenderables(Ogre::Renderable::Visitor*, bool)
Ogre::Light::_getIndexInFrame() const
Ogre::Light::_notifyIndexInFrame(unsigned int)
Ogre::Light::setShadowFarDistance(float)
Ogre::Light::resetShadowFarDistance()
Ogre::Light::getShadowFarDistance() const
Ogre::Light::getShadowFarDistanceSquared() const
Ogre::Light::setShadowNearClipDistance(float)
Ogre::Light::getShadowNearClipDistance() const
Ogre::Light::_deriveShadowNearClipDistance(Ogre::Camera const*) const
Ogre::Light::setShadowFarClipDistance(float)
Ogre::Light::getShadowFarClipDistance() const
Ogre::Light::_deriveShadowFarClipDistance(Ogre::Camera const*) const
Ogre::Light::_setCameraRelative(Ogre::Camera*)
Ogre::Light::setCustomParameter(unsigned short, Ogre::Vector4 const&)
Ogre::Light::getCustomParameter(unsigned short) const
Ogre::Light::_updateCustomGpuParameter(unsigned short, Ogre::GpuProgramParameters::AutoConstantEntry const&, Ogre::GpuProgramParameters*) const
*/
/*
Ogre::MovableObject::Listener
Ogre::MovableObject::operator=(Ogre::MovableObject const&)
Ogre::MovableObject::MovableObject(Ogre::MovableObject const&)
Ogre::MovableObject::MovableObject()
Ogre::MovableObject::MovableObject(std::string const&)
Ogre::MovableObject::~MovableObject()
Ogre::MovableObject::_notifyCreator(Ogre::MovableObjectFactory*)
Ogre::MovableObject::_getCreator() const
Ogre::MovableObject::_notifyManager(Ogre::SceneManager*)
Ogre::MovableObject::_getManager() const
Ogre::MovableObject::getName() const
Ogre::MovableObject::getMovableType() const
Ogre::MovableObject::getParentNode() const
Ogre::MovableObject::getParentSceneNode() const
Ogre::MovableObject::isParentTagPoint() const
Ogre::MovableObject::_notifyAttached(Ogre::Node*, bool)
Ogre::MovableObject::isAttached() const
Ogre::MovableObject::detachFromParent()
Ogre::MovableObject::isInScene() const
Ogre::MovableObject::_notifyMoved()
Ogre::MovableObject::_notifyCurrentCamera(Ogre::Camera*)
Ogre::MovableObject::getBoundingBox() const
Ogre::MovableObject::getBoundingRadius() const
Ogre::MovableObject::getWorldBoundingBox(bool) const
Ogre::MovableObject::getWorldBoundingSphere(bool) const
Ogre::MovableObject::_updateRenderQueue(Ogre::RenderQueue*)
Ogre::MovableObject::setVisible(bool)
Ogre::MovableObject::getVisible() const
Ogre::MovableObject::isVisible() const
Ogre::MovableObject::setRenderingDistance(float)
Ogre::MovableObject::getRenderingDistance() const
Ogre::MovableObject::setUserAny(Ogre::Any const&)
Ogre::MovableObject::getUserAny() const
Ogre::MovableObject::getUserObjectBindings()
Ogre::MovableObject::getUserObjectBindings() const
Ogre::MovableObject::setRenderQueueGroup(unsigned char)
Ogre::MovableObject::setRenderQueueGroupAndPriority(unsigned char, unsigned short)
Ogre::MovableObject::getRenderQueueGroup() const
Ogre::MovableObject::_getParentNodeFullTransform() const
Ogre::MovableObject::setQueryFlags(unsigned int)
Ogre::MovableObject::addQueryFlags(unsigned int)
Ogre::MovableObject::removeQueryFlags(unsigned long)
Ogre::MovableObject::getQueryFlags() const
Ogre::MovableObject::setDefaultQueryFlags(unsigned int)
Ogre::MovableObject::getDefaultQueryFlags()
Ogre::MovableObject::setVisibilityFlags(unsigned int)
Ogre::MovableObject::addVisibilityFlags(unsigned int)
Ogre::MovableObject::removeVisibilityFlags(unsigned int)
Ogre::MovableObject::getVisibilityFlags() const
Ogre::MovableObject::setDefaultVisibilityFlags(unsigned int)
Ogre::MovableObject::getDefaultVisibilityFlags()
Ogre::MovableObject::setListener(Ogre::MovableObject::Listener*)
Ogre::MovableObject::getListener() const
Ogre::MovableObject::queryLights() const
Ogre::MovableObject::getLightMask() const
Ogre::MovableObject::setLightMask(unsigned int)
Ogre::MovableObject::_getLightList()
Ogre::MovableObject::getEdgeList()
Ogre::MovableObject::hasEdgeList()
Ogre::MovableObject::getShadowVolumeRenderableIterator(Ogre::ShadowTechnique, Ogre::Light const*, Ogre::HardwareIndexBufferSharedPtr*, bool, float, unsigned long)
Ogre::MovableObject::getLightCapBounds() const
Ogre::MovableObject::getDarkCapBounds(Ogre::Light const&, float) const
Ogre::MovableObject::setCastShadows(bool)
Ogre::MovableObject::getCastShadows() const
Ogre::MovableObject::getReceivesShadows()
Ogre::MovableObject::getPointExtrusionDistance(Ogre::Light const*) const
Ogre::MovableObject::getTypeFlags() const
Ogre::MovableObject::visitRenderables(Ogre::Renderable::Visitor*, bool)
Ogre::MovableObject::setDebugDisplayEnabled(bool)
Ogre::MovableObject::isDebugDisplayEnabled() const
Ogre::MovableObject::Listener::operator=(Ogre::MovableObject::Listener const&)
Ogre::MovableObject::Listener::Listener(Ogre::MovableObject::Listener const&)
Ogre::MovableObject::Listener::Listener()
Ogre::MovableObject::Listener::~Listener()
Ogre::MovableObject::Listener::objectDestroyed(Ogre::MovableObject*)
Ogre::MovableObject::Listener::objectAttached(Ogre::MovableObject*)
Ogre::MovableObject::Listener::objectDetached(Ogre::MovableObject*)
Ogre::MovableObject::Listener::objectMoved(Ogre::MovableObject*)
Ogre::MovableObject::Listener::objectRendering(Ogre::MovableObject const*, Ogre::Camera const*)
Ogre::MovableObject::Listener::objectQueryLights(Ogre::MovableObject const*)
*/
