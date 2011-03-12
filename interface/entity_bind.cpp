/******************************************************************************
 * entity_bind.cpp - bindings for Ogre::Entity
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
#include <OgreEntity.h>
#include "ogre_manager.h"

EntityHandle create_entity(const char* entity_name, const char* mesh_file)
{
    Ogre::Entity* entity = Ogre::Root::getSingletonPtr()->getSceneManager(OgreManager::getSingletonPtr()->get_active_scene_manager_name())->createEntity(entity_name, mesh_file);
    return reinterpret_cast<EntityHandle>(entity);
}

// How do we handle the fact that Ogre::Entity is an Ogre::MovableObject ?
// Duplicate?

/*
Ogre::Entity::operator=(Ogre::Entity const&)
Ogre::Entity::Entity(Ogre::Entity const&)
Ogre::Entity::~Entity()
Ogre::Entity::getMesh() const
Ogre::Entity::getSubEntity(unsigned int) const
Ogre::Entity::getSubEntity(std::string const&) const
Ogre::Entity::getNumSubEntities() const
Ogre::Entity::clone(std::string const&) const
Ogre::Entity::setMaterialName(std::string const&, std::string const&)
Ogre::Entity::setMaterial(Ogre::MaterialPtr const&)
Ogre::Entity::_notifyCurrentCamera(Ogre::Camera*)
Ogre::Entity::setRenderQueueGroup(unsigned char)
Ogre::Entity::setRenderQueueGroupAndPriority(unsigned char, unsigned short)
Ogre::Entity::getBoundingBox() const
Ogre::Entity::getChildObjectsBoundingBox() const
Ogre::Entity::_updateRenderQueue(Ogre::RenderQueue*)
Ogre::Entity::getMovableType() const
Ogre::Entity::getAnimationState(std::string const&) const
Ogre::Entity::getAllAnimationStates() const
Ogre::Entity::setDisplaySkeleton(bool)
Ogre::Entity::getDisplaySkeleton() const
Ogre::Entity::getManualLodLevel(unsigned int) const
Ogre::Entity::getNumManualLodLevels() const
Ogre::Entity::getCurrentLodIndex()
Ogre::Entity::setMeshLodBias(float, unsigned short, unsigned short)
Ogre::Entity::setMaterialLodBias(float, unsigned short, unsigned short)
Ogre::Entity::setPolygonModeOverrideable(bool)
Ogre::Entity::attachObjectToBone(std::string const&, Ogre::MovableObject*, Ogre::Quaternion const&, Ogre::Vector3 const&)
Ogre::Entity::detachObjectFromBone(std::string const&)
Ogre::Entity::detachObjectFromBone(Ogre::MovableObject*)
Ogre::Entity::detachAllObjectsFromBone()
Ogre::Entity::getAttachedObjectIterator()
Ogre::Entity::getBoundingRadius() const
Ogre::Entity::getWorldBoundingBox(bool) const
Ogre::Entity::getWorldBoundingSphere(bool) const
Ogre::Entity::getEdgeList()
Ogre::Entity::hasEdgeList()
Ogre::Entity::getShadowVolumeRenderableIterator(Ogre::ShadowTechnique, Ogre::Light const*, Ogre::HardwareIndexBufferSharedPtr*, bool, float, unsigned long)
Ogre::Entity::_getBoneMatrices() const
Ogre::Entity::_getNumBoneMatrices() const
Ogre::Entity::hasSkeleton() const
Ogre::Entity::getSkeleton() const
Ogre::Entity::isHardwareAnimationEnabled()
Ogre::Entity::_notifyAttached(Ogre::Node*, bool)
Ogre::Entity::getSoftwareAnimationRequests() const
Ogre::Entity::getSoftwareAnimationNormalsRequests() const
Ogre::Entity::addSoftwareAnimationRequest(bool)
Ogre::Entity::removeSoftwareAnimationRequest(bool)
Ogre::Entity::shareSkeletonInstanceWith(Ogre::Entity*)
Ogre::Entity::hasVertexAnimation() const
Ogre::Entity::stopSharingSkeletonInstance()
Ogre::Entity::sharesSkeletonInstance() const
Ogre::Entity::getSkeletonInstanceSharingSet() const
Ogre::Entity::refreshAvailableAnimationState()
Ogre::Entity::_updateAnimation()
Ogre::Entity::_isAnimated() const
Ogre::Entity::_isSkeletonAnimated() const
Ogre::Entity::_getSkelAnimVertexData() const
Ogre::Entity::_getSoftwareVertexAnimVertexData() const
Ogre::Entity::_getHardwareVertexAnimVertexData() const
Ogre::Entity::_getSkelAnimTempBufferInfo()
Ogre::Entity::_getVertexAnimTempBufferInfo()
Ogre::Entity::getTypeFlags() const
Ogre::Entity::getVertexDataForBinding()
Ogre::Entity::chooseVertexDataForBinding(bool)
Ogre::Entity::_getBuffersMarkedForAnimation() const
Ogre::Entity::_markBuffersUsedForAnimation()
Ogre::Entity::isInitialised() const
Ogre::Entity::_initialise(bool)
Ogre::Entity::_deinitialise()
Ogre::Entity::backgroundLoadingComplete(Ogre::Resource*)
Ogre::Entity::visitRenderables(Ogre::Renderable::Visitor*, bool)
Ogre::Entity::_getMeshLodFactorTransformed() const
Ogre::Entity::setSkipAnimationStateUpdate(bool)
Ogre::Entity::getSkipAnimationStateUpdate() const
Ogre::Entity::setAlwaysUpdateMainSkeleton(bool)
Ogre::Entity::getAlwaysUpdateMainSkeleton() const
Ogre::Entity::EntityShadowRenderable::operator=(Ogre::Entity::EntityShadowRenderable const&)
Ogre::Entity::EntityShadowRenderable::EntityShadowRenderable(Ogre::Entity::EntityShadowRenderable const&)
Ogre::Entity::EntityShadowRenderable::EntityShadowRenderable(Ogre::Entity*, Ogre::HardwareIndexBufferSharedPtr*, Ogre::VertexData const*, bool, Ogre::SubEntity*, bool)
Ogre::Entity::EntityShadowRenderable::~EntityShadowRenderable()
Ogre::Entity::EntityShadowRenderable::_createSeparateLightCap()
Ogre::Entity::EntityShadowRenderable::getWorldTransforms(Ogre::Matrix4*) const
Ogre::Entity::EntityShadowRenderable::getPositionBuffer()
Ogre::Entity::EntityShadowRenderable::getWBuffer()
Ogre::Entity::EntityShadowRenderable::rebindPositionBuffer(Ogre::VertexData const*, bool)
Ogre::Entity::EntityShadowRenderable::isVisible() const
*/