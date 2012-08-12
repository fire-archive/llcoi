/******************************************************************************
 * manualobject_bind.cpp - bindings for Ogre::ManualObject
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
#include <OgreManualObject.h>
#include "ogre_manager.h"

/*
Ogre::ManualObject::ManualObjectSection
Ogre::ManualObject::ManualObjectSectionShadowRenderable
Ogre::ManualObject::operator=(Ogre::ManualObject const&)
Ogre::ManualObject::ManualObject(Ogre::ManualObject const&)
Ogre::ManualObject::ManualObject(std::string const&)
Ogre::ManualObject::~ManualObject()
Ogre::ManualObject::clear()
Ogre::ManualObject::estimateVertexCount(unsigned int)
Ogre::ManualObject::estimateIndexCount(unsigned int)
Ogre::ManualObject::begin(std::string const&, Ogre::RenderOperation::OperationType, std::string const&)
Ogre::ManualObject::setDynamic(bool)
Ogre::ManualObject::getDynamic() const
Ogre::ManualObject::beginUpdate(unsigned int)
Ogre::ManualObject::position(Ogre::Vector3 const&)
Ogre::ManualObject::position(float, float, float)
Ogre::ManualObject::normal(Ogre::Vector3 const&)
Ogre::ManualObject::normal(float, float, float)
Ogre::ManualObject::tangent(Ogre::Vector3 const&)
Ogre::ManualObject::tangent(float, float, float)
Ogre::ManualObject::textureCoord(float)
Ogre::ManualObject::textureCoord(float, float)
Ogre::ManualObject::textureCoord(float, float, float)
Ogre::ManualObject::textureCoord(float, float, float, float)
Ogre::ManualObject::textureCoord(Ogre::Vector2 const&)
Ogre::ManualObject::textureCoord(Ogre::Vector3 const&)
Ogre::ManualObject::textureCoord(Ogre::Vector4 const&)
Ogre::ManualObject::colour(Ogre::ColourValue const&)
Ogre::ManualObject::colour(float, float, float, float)
Ogre::ManualObject::index(unsigned int)
Ogre::ManualObject::triangle(unsigned int, unsigned int, unsigned int)
Ogre::ManualObject::quad(unsigned int, unsigned int, unsigned int, unsigned int)
Ogre::ManualObject::end()
Ogre::ManualObject::setMaterialName(unsigned int, std::string const&, std::string const&)
Ogre::ManualObject::convertToMesh(std::string const&, std::string const&)
Ogre::ManualObject::setUseIdentityProjection(bool)
Ogre::ManualObject::getUseIdentityProjection() const
Ogre::ManualObject::setUseIdentityView(bool)
Ogre::ManualObject::getUseIdentityView() const
Ogre::ManualObject::setBoundingBox(Ogre::AxisAlignedBox const&)
Ogre::ManualObject::getSection(unsigned int) const
Ogre::ManualObject::getNumSections() const
Ogre::ManualObject::setKeepDeclarationOrder(bool)
Ogre::ManualObject::getKeepDeclarationOrder() const
Ogre::ManualObject::getMovableType() const
Ogre::ManualObject::getBoundingBox() const
Ogre::ManualObject::getBoundingRadius() const
Ogre::ManualObject::_updateRenderQueue(Ogre::RenderQueue*)
Ogre::ManualObject::getEdgeList()
Ogre::ManualObject::hasEdgeList()
Ogre::ManualObject::getShadowVolumeRenderableIterator(Ogre::ShadowTechnique, Ogre::Light const*, Ogre::HardwareIndexBufferSharedPtr*, bool, float, unsigned long)
Ogre::ManualObject::visitRenderables(Ogre::Renderable::Visitor*, bool)
Ogre::ManualObject::ManualObjectSection::operator=(Ogre::ManualObject::ManualObjectSection const&)
Ogre::ManualObject::ManualObjectSection::ManualObjectSection(Ogre::ManualObject::ManualObjectSection const&)
Ogre::ManualObject::ManualObjectSection::ManualObjectSection(Ogre::ManualObject*, std::string const&, Ogre::RenderOperation::OperationType, std::string const&)
Ogre::ManualObject::ManualObjectSection::~ManualObjectSection()
Ogre::ManualObject::ManualObjectSection::getRenderOperation()
Ogre::ManualObject::ManualObjectSection::getMaterialName() const
Ogre::ManualObject::ManualObjectSection::getMaterialGroup() const
Ogre::ManualObject::ManualObjectSection::setMaterialName(std::string const&, std::string const&)
Ogre::ManualObject::ManualObjectSection::set32BitIndices(bool)
Ogre::ManualObject::ManualObjectSection::get32BitIndices() const
Ogre::ManualObject::ManualObjectSection::getMaterial() const
Ogre::ManualObject::ManualObjectSection::getRenderOperation(Ogre::RenderOperation&)
Ogre::ManualObject::ManualObjectSection::getWorldTransforms(Ogre::Matrix4*) const
Ogre::ManualObject::ManualObjectSection::getSquaredViewDepth(Ogre::Camera const*) const
Ogre::ManualObject::ManualObjectSection::getLights() const
Ogre::ManualObject::ManualObjectSectionShadowRenderable::operator=(Ogre::ManualObject::ManualObjectSectionShadowRenderable const&)
Ogre::ManualObject::ManualObjectSectionShadowRenderable::ManualObjectSectionShadowRenderable(Ogre::ManualObject::ManualObjectSectionShadowRenderable const&)
Ogre::ManualObject::ManualObjectSectionShadowRenderable::ManualObjectSectionShadowRenderable(Ogre::ManualObject*, Ogre::HardwareIndexBufferSharedPtr*, Ogre::VertexData const*, bool, bool)
Ogre::ManualObject::ManualObjectSectionShadowRenderable::~ManualObjectSectionShadowRenderable()
Ogre::ManualObject::ManualObjectSectionShadowRenderable::getWorldTransforms(Ogre::Matrix4*) const
Ogre::ManualObject::ManualObjectSectionShadowRenderable::getPositionBuffer()
Ogre::ManualObject::ManualObjectSectionShadowRenderable::getWBuffer()
Ogre::ManualObject::TempVertex::~TempVertex()
Ogre::ManualObject::TempVertex::operator=(Ogre::ManualObject::TempVertex const&)
Ogre::ManualObject::TempVertex::TempVertex(Ogre::ManualObject::TempVertex const&)
Ogre::ManualObject::TempVertex::TempVertex()
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
