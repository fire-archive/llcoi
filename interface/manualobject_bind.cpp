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