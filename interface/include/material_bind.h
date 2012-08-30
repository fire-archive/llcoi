/******************************************************************************
 * material_bind.h - bindings for Ogre::Material
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
#ifndef MATERIAL_BIND_H
#define MATERIAL_BIND_H
#include "ogre_interface.h"
#define MaterialHandle void*
#define MaterialPtrHandle void*

//Material(ResourceManager* creator, const String& name, ResourceHandle handle, const String& group, bool isManual = false, ManualResourceLoader* loader = 0)
//~Material()
//Material& operator=( const Material& rhs )
//bool isTransparent(void) const
//void setReceiveShadows(bool enabled) { mReceiveShadows = enabled; }
//bool getReceiveShadows(void) const { return mReceiveShadows; }
//void setTransparencyCastsShadows(bool enabled) { mTransparencyCastsShadows = enabled; }
//bool getTransparencyCastsShadows(void) const { return mTransparencyCastsShadows; }
//Technique* createTechnique(void)
//Technique* getTechnique(unsigned short index)
//Technique* getTechnique(const String& name)
//unsigned short getNumTechniques(void) const
//void removeTechnique(unsigned short index);		
//void removeAllTechniques(void)
//typedef VectorIterator<Techniques> TechniqueIterator
//TechniqueIterator getTechniqueIterator(void)
//TechniqueIterator getSupportedTechniqueIterator(void)
//Technique* getSupportedTechnique(unsigned short index)
//unsigned short getNumSupportedTechniques(void) const
//const String& getUnsupportedTechniquesExplanation() const { return mUnsupportedReasons; }
//unsigned short getNumLodLevels(unsigned short schemeIndex) const
//unsigned short getNumLodLevels(const String& schemeName) const
//Technique* getBestTechnique(unsigned short lodIndex = 0, const Renderable* rend = 0)
//MaterialPtr clone(const String& newName, bool changeGroup = false,  const String& newGroup = StringUtil::BLANK) const
//void copyDetailsTo(MaterialPtr& mat) const
//void compile(bool autoManageTextureUnits = true)
//void setPointSize(Real ps)
//void setAmbient(Real red, Real green, Real blue)
//void setAmbient(const ColourValue& ambient)
//void setDiffuse(Real red, Real green, Real blue, Real alpha)
//void setDiffuse(const ColourValue& diffuse)
//void setSpecular(Real red, Real green, Real blue, Real alpha)
//void setSpecular(const ColourValue& specular)
//void setShininess(Real val)
//void setSelfIllumination(Real red, Real green, Real blue)
//void setSelfIllumination(const ColourValue& selfIllum)
//void setDepthCheckEnabled(bool enabled)
//void setDepthWriteEnabled(bool enabled)
//void setDepthFunction( CompareFunction func )
//void setColourWriteEnabled(bool enabled)
//void setCullingMode( CullingMode mode )
//void setManualCullingMode( ManualCullingMode mode )
//void setLightingEnabled(bool enabled)
//void setShadingMode( ShadeOptions mode )
//void setFog(bool overrideScene,  FogMode mode = FOG_NONE, const ColourValue& colour = ColourValue::White, Real expDensity = 0.001, Real linearStart = 0.0, Real linearEnd = 1.0 )
//void setDepthBias(float constantBias, float slopeScaleBias)
//void setTextureFiltering(TextureFilterOptions filterType)
//void setTextureAnisotropy(int maxAniso)
//void setSceneBlending( const SceneBlendType sbt )
//void setSeparateSceneBlending( const SceneBlendType sbt, const SceneBlendType sbta )
//void setSceneBlending( const SceneBlendFactor sourceFactor, const SceneBlendFactor destFactor)
//void setSeparateSceneBlending( const SceneBlendFactor sourceFactor, const SceneBlendFactor destFactor, const SceneBlendFactor sourceFactorAlpha, const SceneBlendFactor destFactorAlpha)
//void _notifyNeedsRecompile(void)
//void setLodLevels(const LodValueList& lodValues)
//LodValueIterator getLodValueIterator(void) const
//LodValueIterator getUserLodValueIterator(void) const
//ushort getLodIndex(Real value) const
//const LodStrategy *getLodStrategy() const
//void setLodStrategy(LodStrategy *lodStrategy)
//void touch(void) 
//bool applyTextureAliases(const AliasTextureNamePairList& aliasList, const bool apply = true) const
bool getCompilationRequired() const

#endif
