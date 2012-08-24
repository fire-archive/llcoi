/******************************************************************************
 * movableobject_bind.h - bindings for Ogre::MovableObject
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
#ifndef MOVABLEOBJECT_BIND_H
#define MOVABLEOBJECT_BIND_H

#include "ogre_interface.h"
#define MovableObjectHandle void*

///~MovableObject();
///void _notifyCreator(MovableObjectFactory* fact);
///MovableObjectFactory*  _getCreator(void) const;
///void _notifyManager(SceneManager* man);
///SceneManager* _getManager(void) const;
///const String& getName(void) const;
///const String& getMovableType(void) const = 0;
///Node* getParentNode(void) const;
///SceneNode* getParentSceneNode(void) const;
///bool isParentTagPoint() const;
///void _notifyAttached(Node* parent, bool isTagPoint = false);
///bool isAttached(void) const;
///void detachFromParent(void);
///bool isInScene(void) const;
///void _notifyMoved(void);
///void _notifyCurrentCamera(Camera* cam);
///const AxisAlignedBox& getBoundingBox(void) const = 0;
///Real getBoundingRadius(void) const = 0;
///const AxisAlignedBox& getWorldBoundingBox(bool derive = false) const;
///const Sphere& getWorldBoundingSphere(bool derive = false) const;
///void _updateRenderQueue(RenderQueue* queue) = 0;
///void setVisible(bool visible);
///bool getVisible(void) const;
///bool isVisible(void) const;
///void setRenderingDistance(Real dist);
///Real getRenderingDistance(void) const;
///void setRenderingMinPixelSize(Real pixelSize);
///Real getRenderingMinPixelSize() const;
//void setUserAny(const Any& anything); XXX: deprecated
//const Any& getUserAny(void) const; XXX: deprecated
///UserObjectBindings&	getUserObjectBindings();
///const UserObjectBindings& getUserObjectBindings() const;
///void setRenderQueueGroup(uint8 queueID);
///void setRenderQueueGroupAndPriority(uint8 queueID, ushort priority);
///uint8 getRenderQueueGroup(void) const;
///const Matrix4& _getParentNodeFullTransform(void) const;
///void setQueryFlags(uint32 flags);
///void addQueryFlags(uint32 flags);
///void removeQueryFlags(uint32 flags);
///uint32 getQueryFlags(void) const;
///static void setDefaultQueryFlags(uint32 flags);
///static uint32 getDefaultQueryFlags();
///void setVisibilityFlags(uint32 flags)
///void addVisibilityFlags(uint32 flags);
///void removeVisibilityFlags(uint32 flags);
///uint32 getVisibilityFlags(void) const;
///static void setDefaultVisibilityFlags(uint32 flags);
///static uint32 getDefaultVisibilityFlags();
///void setListener(Listener* listener);
///Listener* getListener(void) const;
///const LightList& queryLights(void) const;
///uint32 getLightMask() const;
///void setLightMask(uint32 lightMask);
///LightList* _getLightList();
///EdgeData* getEdgeList(void);
///bool hasEdgeList(void);
///ShadowRenderableListIterator getShadowVolumeRenderableIterator(ShadowTechnique shadowTechnique, const Light* light, HardwareIndexBufferSharedPtr* indexBuffer,  bool extrudeVertices, Real extrusionDist, unsigned long flags = 0);
///const AxisAlignedBox& getLightCapBounds(void) const;
///const AxisAlignedBox& getDarkCapBounds(const Light& light, Real dirLightExtrusionDist) const;
///void setCastShadows(bool enabled);
///bool getCastShadows(void) const;
///bool getReceivesShadows();
///Real getPointExtrusionDistance(const Light* l) const;
///uint32 getTypeFlags(void) const;
///void visitRenderables(Renderable::Visitor* visitor,bool debugRenderables = false) = 0;
///void setDebugDisplayEnabled(bool enabled) { mDebugDisplay = enabled; }
///bool isDebugDisplayEnabled(void) const { return mDebugDisplay; }


#endif
