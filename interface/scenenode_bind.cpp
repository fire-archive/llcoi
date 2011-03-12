/******************************************************************************
 * scenenode_bind.cpp - bindings for Ogre::SceneNode
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
#include <OgreSceneNode.h>
#include <OgreEntity.h>
#include "ogre_manager.h"


SceneNodeHandle create_child_scenenode(const char* node_name)
{
    Ogre::SceneNode* scenenode = Ogre::Root::getSingletonPtr()->getSceneManager(OgreManager::getSingletonPtr()->get_active_scene_manager_name())->getRootSceneNode()->createChildSceneNode(node_name);
    return reinterpret_cast<SceneNodeHandle>(scenenode);
}

void attach_entity_to_scenenode(EntityHandle entity_handle, SceneNodeHandle scenenode_handle)
{
    Ogre::Entity* object = reinterpret_cast<Ogre::Entity*>(entity_handle);
    Ogre::SceneNode* node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    node->attachObject(object);
}

/*
Ogre::SceneNode::operator=(Ogre::SceneNode const&)
Ogre::SceneNode::SceneNode(Ogre::SceneNode const&)
Ogre::SceneNode::SceneNode(Ogre::SceneManager*)
Ogre::SceneNode::SceneNode(Ogre::SceneManager*, std::string const&)
Ogre::SceneNode::~SceneNode()
Ogre::SceneNode::attachObject(Ogre::MovableObject*)
Ogre::SceneNode::numAttachedObjects() const
Ogre::SceneNode::getAttachedObject(unsigned short)
Ogre::SceneNode::getAttachedObject(std::string const&)
Ogre::SceneNode::detachObject(unsigned short)
Ogre::SceneNode::detachObject(Ogre::MovableObject*)
Ogre::SceneNode::detachObject(std::string const&)
Ogre::SceneNode::detachAllObjects()
Ogre::SceneNode::isInSceneGraph() const
Ogre::SceneNode::_notifyRootNode()
Ogre::SceneNode::_update(bool, bool)
Ogre::SceneNode::_updateBounds()
Ogre::SceneNode::_findVisibleObjects(Ogre::Camera*, Ogre::RenderQueue*, Ogre::VisibleObjectsBoundsInfo*, bool, bool, bool)
Ogre::SceneNode::_getWorldAABB() const
Ogre::SceneNode::getAttachedObjectIterator()
Ogre::SceneNode::getAttachedObjectIterator() const
Ogre::SceneNode::getCreator() const
Ogre::SceneNode::removeAndDestroyChild(std::string const&)
Ogre::SceneNode::removeAndDestroyChild(unsigned short)
Ogre::SceneNode::removeAndDestroyAllChildren()
Ogre::SceneNode::showBoundingBox(bool)
Ogre::SceneNode::hideBoundingBox(bool)
Ogre::SceneNode::_addBoundingBoxToQueue(Ogre::RenderQueue*)
Ogre::SceneNode::getShowBoundingBox() const
Ogre::SceneNode::createChildSceneNode(Ogre::Vector3 const&, Ogre::Quaternion const&)
Ogre::SceneNode::createChildSceneNode(std::string const&, Ogre::Vector3 const&, Ogre::Quaternion const&)
Ogre::SceneNode::findLights(Ogre::HashedVector<Ogre::Light*>&, float, unsigned int) const
Ogre::SceneNode::setFixedYawAxis(bool, Ogre::Vector3 const&)
Ogre::SceneNode::yaw(Ogre::Radian const&, Ogre::Node::TransformSpace)
Ogre::SceneNode::setDirection(float, float, float, Ogre::Node::TransformSpace, Ogre::Vector3 const&)
Ogre::SceneNode::setDirection(Ogre::Vector3 const&, Ogre::Node::TransformSpace, Ogre::Vector3 const&)
Ogre::SceneNode::lookAt(Ogre::Vector3 const&, Ogre::Node::TransformSpace, Ogre::Vector3 const&)
Ogre::SceneNode::setAutoTracking(bool, Ogre::SceneNode*, Ogre::Vector3 const&, Ogre::Vector3 const&)
Ogre::SceneNode::getAutoTrackTarget()
Ogre::SceneNode::getAutoTrackOffset()
Ogre::SceneNode::getAutoTrackLocalDirection()
Ogre::SceneNode::_autoTrack()
Ogre::SceneNode::getParentSceneNode() const
Ogre::SceneNode::setVisible(bool, bool)
Ogre::SceneNode::flipVisibility(bool)
Ogre::SceneNode::setDebugDisplayEnabled(bool, bool)
Ogre::SceneNode::getDebugRenderable()
*/