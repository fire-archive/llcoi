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
 * See https://bitbucket.org/galaktor/llcoi for more information.
 *
 * Copyright (c) 2011-2012, Llcoi Team
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
#include "scenenode_bind.h"

#include <OgreSceneNode.h>
#include <OgreEntity.h>

// SceneNode is going to carry Node's code - we'll duplicate for Bone

//TODO: need to have a function which translates to/from Ogre::Node::TransformSpace


// Maybe this would be enough? One could set position and orientation afterwards..
// Ogre::SceneNode::createChildSceneNode(Ogre::Vector3 const&, Ogre::Quaternion const&)
// Ogre::SceneNode::createChildSceneNode(std::string const&, Ogre::Vector3 const&, Ogre::Quaternion const&)


void scenenode_attach_entity(SceneNodeHandle scenenode_handle, EntityHandle entity_handle)
{
  Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);   Ogre::Entity* entity = reinterpret_cast<Ogre::Entity*>(entity_handle);

  scene_node->attachObject(entity);
}

void scenenode_update(SceneNodeHandle scenenode_handle, int update_children, int parent_has_changed)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->_update(update_children, parent_has_changed);
}

void scenenode_update_bounds(SceneNodeHandle scenenode_handle)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->_updateBounds();
}

EntityHandle scenenode_get_attached_entity_int(SceneNodeHandle scenenode_handle, int entity_index)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    Ogre::Entity* entity = static_cast<Ogre::Entity*>(scene_node->getAttachedObject(entity_index));
    return reinterpret_cast<EntityHandle>(entity);
}

EntityHandle scenenode_get_attached_entity(SceneNodeHandle scenenode_handle, const char* entity_name)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    Ogre::Entity* entity = static_cast<Ogre::Entity*>(scene_node->getAttachedObject(entity_name));
    return reinterpret_cast<EntityHandle>(entity);
}

int scenenode_num_attached_objects(SceneNodeHandle scenenode_handle)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    return scene_node->numAttachedObjects();
}

void scenenode_detach_entity_int(SceneNodeHandle scenenode_handle, int entity_index)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->detachObject(entity_index);
}

void scenenode_detach_entity(SceneNodeHandle scenenode_handle, EntityHandle entity_handle)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    Ogre::Entity* entity = reinterpret_cast<Ogre::Entity*>(entity_handle);
    scene_node->detachObject(entity);
}

void scenenode_detach_entity_string(SceneNodeHandle scenenode_handle, const char* entity_name)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->detachObject(entity_name);
}

void scenenode_detach_all_objects(SceneNodeHandle scenenode_handle)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->detachAllObjects();
}

int scenenode_is_in_scenegraph(SceneNodeHandle scenenode_handle)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    if(scene_node->isInSceneGraph())
        return 1;
    return 0;
}

void scenenode_notify_rootnode(SceneNodeHandle scenenode_handle)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->_notifyRootNode();
}

void scenenode_show_boundingbox(SceneNodeHandle scenenode_handle, int show_boundingbox)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->showBoundingBox(show_boundingbox);
}

void scenenode_hide_boundingbox(SceneNodeHandle scenenode_handle, int hide_boundingbox)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->hideBoundingBox(hide_boundingbox);
}

int scenenode_get_show_boundingbox(SceneNodeHandle scenenode_handle)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    if(scene_node->getShowBoundingBox())
        return 1;
    return 0;
}

SceneNodeHandle scenenode_get_parent_scenenode(SceneNodeHandle scenenode_handle)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    return reinterpret_cast<SceneNodeHandle>(scene_node->getParentSceneNode());
}

void scenenode_set_visible(SceneNodeHandle scenenode_handle, int visible)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->setVisible(visible);
}

void scenenode_set_visible_ex(SceneNodeHandle scenenode_handle, int visible, int cascade)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->setVisible(visible, cascade);
}

void scenenode_flip_visibility(SceneNodeHandle scenenode_handle)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->flipVisibility();
}

void scenenode_flip_visibility_ex(SceneNodeHandle scenenode_handle, int cascade)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->flipVisibility(cascade);
}

void scenenode_set_debug_display_enabled(SceneNodeHandle scenenode_handle, int enabled)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->setDebugDisplayEnabled(enabled);
}

void scenenode_set_debug_display_enabled_ex(SceneNodeHandle scenenode_handle, int enabled, int cascade)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->setDebugDisplayEnabled(enabled, cascade);
}

SceneManagerHandle scenenode_get_creator(SceneNodeHandle scenenode_handle)
{
    Ogre::SceneManager* scene_manager = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle)->getCreator();
    return reinterpret_cast<SceneManagerHandle>(scene_manager);
}

void scenenode_set_direction(SceneNodeHandle scenenode_handle, float x, float y, float z)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->setDirection(x, y, z);
}

void scenenode_set_orientation(SceneNodeHandle scenenode_handle, float w, float x, float y, float z)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->setOrientation(w, x, y, z);
}

void scenenode_set_position(SceneNodeHandle scenenode_handle, float x, float y, float z)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->setPosition(x, y, z);
}

void scenenode_yaw(SceneNodeHandle scenenode_handle, coiReal radians)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->yaw(Ogre::Radian(radians));
}

void scenenode_set_scale(SceneNodeHandle scenenode_handle, float x, float y, float z)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->setScale(x, y, z);
}

void scenenode_scale(SceneNodeHandle scenenode_handle, float x, float y, float z)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->scale(x, y, z);
}

void scenenode_translate(SceneNodeHandle scenenode_handle, float x, float y, float z)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->translate(x, y, z);
}

void scenenode_roll(SceneNodeHandle scenenode_handle, coiReal radians)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->roll(Ogre::Radian(radians));
}

void scenenode_pitch(SceneNodeHandle scenenode_handle, coiReal radians)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->pitch(Ogre::Radian(radians));
}

/*
Ogre::SceneNode::operator=(Ogre::SceneNode const&)
Ogre::SceneNode::SceneNode(Ogre::SceneNode const&)
Ogre::SceneNode::SceneNode(Ogre::SceneManager*)
Ogre::SceneNode::SceneNode(Ogre::SceneManager*, std::string const&)
Ogre::SceneNode::~SceneNode()
Ogre::SceneNode::_findVisibleObjects(Ogre::Camera*, Ogre::RenderQueue*, Ogre::VisibleObjectsBoundsInfo*, bool, bool, bool)
Ogre::SceneNode::_getWorldAABB() const
Ogre::SceneNode::getAttachedObjectIterator()
Ogre::SceneNode::getAttachedObjectIterator() const
Ogre::SceneNode::removeAndDestroyChild(std::string const&)
Ogre::SceneNode::removeAndDestroyChild(unsigned short)
Ogre::SceneNode::removeAndDestroyAllChildren()
Ogre::SceneNode::_addBoundingBoxToQueue(Ogre::RenderQueue*)
Ogre::SceneNode::findLights(Ogre::HashedVector<Ogre::Light*>&, float, unsigned int) const
Ogre::SceneNode::setFixedYawAxis(bool, Ogre::Vector3 const&)
Ogre::SceneNode::setDirection(Ogre::Vector3 const&, Ogre::Node::TransformSpace, Ogre::Vector3 const&)
Ogre::SceneNode::lookAt(Ogre::Vector3 const&, Ogre::Node::TransformSpace, Ogre::Vector3 const&)
Ogre::SceneNode::setAutoTracking(bool, Ogre::SceneNode*, Ogre::Vector3 const&, Ogre::Vector3 const&)
Ogre::SceneNode::getAutoTrackTarget()
Ogre::SceneNode::getAutoTrackOffset()
Ogre::SceneNode::getAutoTrackLocalDirection()
Ogre::SceneNode::_autoTrack()
Ogre::SceneNode::getDebugRenderable()
*/
/*
Ogre::Node::Listener
Ogre::Node::DebugRenderable
Ogre::Node::operator=(Ogre::Node const&)
Ogre::Node::Node(Ogre::Node const&)
Ogre::Node::Node()
Ogre::Node::Node(std::string const&)
Ogre::Node::~Node()
Ogre::Node::getName() const
Ogre::Node::getParent() const
Ogre::Node::getOrientation() const
Ogre::Node::setOrientation(Ogre::Quaternion const&)
Ogre::Node::resetOrientation()
Ogre::Node::setPosition(Ogre::Vector3 const&)
Ogre::Node::getPosition() const
Ogre::Node::setScale(Ogre::Vector3 const&)
Ogre::Node::getScale() const
Ogre::Node::setInheritOrientation(bool)
Ogre::Node::getInheritOrientation() const
Ogre::Node::setInheritScale(bool)
Ogre::Node::getInheritScale() const
Ogre::Node::scale(Ogre::Vector3 const&)
Ogre::Node::translate(Ogre::Vector3 const&, Ogre::Node::TransformSpace)
Ogre::Node::translate(Ogre::Matrix3 const&, Ogre::Vector3 const&, Ogre::Node::TransformSpace)
Ogre::Node::translate(Ogre::Matrix3 const&, float, float, float, Ogre::Node::TransformSpace)
Ogre::Node::rotate(Ogre::Vector3 const&, Ogre::Radian const&, Ogre::Node::TransformSpace)
Ogre::Node::rotate(Ogre::Quaternion const&, Ogre::Node::TransformSpace)
Ogre::Node::getLocalAxes() const
Ogre::Node::createChild(Ogre::Vector3 const&, Ogre::Quaternion const&)
Ogre::Node::createChild(std::string const&, Ogre::Vector3 const&, Ogre::Quaternion const&)
Ogre::Node::addChild(Ogre::Node*)
Ogre::Node::numChildren() const
Ogre::Node::getChild(unsigned short) const
Ogre::Node::getChild(std::string const&) const
Ogre::Node::getChildIterator()
Ogre::Node::getChildIterator() const
Ogre::Node::removeChild(unsigned short)
Ogre::Node::removeChild(Ogre::Node*)
Ogre::Node::removeChild(std::string const&)
Ogre::Node::removeAllChildren()
Ogre::Node::_setDerivedPosition(Ogre::Vector3 const&)
Ogre::Node::_setDerivedOrientation(Ogre::Quaternion const&)
Ogre::Node::_getDerivedOrientation() const
Ogre::Node::_getDerivedPosition() const
Ogre::Node::_getDerivedScale() const
Ogre::Node::_getFullTransform() const
Ogre::Node::_update(bool, bool)
Ogre::Node::setListener(Ogre::Node::Listener*)
Ogre::Node::getListener() const
Ogre::Node::setInitialState()
Ogre::Node::resetToInitialState()
Ogre::Node::getInitialPosition() const
Ogre::Node::convertWorldToLocalPosition(Ogre::Vector3 const&)
Ogre::Node::convertLocalToWorldPosition(Ogre::Vector3 const&)
Ogre::Node::convertWorldToLocalOrientation(Ogre::Quaternion const&)
Ogre::Node::convertLocalToWorldOrientation(Ogre::Quaternion const&)
Ogre::Node::getInitialOrientation() const
Ogre::Node::getInitialScale() const
Ogre::Node::getSquaredViewDepth(Ogre::Camera const*) const
Ogre::Node::needUpdate(bool)
Ogre::Node::requestUpdate(Ogre::Node*, bool)
Ogre::Node::cancelUpdate(Ogre::Node*)
Ogre::Node::getDebugRenderable(float)
Ogre::Node::queueNeedUpdate(Ogre::Node*)
Ogre::Node::processQueuedUpdates()
Ogre::Node::setUserAny(Ogre::Any const&)
Ogre::Node::getUserAny() const
Ogre::Node::getUserObjectBindings()
Ogre::Node::getUserObjectBindings() const
Ogre::Node::Listener::operator=(Ogre::Node::Listener const&)
Ogre::Node::Listener::Listener(Ogre::Node::Listener const&)
Ogre::Node::Listener::Listener()
Ogre::Node::Listener::~Listener()
Ogre::Node::Listener::nodeUpdated(Ogre::Node const*)
Ogre::Node::Listener::nodeDestroyed(Ogre::Node const*)
Ogre::Node::Listener::nodeAttached(Ogre::Node const*)
Ogre::Node::Listener::nodeDetached(Ogre::Node const*)
Ogre::Node::DebugRenderable::operator=(Ogre::Node::DebugRenderable const&)
Ogre::Node::DebugRenderable::DebugRenderable(Ogre::Node::DebugRenderable const&)
Ogre::Node::DebugRenderable::DebugRenderable(Ogre::Node*)
Ogre::Node::DebugRenderable::~DebugRenderable()
Ogre::Node::DebugRenderable::getMaterial() const
Ogre::Node::DebugRenderable::getRenderOperation(Ogre::RenderOperation&)
Ogre::Node::DebugRenderable::getWorldTransforms(Ogre::Matrix4*) const
Ogre::Node::DebugRenderable::getSquaredViewDepth(Ogre::Camera const*) const
Ogre::Node::DebugRenderable::getLights() const
Ogre::Node::DebugRenderable::setScaling(float)
*/
