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
#include <OgreMovableObject.h>
#include <OgreEntity.h>
#include "ogre_manager.h"

// SceneNode is going to carry Node's code - we'll duplicate for Bone

//TODO: need to have a function which translates to/from Ogre::Node::TransformSpace


// Maybe this would be enough? One could set position and orientation afterwards..
// Ogre::SceneNode::createChildSceneNode(Ogre::Vector3 const&, Ogre::Quaternion const&)
// Ogre::SceneNode::createChildSceneNode(std::string const&, Ogre::Vector3 const&, Ogre::Quaternion const&)
SceneNodeHandle create_child_scenenode(const char* node_name)
{
    Ogre::SceneNode* scene_node = Ogre::Root::getSingletonPtr()->getSceneManager(OgreManager::getSingletonPtr()->get_active_scene_manager_name())->getRootSceneNode()->createChildSceneNode(node_name);
    return reinterpret_cast<SceneNodeHandle>(scene_node);
}

// Ogre::SceneNode::attachObject(Ogre::MovableObject*)
void attach_entity_to_scenenode(EntityHandle entity_handle, SceneNodeHandle scenenode_handle)
{
    Ogre::Entity* entity = reinterpret_cast<Ogre::Entity*>(entity_handle);
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->attachObject(entity);
}

// Ogre::SceneNode::_update(bool, bool)
void scenenode_update(SceneNodeHandle scenenode_handle, int update_children, int parent_has_changed)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->_update(update_children, parent_has_changed);
}

// Ogre::SceneNode::_updateBounds()
void scenenode_update_bounds(SceneNodeHandle scenenode_handle)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->_updateBounds();
}

// Ogre::SceneNode::getAttachedObject(unsigned short)
EntityHandle scenenode_get_attached_entity_int(SceneNodeHandle scenenode_handle, int entity_index)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    Ogre::Entity* entity = static_cast<Ogre::Entity*>(scene_node->getAttachedObject(entity_index));
    return reinterpret_cast<EntityHandle>(entity);
}

// Ogre::SceneNode::getAttachedObject(std::string const&)
EntityHandle scenenode_get_attached_entity(SceneNodeHandle scenenode_handle, const char* entity_name)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    Ogre::Entity* entity = static_cast<Ogre::Entity*>(scene_node->getAttachedObject(entity_name));
    return reinterpret_cast<EntityHandle>(entity);
}

//Ogre::SceneNode::numAttachedObjects() const
int scenenode_num_attached_objects(SceneNodeHandle scenenode_handle)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    return scene_node->numAttachedObjects();
}

// Ogre::SceneNode::detachObject(unsigned short)
void scenenode_detach_entity_int(SceneNodeHandle scenenode_handle, int entity_index)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->detachObject(entity_index);
}

// Ogre::SceneNode::detachObject(Ogre::MovableObject*)
void scenenode_detach_entity(SceneNodeHandle scenenode_handle, EntityHandle entity_handle)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    Ogre::Entity* entity = reinterpret_cast<Ogre::Entity*>(entity_handle);
    scene_node->detachObject(entity);
}

// Ogre::SceneNode::detachObject(std::string const&)
void scenenode_detach_entity_string(SceneNodeHandle scenenode_handle, const char* entity_name)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->detachObject(entity_name);
}

// Ogre::SceneNode::detachAllObjects()
void scenenode_detach_all_objects(SceneNodeHandle scenenode_handle)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->detachAllObjects();
}

// Ogre::SceneNode::isInSceneGraph() const
int scenenode_is_in_scenegraph(SceneNodeHandle scenenode_handle)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    if(scene_node->isInSceneGraph())
        return 1;
    return 0;
}

// Ogre::SceneNode::_notifyRootNode()
void scenenode_notify_rootnode(SceneNodeHandle scenenode_handle)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->_notifyRootNode();
}

// Ogre::SceneNode::showBoundingBox(bool)
void scenenode_show_boundingbox(SceneNodeHandle scenenode_handle, int show_boundingbox)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->showBoundingBox(show_boundingbox);
}

// Ogre::SceneNode::hideBoundingBox(bool)
void scenenode_hide_boundingbox(SceneNodeHandle scenenode_handle, int hide_boundingbox)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->hideBoundingBox(hide_boundingbox);
}

// Ogre::SceneNode::getShowBoundingBox() const
int scenenode_get_show_boundingbox(SceneNodeHandle scenenode_handle)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    if(scene_node->getShowBoundingBox())
        return 1;
    return 0;
}

// Ogre::SceneNode::getParentSceneNode() const
SceneNodeHandle scenenode_get_parent_scenenode(SceneNodeHandle scenenode_handle)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    return reinterpret_cast<SceneNodeHandle>(scene_node->getParentSceneNode());
}

// Ogre::SceneNode::setVisible(bool, bool)
void scenenode_set_visible(SceneNodeHandle scenenode_handle, int visible)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->setVisible(visible);
}

// Ogre::SceneNode::setVisible(bool, bool)
void scenenode_set_visible_ex(SceneNodeHandle scenenode_handle, int visible, int cascade)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->setVisible(visible, cascade);
}

// Ogre::SceneNode::flipVisibility(bool)
void scenenode_flip_visibility(SceneNodeHandle scenenode_handle)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->flipVisibility();
}

// Ogre::SceneNode::flipVisibility(bool)
void scenenode_flip_visibility_ex(SceneNodeHandle scenenode_handle, int cascade)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->flipVisibility(cascade);
}

// Ogre::SceneNode::setDebugDisplayEnabled(bool, bool)
void scenenode_set_debug_display_enabled(SceneNodeHandle scenenode_handle, int enabled)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->setDebugDisplayEnabled(enabled);
}

// Ogre::SceneNode::setDebugDisplayEnabled(bool, bool)
void scenenode_set_debug_display_enabled_ex(SceneNodeHandle scenenode_handle, int enabled, int cascade)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->setDebugDisplayEnabled(enabled, cascade);
}

// Ogre::SceneNode::getCreator() const
SceneManagerHandle scenenode_get_creator(SceneNodeHandle scenenode_handle)
{
    Ogre::SceneManager* scene_manager = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle)->getCreator();
    return reinterpret_cast<SceneManagerHandle>(scene_manager);
}

// Ogre::SceneNode::setDirection(float, float, float, Ogre::Node::TransformSpace, Ogre::Vector3 const&)
void scenenode_set_direction(SceneNodeHandle scenenode_handle, float x, float y, float z)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->setDirection(x, y, z);
}

// Ogre::Node::setOrientation(float, float, float, float)
void scenenode_set_orientation(SceneNodeHandle scenenode_handle, float w, float x, float y, float z)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->setOrientation(w, x, y, z);
}

//Ogre::Node::setPosition(float, float, float)
void scenenode_set_position(SceneNodeHandle scenenode_handle, float x, float y, float z)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->setPosition(x, y, z);
}

// Ogre::SceneNode::yaw(Ogre::Radian const&, Ogre::Node::TransformSpace)
// Ogre::Node::yaw(Ogre::Radian const&, Ogre::Node::TransformSpace)
void scenenode_yaw(SceneNodeHandle scenenode_handle, coiReal radians)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->yaw(Ogre::Radian(radians));
}

// Ogre::Node::setScale(float, float, float)
void scenenode_set_scale(SceneNodeHandle scenenode_handle, float x, float y, float z)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->setScale(x, y, z);
}

// Ogre::Node::scale(float, float, float)
void scenenode_scale(SceneNodeHandle scenenode_handle, float x, float y, float z)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->scale(x, y, z);
}

// Ogre::Node::translate(float, float, float, Ogre::Node::TransformSpace)
void scenenode_translate(SceneNodeHandle scenenode_handle, float x, float y, float z)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->translate(x, y, z);
}

// Ogre::Node::roll(Ogre::Radian const&, Ogre::Node::TransformSpace)
void scenenode_roll(SceneNodeHandle scenenode_handle, coiReal radians)
{
    Ogre::SceneNode* scene_node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->roll(Ogre::Radian(radians));
}

// Ogre::Node::pitch(Ogre::Radian const&, Ogre::Node::TransformSpace)
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
