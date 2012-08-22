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
#include "scenenode_bind.h"
#include "binding_utils.h"

#include <OgreRoot.h>
#include <OgreSceneNode.h>
#include <OgreMovableObject.h>
#include <OgreEntity.h>
#include "ogre_manager.h"

// Maybe this would be enough? One could set position and orientation afterwards..
// Ogre::SceneNode::createChildSceneNode(Ogre::Vector3 const&, Ogre::Quaternion const&)
// Ogre::SceneNode::createChildSceneNode(std::string const&, Ogre::Vector3 const&, Ogre::Quaternion const&)
SceneNodeHandle create_child_scenenode(const char* node_name)
{
    Ogre::SceneNode* scene_node = Ogre::Root::getSingletonPtr()->getSceneManager(OgreManager::getSingletonPtr()->get_active_scene_manager_name())->getRootSceneNode()->createChildSceneNode(node_name);
    return static_cast<SceneNodeHandle>(scene_node);
}

// Ogre::SceneNode::attachObject(Ogre::MovableObject*)
void attach_entity_to_scenenode(EntityHandle entity_handle, SceneNodeHandle scenenode_handle)
{
    Ogre::Entity* entity = static_cast<Ogre::Entity*>(entity_handle);
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->attachObject(entity);
}

// Ogre::SceneNode::_update(bool, bool)
void scenenode_update(SceneNodeHandle scenenode_handle, int update_children, int parent_has_changed)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->_update(update_children, parent_has_changed);
}

// Ogre::SceneNode::_updateBounds()
void scenenode_update_bounds(SceneNodeHandle scenenode_handle)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->_updateBounds();
}

// Ogre::SceneNode::getAttachedObject(unsigned short)
EntityHandle scenenode_get_attached_entity_int(SceneNodeHandle scenenode_handle, int entity_index)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    Ogre::Entity* entity = static_cast<Ogre::Entity*>(scene_node->getAttachedObject(entity_index));
    return static_cast<EntityHandle>(entity);
}

// Ogre::SceneNode::getAttachedObject(std::string const&)
EntityHandle scenenode_get_attached_entity(SceneNodeHandle scenenode_handle, const char* entity_name)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    Ogre::Entity* entity = static_cast<Ogre::Entity*>(scene_node->getAttachedObject(entity_name));
    return static_cast<EntityHandle>(entity);
}

//Ogre::SceneNode::numAttachedObjects() const
unsigned short scenenode_num_attached_objects(SceneNodeHandle scenenode_handle)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    return scene_node->numAttachedObjects();
}

// Ogre::SceneNode::detachObject(unsigned short)
void scenenode_detach_entity_int(SceneNodeHandle scenenode_handle, int entity_index)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->detachObject(entity_index);
}

// Ogre::SceneNode::detachObject(Ogre::MovableObject*)
void scenenode_detach_entity(SceneNodeHandle scenenode_handle, EntityHandle entity_handle)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    Ogre::Entity* entity = static_cast<Ogre::Entity*>(entity_handle);
    scene_node->detachObject(entity);
}

// Ogre::SceneNode::detachObject(std::string const&)
void scenenode_detach_entity_string(SceneNodeHandle scenenode_handle, const char* entity_name)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->detachObject(entity_name);
}

// Ogre::SceneNode::detachAllObjects()
void scenenode_detach_all_objects(SceneNodeHandle scenenode_handle)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->detachAllObjects();
}

// Ogre::SceneNode::isInSceneGraph() const
int scenenode_is_in_scenegraph(SceneNodeHandle scenenode_handle)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    if(scene_node->isInSceneGraph())
        return 1;
    return 0;
}

// Ogre::SceneNode::_notifyRootNode()
void scenenode_notify_rootnode(SceneNodeHandle scenenode_handle)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->_notifyRootNode();
}

// Ogre::SceneNode::showBoundingBox(bool)
void scenenode_show_boundingbox(SceneNodeHandle scenenode_handle, int show_boundingbox)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->showBoundingBox(show_boundingbox);
}

// Ogre::SceneNode::hideBoundingBox(bool)
void scenenode_hide_boundingbox(SceneNodeHandle scenenode_handle, int hide_boundingbox)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->hideBoundingBox(hide_boundingbox);
}

// Ogre::SceneNode::getShowBoundingBox() const
int scenenode_get_show_boundingbox(SceneNodeHandle scenenode_handle)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    return scene_node->getShowBoundingBox();
}

// Ogre::SceneNode::getParentSceneNode() const
SceneNodeHandle scenenode_get_parent_scenenode(SceneNodeHandle scenenode_handle)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    return static_cast<SceneNodeHandle>(scene_node->getParentSceneNode());
}

// Ogre::SceneNode::setVisible(bool, bool)
void scenenode_set_visible(SceneNodeHandle scenenode_handle, int visible)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->setVisible(visible);
}

// Ogre::SceneNode::setVisible(bool, bool)
void scenenode_set_visible_ex(SceneNodeHandle scenenode_handle, int visible, int cascade)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->setVisible(visible, cascade);
}

// Ogre::SceneNode::flipVisibility(bool)
void scenenode_flip_visibility(SceneNodeHandle scenenode_handle)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->flipVisibility();
}

// Ogre::SceneNode::flipVisibility(bool)
void scenenode_flip_visibility_ex(SceneNodeHandle scenenode_handle, int cascade)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->flipVisibility(cascade);
}

// Ogre::SceneNode::setDebugDisplayEnabled(bool, bool)
void scenenode_set_debug_display_enabled(SceneNodeHandle scenenode_handle, int enabled)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->setDebugDisplayEnabled(enabled);
}

// Ogre::SceneNode::setDebugDisplayEnabled(bool, bool)
void scenenode_set_debug_display_enabled_ex(SceneNodeHandle scenenode_handle, int enabled, int cascade)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->setDebugDisplayEnabled(enabled, cascade);
}

// Ogre::SceneNode::getCreator() const
SceneManagerHandle scenenode_get_creator(SceneNodeHandle scenenode_handle)
{
    Ogre::SceneManager* scene_manager = static_cast<Ogre::SceneNode*>(scenenode_handle)->getCreator();
    return static_cast<SceneManagerHandle>(scene_manager);
}

// Ogre::SceneNode::setDirection(float, float, float, Ogre::Node::TransformSpace, Ogre::Vector3 const&)
void scenenode_set_direction(SceneNodeHandle scenenode_handle, float x, float y, float z, transform_space relative_to)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    Ogre::Node::TransformSpace ts = llcoi_ts_to_ogre_ts(relative_to);
    scene_node->setDirection(x, y, z,ts);
}

// Ogre::Node::setOrientation(float, float, float, float)
void scenenode_set_orientation(SceneNodeHandle scenenode_handle, float w, float x, float y, float z)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->setOrientation(w, x, y, z);
}

//Ogre::Node::setPosition(float, float, float)
void scenenode_set_position(SceneNodeHandle scenenode_handle, float x, float y, float z)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->setPosition(x, y, z);
}

//Ogre::Node::getPosition() const
void scenenode_get_position(SceneNodeHandle handle, coiVector3* pos)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(handle);
    const Ogre::Vector3& v = scene_node->getPosition();

    pos->x = v.x;
    pos->y = v.y;
    pos->z = v.z;
}

//Ogre::Node::_getDerivedPosition() const
void scenenode_get_derived_position(SceneNodeHandle handle, coiVector3* pos)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(handle);
    const Ogre::Vector3& v = scene_node->_getDerivedPosition();
    pos->x = v.x;
    pos->y = v.y;
    pos->z = v.z;
}

// Ogre::Node::_setDerivedPosition(Ogre::Vector3 const&)
void scenenode_set_derived_position(SceneNodeHandle handle, const coiVector3* pos)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(handle);
    const Ogre::Vector3 v(pos->x, pos->y, pos->z);
    scene_node->_setDerivedPosition(v);
}

// Ogre::SceneNode::yaw(Ogre::Radian const&, Ogre::Node::TransformSpace)
// Ogre::Node::yaw(Ogre::Radian const&, Ogre::Node::TransformSpace)
void scenenode_yaw(SceneNodeHandle scenenode_handle, coiReal radians, transform_space relative_to)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    Ogre::Node::TransformSpace ts = llcoi_ts_to_ogre_ts(relative_to);
    scene_node->yaw(Ogre::Radian(radians));
}

// Ogre::Node::setScale(float, float, float)
void scenenode_set_scale(SceneNodeHandle scenenode_handle, float x, float y, float z)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->setScale(x, y, z);
}

// Ogre::Node::scale(float, float, float)
void scenenode_scale(SceneNodeHandle scenenode_handle, float x, float y, float z)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    scene_node->scale(x, y, z);
}

// Ogre::Node::translate(float, float, float, Ogre::Node::TransformSpace)
void scenenode_translate(SceneNodeHandle scenenode_handle, float x, float y, float z, transform_space relative_to)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    Ogre::Node::TransformSpace ts = llcoi_ts_to_ogre_ts(relative_to);
    scene_node->translate(x, y, z, ts);
}

// Ogre::Node::roll(Ogre::Radian const&, Ogre::Node::TransformSpace)
void scenenode_roll(SceneNodeHandle scenenode_handle, coiReal radians, transform_space relative_to)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    Ogre::Node::TransformSpace ts = llcoi_ts_to_ogre_ts(relative_to);
    scene_node->roll(Ogre::Radian(radians), ts);
}

// Ogre::Node::pitch(Ogre::Radian const&, Ogre::Node::TransformSpace)
void scenenode_pitch(SceneNodeHandle scenenode_handle, coiReal radians, transform_space relative_to)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    Ogre::Node::TransformSpace ts = llcoi_ts_to_ogre_ts(relative_to);
    scene_node->pitch(Ogre::Radian(radians), ts);
}

SceneNodeHandle scenenode_create_child_scenenode(SceneNodeHandle parent_handle, const char* name, const coiVector3* translate, const coiQuaternion* rotate)
{
    Ogre::SceneNode* parent = static_cast<Ogre::SceneNode*>(parent_handle);
    const Ogre::Vector3 trans(translate->x, translate->y, translate->z);
    const Ogre::Quaternion rot(rotate->w, rotate->x, rotate->y, rotate->z);

    Ogre::SceneNode* child = parent->createChildSceneNode(Ogre::String(name), trans, rot);
    return static_cast<SceneNodeHandle>(child);
}

void scenenode_yaw_degree(SceneNodeHandle handle, float angle, transform_space relative_to)
{
    Ogre::SceneNode* scene_node = static_cast<Ogre::SceneNode*>(handle);
    Ogre::Node::TransformSpace ts = llcoi_ts_to_ogre_ts(relative_to);
    scene_node->yaw(Ogre::Degree(angle));
}

//Ogre::SceneNode::setFixedYawAxis(bool, Ogre::Vector3 const&)
void scenenode_set_fixed_yaw_axis(SceneNodeHandle handle, int use_fixed, const coiVector3* fixed_axis)
{
    Ogre::SceneNode* sn = static_cast<Ogre::SceneNode*>(handle);
    Ogre::Vector3 setter(fixed_axis->x, fixed_axis->y, fixed_axis->z);
    sn->setFixedYawAxis(use_fixed, setter);
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
Ogre::SceneNode::setDirection(Ogre::Vector3 const&, Ogre::Node::TransformSpace, Ogre::Vector3 const&)
Ogre::SceneNode::lookAt(Ogre::Vector3 const&, Ogre::Node::TransformSpace, Ogre::Vector3 const&)
Ogre::SceneNode::setAutoTracking(bool, Ogre::SceneNode*, Ogre::Vector3 const&, Ogre::Vector3 const&)
Ogre::SceneNode::getAutoTrackTarget()
Ogre::SceneNode::getAutoTrackOffset()
Ogre::SceneNode::getAutoTrackLocalDirection()
Ogre::SceneNode::_autoTrack()
Ogre::SceneNode::getDebugRenderable()
*/
