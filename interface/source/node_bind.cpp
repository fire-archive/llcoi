/******************************************************************************
 * node_bind.cpp - bindings for Ogre::Node
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

#include "node_bind.h"
#include <OgreNode.h>

//Ogre::Node::getName() const
const char* node_get_name(NodeHandle handle)
{
    Ogre::Node* node = reinterpret_cast<Ogre::Node*>(handle);
    return node->getName().c_str();
}

//Ogre::Node::getParent() const
NodeHandle node_get_parent(NodeHandle handle)
{
    Ogre::Node* node = reinterpret_cast<Ogre::Node*>(handle);

    // N.B. May be NULL if this is the root node.
    return reinterpret_cast<NodeHandle>(
        node->getParent()
    );
}

//Ogre::Node::getOrientation() const
void node_get_orientation(NodeHandle handle, coiQuaternion* q)
{
    Ogre::Node* node = reinterpret_cast<Ogre::Node*>(handle);
    const Ogre::Quaternion& getter = node->getOrientation();

    q->w = getter.w;
    q->x = getter.x;
    q->y = getter.y;
    q->z = getter.z;
}

//Ogre::Node::setOrientation(Ogre::Quaternion const&)
void node_set_orientation(NodeHandle handle, const coiQuaternion* o)
{
    Ogre::Node* node = reinterpret_cast<Ogre::Node*>(handle);
    const Ogre::Quaternion q(o->w, o->x, o->y, o->z);
    node->setOrientation(q);
}

/*
//Ogre::Node::Listener
//Ogre::Node::DebugRenderable
//Ogre::Node::operator=(Ogre::Node const&)
//Ogre::Node::Node(Ogre::Node const&)
//Ogre::Node::Node()
//Ogre::Node::Node(std::string const&)
//Ogre::Node::~Node()
//Ogre::Node::resetOrientation()
//Ogre::Node::setScale(Ogre::Vector3 const&)
//Ogre::Node::getScale() const
//Ogre::Node::setInheritOrientation(bool)
//Ogre::Node::getInheritOrientation() const
//Ogre::Node::setInheritScale(bool)
//Ogre::Node::getInheritScale() const
//Ogre::Node::scale(Ogre::Vector3 const&)
//Ogre::Node::translate(Ogre::Vector3 const&, Ogre::Node::TransformSpace)
//Ogre::Node::translate(Ogre::Matrix3 const&, Ogre::Vector3 const&, Ogre::Node::TransformSpace)
//Ogre::Node::translate(Ogre::Matrix3 const&, float, float, float, Ogre::Node::TransformSpace)
//Ogre::Node::rotate(Ogre::Vector3 const&, Ogre::Radian const&, Ogre::Node::TransformSpace)
//Ogre::Node::rotate(Ogre::Quaternion const&, Ogre::Node::TransformSpace)
//Ogre::Node::getLocalAxes() const
//Ogre::Node::createChild(Ogre::Vector3 const&, Ogre::Quaternion const&)
//Ogre::Node::createChild(std::string const&, Ogre::Vector3 const&, Ogre::Quaternion const&)
//Ogre::Node::addChild(Ogre::Node*)
//Ogre::Node::numChildren() const
//Ogre::Node::getChild(unsigned short) const
//Ogre::Node::getChild(std::string const&) const
//Ogre::Node::getChildIterator()
//Ogre::Node::getChildIterator() const
//Ogre::Node::removeChild(unsigned short)
//Ogre::Node::removeChild(Ogre::Node*)
//Ogre::Node::removeChild(std::string const&)
//Ogre::Node::removeAllChildren()
//Ogre::Node::_setDerivedPosition(Ogre::Vector3 const&)
//Ogre::Node::_setDerivedOrientation(Ogre::Quaternion const&)
//Ogre::Node::_getDerivedOrientation() const
//Ogre::Node::_getDerivedPosition() const
//Ogre::Node::_getDerivedScale() const
//Ogre::Node::_getFullTransform() const
//Ogre::Node::_update(bool, bool)
//Ogre::Node::setListener(Ogre::Node::Listener*)
//Ogre::Node::getListener() const
//Ogre::Node::setInitialState()
//Ogre::Node::resetToInitialState()
//Ogre::Node::getInitialPosition() const
//Ogre::Node::convertWorldToLocalPosition(Ogre::Vector3 const&)
//Ogre::Node::convertLocalToWorldPosition(Ogre::Vector3 const&)
//Ogre::Node::convertWorldToLocalOrientation(Ogre::Quaternion const&)
//Ogre::Node::convertLocalToWorldOrientation(Ogre::Quaternion const&)
//Ogre::Node::getInitialOrientation() const
//Ogre::Node::getInitialScale() const
//Ogre::Node::getSquaredViewDepth(Ogre::Camera const*) const
//Ogre::Node::needUpdate(bool)
//Ogre::Node::requestUpdate(Ogre::Node*, bool)
//Ogre::Node::cancelUpdate(Ogre::Node*)
//Ogre::Node::getDebugRenderable(float)
//Ogre::Node::queueNeedUpdate(Ogre::Node*)
//Ogre::Node::processQueuedUpdates()
//Ogre::Node::setUserAny(Ogre::Any const&)
//Ogre::Node::getUserAny() const
//Ogre::Node::getUserObjectBindings()
//Ogre::Node::getUserObjectBindings() const
//Ogre::Node::Listener::operator=(Ogre::Node::Listener const&)
//Ogre::Node::Listener::Listener(Ogre::Node::Listener const&)
//Ogre::Node::Listener::Listener()
//Ogre::Node::Listener::~Listener()
//Ogre::Node::Listener::nodeUpdated(Ogre::Node const*)
//Ogre::Node::Listener::nodeDestroyed(Ogre::Node const*)
//Ogre::Node::Listener::nodeAttached(Ogre::Node const*)
//Ogre::Node::Listener::nodeDetached(Ogre::Node const*)
//Ogre::Node::DebugRenderable::operator=(Ogre::Node::DebugRenderable const&)
//Ogre::Node::DebugRenderable::DebugRenderable(Ogre::Node::DebugRenderable const&)
//Ogre::Node::DebugRenderable::DebugRenderable(Ogre::Node*)
//Ogre::Node::DebugRenderable::~DebugRenderable()
//Ogre::Node::DebugRenderable::getMaterial() const
//Ogre::Node::DebugRenderable::getRenderOperation(Ogre::RenderOperation&)
//Ogre::Node::DebugRenderable::getWorldTransforms(Ogre::Matrix4*) const
//Ogre::Node::DebugRenderable::getSquaredViewDepth(Ogre::Camera const*) const
//Ogre::Node::DebugRenderable::getLights() const
//Ogre::Node::DebugRenderable::setScaling(float)
*/
