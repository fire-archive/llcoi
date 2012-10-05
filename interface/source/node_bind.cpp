/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "node_bind.h"
#include "binding_utils.h"
#include <OgreNode.h>

//Ogre::Node::getName() const
const char* node_get_name(NodeHandle handle)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    return node->getName().c_str();
}

//Ogre::Node::getParent() const
NodeHandle node_get_parent(NodeHandle handle)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);

    // N.B. May be NULL if this is the root node.
    return static_cast<NodeHandle>(
        node->getParent()
    );
}

//Ogre::Node::getOrientation() const
void node_get_orientation(NodeHandle handle, coiQuaternion* q)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    const Ogre::Quaternion& getter = node->getOrientation();

    q->w = getter.w;
    q->x = getter.x;
    q->y = getter.y;
    q->z = getter.z;
}

//Ogre::Node::setOrientation(Ogre::Quaternion const&)
void node_set_orientation(NodeHandle handle, const coiQuaternion* o)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    const Ogre::Quaternion q(o->w, o->x, o->y, o->z);
    node->setOrientation(q);
}

//Ogre::Node::setScale(Ogre::Vector3 const&)
void node_set_scale(NodeHandle handle, const coiVector3* in_scale)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    const Ogre::Vector3 scale(in_scale->x, in_scale->y, in_scale->z);
    node->setScale(scale);
}

//Ogre::Node::setScale(Ogre::Vector3 const&)
void node_set_scale_xyz(NodeHandle handle, const float x, const float y, const float z)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    const Ogre::Vector3 scale(x, y, z);
    node->setScale(scale);
}

//Ogre::Node::getScale() const
void node_get_scale(NodeHandle handle, coiVector3* r)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    const Ogre::Vector3& result = node->getScale();

    r->x = result.x;
    r->y = result.y;
    r->z = result.z;
}

//Ogre::Node::setInheritOrientation(bool)
void node_set_inherit_orientation(NodeHandle handle, int inherit)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    node->setInheritOrientation(inherit);
}

//Ogre::Node::getInheritOrientation() const
int node_get_inherit_orientation(NodeHandle handle)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    return node->getInheritOrientation();
}

//Ogre::Node::resetOrientation()
void node_reset_orientation(NodeHandle handle)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    node->resetOrientation();
}

//Ogre::Node::setInheritScale(bool)
void node_set_inherit_scale(NodeHandle handle, int inherit)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    node->setInheritScale(inherit);
}

//Ogre::Node::getInheritScale() const
int node_get_inherit_scale(NodeHandle handle)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    return node->getInheritScale();
}

//Ogre::Node::scale(Ogre::Vector3 const&)
void node_scale(NodeHandle handle, const coiVector3* scale)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    const Ogre::Vector3 s(scale->x, scale->y, scale->z);
    node->scale(s);
}

void node_scale_xyz(NodeHandle handle, const float x, const float y, const float z)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    const Ogre::Vector3 scale(x, y, z);
    node->scale(scale);
}

//Ogre::Node::translate(Ogre::Vector3 const&, Ogre::Node::TransformSpace)
void node_translate(NodeHandle handle, const coiVector3* d, transform_space relative_to)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    const Ogre::Vector3 direction(d->x, d->y, d->z);
    Ogre::Node::TransformSpace rt = llcoi_ts_to_ogre_ts(relative_to);

    node->translate(direction, rt);
}

//Ogre::Node::translate(Ogre::Matrix3 const&, float, float, float, Ogre::Node::TransformSpace)
void node_translate_xyz(NodeHandle handle, const float x, const float y, const float z, transform_space relative_to)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    const Ogre::Vector3 direction(x, y, z);
    Ogre::Node::TransformSpace rt = llcoi_ts_to_ogre_ts(relative_to);

    node->translate(direction, rt);
}

//Ogre::Node::translate(Ogre::Matrix3 const&, Ogre::Vector3 const&, Ogre::Node::TransformSpace)
void node_translate_m(NodeHandle handle, const coiMatrix3* a, const coiVector3* m, transform_space relative_to)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    const Ogre::Matrix3 axes(a->m);
    const Ogre::Vector3 move(m->x, m->y, m->z);
    Ogre::Node::TransformSpace rt = llcoi_ts_to_ogre_ts(relative_to);

    node->translate(axes, move, rt);
}

//Ogre::Node::roll(Ogre::Radian const&, Ogre::Node::TransformSpace)
void node_roll(NodeHandle handle, const coiReal angle, transform_space relative_to)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    Ogre::Node::TransformSpace rt = llcoi_ts_to_ogre_ts(relative_to);
    node->roll(Ogre::Radian(angle), rt);
}

//Ogre::Node::pitch(Ogre::Radian const&, Ogre::Node::TransformSpace)
void node_pitch(NodeHandle handle, const coiReal angle, transform_space relative_to)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    Ogre::Node::TransformSpace rt = llcoi_ts_to_ogre_ts(relative_to);
    node->pitch(Ogre::Radian(angle), rt);
}

// Ogre::Node::yaw(Ogre::Radian const&, Ogre::Node::TransformSpace)
void node_yaw(NodeHandle handle, const coiReal angle, transform_space relative_to)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    Ogre::Node::TransformSpace rt = llcoi_ts_to_ogre_ts(relative_to);
    node->yaw(Ogre::Radian(angle), rt);
}

//Ogre::Node::rotate(Ogre::Vector3 const&, Ogre::Radian const&, Ogre::Node::TransformSpace)
void node_rotate(NodeHandle handle, const coiVector3* axis, const coiReal angle, transform_space relative_to)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    const Ogre::Vector3 _axis(axis->x, axis->y, axis->z);
    const Ogre::Radian _angle(angle);
    Ogre::Node::TransformSpace rt = llcoi_ts_to_ogre_ts(relative_to);

    node->rotate(_axis, _angle, rt);
}

//Ogre::Node::rotate(Ogre::Quaternion const&, Ogre::Node::TransformSpace)
void node_rotate_q(NodeHandle handle, const coiQuaternion* q, transform_space relative_to)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    const Ogre::Quaternion _q(q->w, q->x, q->y, q->z);
    Ogre::Node::TransformSpace rt = llcoi_ts_to_ogre_ts(relative_to);
    node->rotate(_q, rt);
}

//Ogre::Node::getLocalAxes() const
void node_get_local_axes(NodeHandle handle, coiMatrix3* r)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    const Ogre::Matrix3& m = node->getLocalAxes();
    ogre_matrix3_to_llcoi_matrix3(m, *r);
}

//Ogre::Node::createChild(Ogre::Vector3 const&, Ogre::Quaternion const&)
NodeHandle node_create_child(NodeHandle handle, const coiVector3* t, const coiQuaternion* r)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    const Ogre::Vector3 translate(t->x, t->y, t->z);
    const Ogre::Quaternion rotate(r->w, r->x, r->y, r->z);
    return static_cast<NodeHandle>(node->createChild(translate, rotate));
}

//Ogre::Node::createChild(std::string const&, Ogre::Vector3 const&, Ogre::Quaternion const&)
NodeHandle node_create_named_child(NodeHandle handle, const char* name, const coiVector3* t, const coiQuaternion* r)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    const Ogre::Vector3 translate(t->x, t->y, t->z);
    const Ogre::Quaternion rotate(r->w, r->x, r->y, r->z);

    return static_cast<NodeHandle>(node->createChild(Ogre::String(name), translate, rotate));
}

//Ogre::Node::addChild(Ogre::Node*)
void node_add_child(NodeHandle handle, NodeHandle c)
{
    Ogre::Node* node  = static_cast<Ogre::Node*>(handle);
    Ogre::Node* child = static_cast<Ogre::Node*>(c);
    node->addChild(child);
}

//Ogre::Node::numChildren() const
unsigned short node_num_children(const NodeHandle handle)
{
    const Ogre::Node* node = static_cast<const Ogre::Node*>(handle);
    return node->numChildren();
}

//Ogre::Node::getChild(unsigned short) const
NodeHandle node_get_child_by_index(const NodeHandle handle, unsigned short index)
{
    const Ogre::Node* node = static_cast<const Ogre::Node*>(handle); 
    return static_cast<NodeHandle>(node->getChild(index));
}

//Ogre::Node::getChild(std::string const&) const
NodeHandle node_get_child_by_name(const NodeHandle handle, const char* name)
{
    const Ogre::Node* node = static_cast<const Ogre::Node*>(handle); 
    return static_cast<NodeHandle>(node->getChild(Ogre::String(name)));
}

//Ogre::Node::removeChild(unsigned short)
NodeHandle node_remove_child_by_index(NodeHandle handle, unsigned short index)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    Ogre::Node* child = node->removeChild(index);
    return static_cast<NodeHandle>(child);
}

//Ogre::Node::removeChild(Ogre::Node*)
NodeHandle node_remove_child(NodeHandle handle, NodeHandle child)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    Ogre::Node* _child = static_cast<Ogre::Node*>(child);
    Ogre::Node* result = node->removeChild(_child);
    return static_cast<NodeHandle>(result);
}

//Ogre::Node::removeChild(std::string const&)
NodeHandle node_remove_child_by_name(NodeHandle handle, const char* name)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    Ogre::Node* child = node->removeChild(Ogre::String(name));
    return static_cast<NodeHandle>(child);
}

//Ogre::Node::removeAllChildren()
void node_remove_all_children(NodeHandle handle)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    node->removeAllChildren();
}


//Ogre::Node::_setDerivedPosition(Ogre::Vector3 const&)
void node__set_derived_position(NodeHandle handle, const coiVector3* p)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    const Ogre::Vector3 pos(p->x, p->y, p->z);
    node->_setDerivedPosition(pos);
}

//Ogre::Node::_setDerivedOrientation(Ogre::Quaternion const&)
void node__set_derived_orientation(NodeHandle handle, const coiQuaternion* o)
{
    Ogre::Node* node = static_cast<Ogre::Node*>(handle);
    const Ogre::Quaternion q(o->w, o->x, o->y, o->z);
    node->_setDerivedOrientation(q);
}

//Ogre::Node::_getDerivedOrientation() const
void node__get_derived_orientation(const NodeHandle handle, coiQuaternion* o)
{
    const Ogre::Node* node = static_cast<const Ogre::Node*>(handle);
    const Ogre::Quaternion& q = node->_getDerivedOrientation();

    o->w = q.w;
    o->x = q.x;
    o->y = q.y;
    o->z = q.z;
}

//Ogre::Node::_getDerivedPosition() const
void node__get_derived_position(const NodeHandle handle, coiVector3* p)
{
    const Ogre::Node* node = static_cast<const Ogre::Node*>(handle);
    const Ogre::Vector3& v = node->_getDerivedPosition();

    p->x = v.x;
    p->y = v.y;
    p->z = v.z;
}

//Ogre::Node::_getDerivedScale() const
void node__get_derived_scale(const NodeHandle handle, coiVector3* s)
{
    const Ogre::Node* node = static_cast<const Ogre::Node*>(handle);
    const Ogre::Vector3& v = node->_getDerivedScale();

    s->x = v.x;
    s->y = v.y;
    s->z = v.z;
}



/*
//Ogre::Node::Listener
//Ogre::Node::DebugRenderable
//Ogre::Node::operator=(Ogre::Node const&)
//Ogre::Node::Node(Ogre::Node const&)
//Ogre::Node::Node()
//Ogre::Node::Node(std::string const&)
//Ogre::Node::~Node()
//Ogre::Node::getChildIterator()
//Ogre::Node::getChildIterator() const
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
