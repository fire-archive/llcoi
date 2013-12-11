/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "binding_utils.h"
#include "movableobject_bind.h"

#include <OgreRoot.h>
#include <OgreMovableObject.h>

///~MovableObject();
void destroy_movableobject(MovableObjectHandle handle)
{
    Ogre::MovableObject* obj = static_cast<Ogre::MovableObject*>(handle);
    delete obj;
}

///TODO: void _notifyCreator(MovableObjectFactory* fact);
///TODO: MovableObjectFactory*  _getCreator(void) const;
///void _notifyManager(SceneManager* man);
void movableobject__notify_manager(MovableObjectHandle handle, SceneManagerHandle man)
{
    Ogre::MovableObject* obj = static_cast<Ogre::MovableObject*>(handle);
    Ogre::SceneManager* sm = static_cast<Ogre::SceneManager*>(man);
    obj->_notifyManager(sm);
}
///SceneManager* _getManager(void) const;
SceneManagerHandle movableobject__get_manager(const MovableObjectHandle handle)
{
    const Ogre::MovableObject* obj = static_cast<const Ogre::MovableObject*>(handle);
    Ogre::SceneManager* sm = obj->_getManager();
    return static_cast<SceneManagerHandle>(sm);
}
///const String& getName(void) const;
const char* movableobject_get_name(const MovableObjectHandle handle)
{
    const Ogre::MovableObject* obj = static_cast<const Ogre::MovableObject*>(handle);
    return obj->getName().c_str();
}

///const String& getMovableType(void) const = 0;
const char* movableobject_get_movable_type(const MovableObjectHandle handle)
{
    const Ogre::MovableObject* obj = static_cast<const Ogre::MovableObject*>(handle);
    return obj->getMovableType().c_str();
}

///Node* getParentNode(void) const;
NodeHandle movableobject_get_parent_node(const MovableObjectHandle handle)
{
    const Ogre::MovableObject* obj = static_cast<const Ogre::MovableObject*>(handle);
    Ogre::Node* node = obj->getParentNode();
    return static_cast<NodeHandle>(node);
}

///SceneNode* getParentSceneNode(void) const;
SceneNodeHandle movableobject_get_parent_scene_node(const MovableObjectHandle handle)
{
    const Ogre::MovableObject* obj = static_cast<const Ogre::MovableObject*>(handle);
    Ogre::SceneNode* sn = obj->getParentSceneNode();
    return static_cast<SceneNodeHandle>(sn);
}

///bool isParentTagPoint() const;
int movableobject_is_parent_tag_point(const MovableObjectHandle handle)
{
    const Ogre::MovableObject* obj = static_cast<const Ogre::MovableObject*>(handle);
    return obj->isParentTagPoint();
}

///void _notifyAttached(Node* parent, bool isTagPoint = false);
void movableobject__notify_attached(MovableObjectHandle handle, NodeHandle parent, int is_tag_point)
{
    Ogre::MovableObject* obj = static_cast<Ogre::MovableObject*>(handle);
    Ogre::Node* node = static_cast<Ogre::Node*>(parent);
    obj->_notifyAttached(node, is_tag_point);
}

///bool isAttached(void) const;
int movableobject_is_attached(const MovableObjectHandle handle)
{
    const Ogre::MovableObject* obj = static_cast<const Ogre::MovableObject*>(handle);
    return obj->isAttached();
}

///void detachFromParent(void);
void movableobject_detach_from_parent(MovableObjectHandle handle)
{
    Ogre::MovableObject* obj = static_cast<Ogre::MovableObject*>(handle);
    obj->detachFromParent();
}

///bool isInScene(void) const;
int movableobject_is_in_scene(const MovableObjectHandle handle)
{
    const Ogre::MovableObject* obj = static_cast<const Ogre::MovableObject*>(handle);
    return obj->isInScene();
}

///void _notifyMoved(void);
void movableobject__notify_moved(MovableObjectHandle handle)
{
    Ogre::MovableObject* obj = static_cast<Ogre::MovableObject*>(handle);
    obj->_notifyMoved();
}

///void _notifyCurrentCamera(Camera* cam);
void movableobject__notify_current_camera(MovableObjectHandle handle, CameraHandle cam)
{
    Ogre::MovableObject* obj = static_cast<Ogre::MovableObject*>(handle);
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(cam);
    obj->_notifyCurrentCamera(camera);
}

///const AxisAlignedBox& getBoundingBox(void) const = 0;
AxisAlignedBoxHandle movableobject_get_bounding_box(const MovableObjectHandle handle)
{
    const Ogre::MovableObject* obj = static_cast<const Ogre::MovableObject*>(handle);
    const Ogre::AxisAlignedBox& aabb = obj->getBoundingBox();
    // TODO: preserve constness
    return static_cast<AxisAlignedBoxHandle>(const_cast<Ogre::AxisAlignedBox*>(&aabb));
}

///Real getBoundingRadius(void) const = 0;
coiReal movableobject_get_bounding_radius(const MovableObjectHandle handle)
{
    const Ogre::MovableObject* obj = static_cast<const Ogre::MovableObject*>(handle);
    return obj->getBoundingRadius();
}

///const AxisAlignedBox& getWorldBoundingBox(bool derive = false) const;
AxisAlignedBoxHandle movableobject_get_world_bounding_box(const MovableObjectHandle handle, int derive)
{
    const Ogre::MovableObject* obj = static_cast<const Ogre::MovableObject*>(handle);
    const Ogre::AxisAlignedBox& aabb = obj->getWorldBoundingBox(derive);
    return static_cast<AxisAlignedBoxHandle>(const_cast<Ogre::AxisAlignedBox*>(&aabb));
}

///const Sphere& getWorldBoundingSphere(bool derive = false) const;
SphereHandle movableobject_get_world_bounding_sphere(const MovableObjectHandle handle, int derive)
{
    const Ogre::MovableObject* obj = static_cast<const Ogre::MovableObject*>(handle);
    const Ogre::Sphere& sphere = obj->getWorldBoundingSphere(derive);
    // TODO: preserve constness
    return static_cast<SphereHandle>(const_cast<Ogre::Sphere*>(&sphere));
}

///void _updateRenderQueue(RenderQueue* queue) = 0;
//TODO:DLL void movableobject__update_render_queue(MovableObjectHandle handle, RenderQueueHandle queue);
///void setVisible(bool visible);
void movableobject_set_visible(MovableObjectHandle handle, int visible)
{
    Ogre::MovableObject* obj = static_cast<Ogre::MovableObject*>(handle);
    obj->setVisible(visible);
}

///bool getVisible(void) const;
int movableobject_get_visible(const MovableObjectHandle handle)
{
    const Ogre::MovableObject* obj = static_cast<const Ogre::MovableObject*>(handle);
    return obj->getVisible();
}

///bool isVisible(void) const;
int movableobject_is_visible(const MovableObjectHandle handle)
{
    const Ogre::MovableObject* obj = static_cast<const Ogre::MovableObject*>(handle);
    return obj->isVisible();
}

///void setRenderingDistance(Real dist);
void movableobject_set_rendering_distance(MovableObjectHandle handle, coiReal dist)
{
    Ogre::MovableObject* obj = static_cast<Ogre::MovableObject*>(handle);
    obj->setRenderingDistance(dist);
}

///Real getRenderingDistance(void) const;
coiReal movableobject_get_rendering_distance(const MovableObjectHandle handle)
{
    const Ogre::MovableObject* obj = static_cast<const Ogre::MovableObject*>(handle);
    return obj->getRenderingDistance();
}

///void setRenderingMinPixelSize(Real pixelSize);
void movableobject_set_rendering_min_pixel_size(MovableObjectHandle handle, coiReal pixel_size)
{
    Ogre::MovableObject* obj = static_cast<Ogre::MovableObject*>(handle);
    obj->setRenderingMinPixelSize(pixel_size);
}

///Real getRenderingMinPixelSize() const;
coiReal movableobject_get_rendering_min_pixel_size(const MovableObjectHandle handle)
{
    const Ogre::MovableObject* obj = static_cast<const Ogre::MovableObject*>(handle);
    return obj->getRenderingMinPixelSize();
}

//void setUserAny(const Any& anything); XXX: deprecated
//const Any& getUserAny(void) const; XXX: deprecated
///UserObjectBindings&	getUserObjectBindings();
///const UserObjectBindings& getUserObjectBindings() const;
///void setRenderQueueGroup(uint8 queueID);
void movableobject_set_render_queue_group(MovableObjectHandle handle, unsigned char queue_id)
{
    Ogre::MovableObject* obj = static_cast<Ogre::MovableObject*>(handle);
    obj->setRenderQueueGroup(queue_id);
}

///void setRenderQueueGroupAndPriority(uint8 queueID, ushort priority);
void movableobject_set_render_queue_group_and_priority(MovableObjectHandle handle, unsigned char queue_id, unsigned short priority)
{
    Ogre::MovableObject* obj = static_cast<Ogre::MovableObject*>(handle);
    obj->setRenderQueueGroupAndPriority(queue_id, priority);
}

///uint8 getRenderQueueGroup(void) const;
unsigned char movableobject_get_render_queue_group(const MovableObjectHandle handle)
{
    const Ogre::MovableObject* obj = static_cast<const Ogre::MovableObject*>(handle);
    return obj->getRenderQueueGroup();
}

///const Matrix4& _getParentNodeFullTransform(void) const;
void movableobject__get_parent_node_full_transform(const MovableObjectHandle handle, coiMatrix4* result)
{
    const Ogre::MovableObject* obj = static_cast<const Ogre::MovableObject*>(handle);
    const Ogre::Matrix4& xform = obj->_getParentNodeFullTransform();
    ogre_matrix4_to_llcoi_matrix4(xform, *result);
}

///void setQueryFlags(uint32 flags);
void movableobject_set_query_flags(MovableObjectHandle handle, unsigned int flags)
{
    Ogre::MovableObject* obj = static_cast<Ogre::MovableObject*>(handle);
    obj->setQueryFlags(flags);
}

///void addQueryFlags(uint32 flags);
void movableobject_add_query_flags(MovableObjectHandle handle, unsigned int flags)
{
    Ogre::MovableObject* obj = static_cast<Ogre::MovableObject*>(handle);
    obj->addQueryFlags(flags);
}

///void removeQueryFlags(uint32 flags);
void movableobject_remove_query_flags(MovableObjectHandle handle, unsigned int flags)
{
    Ogre::MovableObject* obj = static_cast<Ogre::MovableObject*>(handle);
    obj->removeQueryFlags(flags);
}

///uint32 getQueryFlags(void) const;
unsigned int movableobject_get_query_flags(const MovableObjectHandle handle)
{
    const Ogre::MovableObject* obj = static_cast<const Ogre::MovableObject*>(handle);
    return obj->getQueryFlags();
}

///static void setDefaultQueryFlags(uint32 flags);
void movableobject_set_default_query_flags(unsigned int flags)
{
    Ogre::MovableObject::setDefaultQueryFlags(flags);
}

///static uint32 getDefaultQueryFlags();
unsigned int movableobject_get_default_query_flags()
{
    return Ogre::MovableObject::getDefaultQueryFlags();
}

///void setVisibilityFlags(uint32 flags)
void movableobject_set_visibility_flags(MovableObjectHandle handle, unsigned int flags)
{ 
    Ogre::MovableObject* obj = static_cast<Ogre::MovableObject*>(handle);
    obj->setVisibilityFlags(flags);
}

///void addVisibilityFlags(uint32 flags);
void movableobject_add_visibility_flags(MovableObjectHandle handle, unsigned int flags)
{
    Ogre::MovableObject* obj = static_cast<Ogre::MovableObject*>(handle);
    obj->addVisibilityFlags(flags);
}

///void removeVisibilityFlags(uint32 flags);
void movableobject_remove_visibility_flags(MovableObjectHandle handle, unsigned int flags)
{
    Ogre::MovableObject* obj = static_cast<Ogre::MovableObject*>(handle);
    obj->removeVisibilityFlags(flags);
}

///uint32 getVisibilityFlags(void) const;
unsigned int movableobject_get_visibility_flags(const MovableObjectHandle handle)
{
    const Ogre::MovableObject* obj = static_cast<const Ogre::MovableObject*>(handle);
    return obj->getVisibilityFlags();
}

///static void setDefaultVisibilityFlags(uint32 flags);
void movableobject_set_default_visibility_flags(unsigned int flags)
{
    Ogre::MovableObject::setDefaultVisibilityFlags(flags);
}

///static uint32 getDefaultVisibilityFlags();
unsigned int movableobject_get_default_visibility_flags()
{
    return Ogre::MovableObject::getDefaultVisibilityFlags();
}

///void setListener(Listener* listener);
//movableobject_set_listener(MovableObjectHandle handle, 
///Listener* getListener(void) const;
//movableobject_get_listener(MovableObjectHandle handle, 
///const LightList& queryLights(void) const;
size_t movableobject_query_lights(const MovableObjectHandle handle, LightHandle result)
{
    const Ogre::MovableObject* obj = static_cast<const Ogre::MovableObject*>(handle); 
    // TODO: can we preserve constness here?
    Ogre::LightList& ll = const_cast<Ogre::LightList&>(obj->queryLights());
    return ogre_light_list_to_llcoi(ll, result);
}

///uint32 getLightMask() const;
unsigned int movableobject_get_light_mask(const MovableObjectHandle handle)
{
    const Ogre::MovableObject* obj = static_cast<const Ogre::MovableObject*>(handle);
    return obj->getLightMask();
}

///void setLightMask(uint32 lightMask);
void movableobject_set_light_mask(MovableObjectHandle handle, unsigned int light_mask)
{
    Ogre::MovableObject* obj = static_cast<Ogre::MovableObject*>(handle);
    obj->setLightMask(light_mask);
}

///LightList* _getLightList();
///EdgeData* getEdgeList(void);
///bool hasEdgeList(void);
int movableobject_has_edge_list(MovableObjectHandle handle)
{
    Ogre::MovableObject* obj = static_cast<Ogre::MovableObject*>(handle);
    return obj->hasEdgeList();
}

///ShadowRenderableListIterator getShadowVolumeRenderableIterator(ShadowTechnique shadowTechnique, const Light* light, HardwareIndexBufferSharedPtr* indexBuffer,  bool extrudeVertices, Real extrusionDist, unsigned long flags = 0);
///const AxisAlignedBox& getLightCapBounds(void) const;
AxisAlignedBoxHandle movableobject_get_light_cap_bounds(const MovableObjectHandle handle)
{
    const Ogre::MovableObject* obj = static_cast<const Ogre::MovableObject*>(handle);
    const Ogre::AxisAlignedBox& aabb = obj->getLightCapBounds();
    // TODO: preserve constness
    return static_cast<AxisAlignedBoxHandle>(const_cast<Ogre::AxisAlignedBox*>(&aabb));
}

///const AxisAlignedBox& getDarkCapBounds(const Light& light, Real dirLightExtrusionDist) const;
AxisAlignedBoxHandle movableobject_get_dark_cap_bounds(const MovableObjectHandle handle, const LightHandle light, coiReal dir_light_extrusion_dist)
{
    const Ogre::MovableObject* obj = static_cast<const Ogre::MovableObject*>(handle);
    const Ogre::Light* l = static_cast<const Ogre::Light*>(light);
    const Ogre::AxisAlignedBox& aabb = obj->getDarkCapBounds(*l, dir_light_extrusion_dist);
    return static_cast<AxisAlignedBoxHandle>(const_cast<Ogre::AxisAlignedBox*>(&aabb));
}

///void setCastShadows(bool enabled);
void movableobject_set_cast_shadows(MovableObjectHandle handle, int enabled)
{
    Ogre::MovableObject* obj = static_cast<Ogre::MovableObject*>(handle);
    obj->setCastShadows(enabled);
}

///bool getCastShadows(void) const;
int movableobject_get_cast_shadows(const MovableObjectHandle handle)
{
    const Ogre::MovableObject* obj = static_cast<const Ogre::MovableObject*>(handle);
    return obj->getCastShadows();
}

///bool getReceivesShadows();
int movableobject_get_receives_shadows(MovableObjectHandle handle)
{
    Ogre::MovableObject* obj = static_cast<Ogre::MovableObject*>(handle);
    return obj->getReceivesShadows();
}

///Real getPointExtrusionDistance(const Light* l) const;
coiReal movableobject_get_point_extrusion_distance(const MovableObjectHandle handle, const LightHandle l)
{
    const Ogre::MovableObject* obj = static_cast<const Ogre::MovableObject*>(handle);
    const Ogre::Light* light = static_cast<const Ogre::Light*>(l);
    return obj->getPointExtrusionDistance(light);
}

///uint32 getTypeFlags(void) const;
unsigned int movableobject_get_type_flags(const MovableObjectHandle handle)
{
    const Ogre::MovableObject* obj = static_cast<const Ogre::MovableObject*>(handle); 
    return obj->getTypeFlags();
}

///void visitRenderables(Renderable::Visitor* visitor,bool debugRenderables = false) = 0;
///void setDebugDisplayEnabled(bool enabled);
void movableobject_set_debug_display_enabled(MovableObjectHandle handle, int enabled)
{
    Ogre::MovableObject* obj = static_cast<Ogre::MovableObject*>(handle);
    obj->setDebugDisplayEnabled(enabled);
}

///bool isDebugDisplayEnabled(void) const;
int movableobject_is_debug_display_enabled(const MovableObjectHandle handle)
{
    const Ogre::MovableObject* obj = static_cast<const Ogre::MovableObject*>(handle);
    return obj->isDebugDisplayEnabled();
}

/*
//Ogre::MovableObject::Listener
//Ogre::MovableObject::operator=(Ogre::MovableObject const&)
//Ogre::MovableObject::MovableObject(Ogre::MovableObject const&)
//Ogre::MovableObject::MovableObject()
//Ogre::MovableObject::MovableObject(std::string const&)
//Ogre::MovableObject::~MovableObject()
//Ogre::MovableObject::_notifyCreator(Ogre::MovableObjectFactory*)
//Ogre::MovableObject::_getCreator() const
//Ogre::MovableObject::_notifyManager(Ogre::SceneManager*)
//Ogre::MovableObject::_getManager() const
//Ogre::MovableObject::getName() const
//Ogre::MovableObject::getMovableType() const
//Ogre::MovableObject::getParentNode() const
//Ogre::MovableObject::getParentSceneNode() const
//Ogre::MovableObject::isParentTagPoint() const
//Ogre::MovableObject::_notifyAttached(Ogre::Node*, bool)
//Ogre::MovableObject::isAttached() const
//Ogre::MovableObject::detachFromParent()
//Ogre::MovableObject::isInScene() const
//Ogre::MovableObject::_notifyMoved()
//Ogre::MovableObject::_notifyCurrentCamera(Ogre::Camera*)
//Ogre::MovableObject::getBoundingBox() const
//Ogre::MovableObject::getBoundingRadius() const
//Ogre::MovableObject::getWorldBoundingBox(bool) const
//Ogre::MovableObject::getWorldBoundingSphere(bool) const
//Ogre::MovableObject::_updateRenderQueue(Ogre::RenderQueue*)
//Ogre::MovableObject::setVisible(bool)
//Ogre::MovableObject::getVisible() const
//Ogre::MovableObject::isVisible() const
//Ogre::MovableObject::setRenderingDistance(float)
//Ogre::MovableObject::getRenderingDistance() const
//Ogre::MovableObject::setUserAny(Ogre::Any const&)
//Ogre::MovableObject::getUserAny() const
//Ogre::MovableObject::getUserObjectBindings()
//Ogre::MovableObject::getUserObjectBindings() const
//Ogre::MovableObject::setRenderQueueGroup(unsigned char)
//Ogre::MovableObject::setRenderQueueGroupAndPriority(unsigned char, unsigned short)
//Ogre::MovableObject::getRenderQueueGroup() const
//Ogre::MovableObject::_getParentNodeFullTransform() const
//Ogre::MovableObject::setQueryFlags(unsigned int)
//Ogre::MovableObject::addQueryFlags(unsigned int)
//Ogre::MovableObject::removeQueryFlags(unsigned long)
//Ogre::MovableObject::getQueryFlags() const
//Ogre::MovableObject::setDefaultQueryFlags(unsigned int)
//Ogre::MovableObject::getDefaultQueryFlags()
//Ogre::MovableObject::setVisibilityFlags(unsigned int)
//Ogre::MovableObject::addVisibilityFlags(unsigned int)
//Ogre::MovableObject::removeVisibilityFlags(unsigned int)
//Ogre::MovableObject::getVisibilityFlags() const
//Ogre::MovableObject::setDefaultVisibilityFlags(unsigned int)
//Ogre::MovableObject::getDefaultVisibilityFlags()
//Ogre::MovableObject::setListener(Ogre::MovableObject::Listener*)
//Ogre::MovableObject::getListener() const
//Ogre::MovableObject::queryLights() const
//Ogre::MovableObject::getLightMask() const
//Ogre::MovableObject::setLightMask(unsigned int)
//Ogre::MovableObject::_getLightList()
//Ogre::MovableObject::getEdgeList()
//Ogre::MovableObject::hasEdgeList()
//Ogre::MovableObject::getShadowVolumeRenderableIterator(Ogre::ShadowTechnique, Ogre::Light const*, Ogre::HardwareIndexBufferSharedPtr*, bool, float, unsigned long)
//Ogre::MovableObject::getLightCapBounds() const
//Ogre::MovableObject::getDarkCapBounds(Ogre::Light const&, float) const
//Ogre::MovableObject::setCastShadows(bool)
//Ogre::MovableObject::getCastShadows() const
//Ogre::MovableObject::getReceivesShadows()
//Ogre::MovableObject::getPointExtrusionDistance(Ogre::Light const*) const
//Ogre::MovableObject::getTypeFlags() const
//Ogre::MovableObject::visitRenderables(Ogre::Renderable::Visitor*, bool)
//Ogre::MovableObject::setDebugDisplayEnabled(bool)
//Ogre::MovableObject::isDebugDisplayEnabled() const
//Ogre::MovableObject::Listener::operator=(Ogre::MovableObject::Listener const&)
//Ogre::MovableObject::Listener::Listener(Ogre::MovableObject::Listener const&)
//Ogre::MovableObject::Listener::Listener()
//Ogre::MovableObject::Listener::~Listener()
//Ogre::MovableObject::Listener::objectDestroyed(Ogre::MovableObject*)
//Ogre::MovableObject::Listener::objectAttached(Ogre::MovableObject*)
//Ogre::MovableObject::Listener::objectDetached(Ogre::MovableObject*)
//Ogre::MovableObject::Listener::objectMoved(Ogre::MovableObject*)
//Ogre::MovableObject::Listener::objectRendering(Ogre::MovableObject const*, Ogre::Camera const*)
//Ogre::MovableObject::Listener::objectQueryLights(Ogre::MovableObject const*)
*/
