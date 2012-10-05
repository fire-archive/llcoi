/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "ogre_interface.h"
#include "entity_bind.h"

#include <OgreEntity.h>

///Ogre::Entity::getMesh() const
const MeshPtrHandle entity_get_mesh(const EntityHandle handle)
{
    const Ogre::Entity* ent = static_cast<const Ogre::Entity*>(handle);
    const Ogre::MeshPtr& ptr = ent->getMesh();
    return static_cast<MeshPtrHandle>(ptr.get());
}

///Ogre::Entity::getNumSubEntities() const
unsigned int entity_get_num_sub_entities(const EntityHandle handle)
{
    const Ogre::Entity* entity = static_cast<const Ogre::Entity*>(handle);
    return entity->getNumSubEntities();
}

//Ogre::Entity::clone(std::string const&) const
EntityHandle entity_clone(const EntityHandle handle, const char* name)
{
    const Ogre::Entity* obj = static_cast<const Ogre::Entity*>(handle);
    Ogre::Entity* clone = obj->clone(Ogre::String(name));
    return static_cast<EntityHandle>(clone);
}


void entity_set_cast_shadows(EntityHandle handle, int enabled)
{
    Ogre::Entity* entity = static_cast<Ogre::Entity*>(handle);
    entity->setCastShadows(enabled);
}

int entity_get_cast_shadows(const EntityHandle handle)
{
    const Ogre::Entity* entity = static_cast<const Ogre::Entity*>(handle);
    return entity->getCastShadows();
}

int entity_get_receives_shadows(EntityHandle handle)
{
    Ogre::Entity* entity = static_cast<Ogre::Entity*>(handle);
    return entity->getReceivesShadows();
}

///Ogre::Entity::setMaterialName(std::string const&, std::string const&)
void entity_set_material(EntityHandle handle, MaterialPtrHandle mat)
{
    Ogre::Entity* ent = static_cast<Ogre::Entity*>(handle);
    Ogre::Material* material = static_cast<Ogre::Material*>(mat);
    Ogre::MaterialPtr ptr(material);
    ent->setMaterial(ptr);
}

///Ogre::Entity::setMaterial(Ogre::MaterialPtr const&)
void entity_set_material_name(EntityHandle handle, const char* material_name, const char* group_name)
{
    Ogre::Entity* entity = static_cast<Ogre::Entity*>(handle);
    entity->setMaterialName(Ogre::String(material_name), Ogre::String(group_name));
}

///Ogre::Entity::_notifyCurrentCamera(Ogre::Camera*)
void entity__notify_current_camera(EntityHandle handle, CameraHandle cam)
{
    Ogre::Entity* entity = static_cast<Ogre::Entity*>(handle);
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(cam);
    entity->_notifyCurrentCamera(camera);
}

///Ogre::Entity::setRenderQueueGroup(unsigned char)
void entity_set_render_queue_group(EntityHandle handle, unsigned char queue_id)
{
    Ogre::Entity* entity = static_cast<Ogre::Entity*>(handle);
    entity->setRenderQueueGroup(queue_id);
}

///Ogre::Entity::setRenderQueueGroupAndPriority(unsigned char, unsigned short)
void entity_set_render_queue_group_and_priority(EntityHandle handle, unsigned char queue_id, unsigned short priority)
{
    Ogre::Entity* entity = static_cast<Ogre::Entity*>(handle);
    entity->setRenderQueueGroupAndPriority(queue_id, priority);
}

//Ogre::Entity::getBoundingBox() const
AxisAlignedBoxHandle entity_get_bounding_box(const EntityHandle handle)
{
    Ogre::Entity* entity = static_cast<Ogre::Entity*>(handle);
    Ogre::AxisAlignedBox& box = const_cast<Ogre::AxisAlignedBox&>(entity->getBoundingBox());
    return reinterpret_cast<AxisAlignedBoxHandle>(&box);
}


//Ogre::Entity::getBoundingRadius() const
coiReal entity_get_bounding_radius(const EntityHandle handle)
{
    const Ogre::Entity* entity = static_cast<const Ogre::Entity*>(handle);
    return entity->getBoundingRadius();
}

//Ogre::Entity::setDisplaySkeleton(bool)
void entity_set_display_skeleton(EntityHandle handle, int display)
{
    Ogre::Entity* entity = static_cast<Ogre::Entity*>(handle); 
    entity->setDisplaySkeleton(display);
}

//Ogre::Entity::getDisplaySkeleton() const
int entity_get_display_skeleton(const EntityHandle handle)
{
    const Ogre::Entity* entity = static_cast<const Ogre::Entity*>(handle);
    return entity->getDisplaySkeleton();
}

///Ogre::Entity::hasSkeleton() const
int entity_has_skeleton(const EntityHandle handle)
{
    const Ogre::Entity* ent = static_cast<const Ogre::Entity*>(handle);
    return ent->hasSkeleton();
}

///Ogre::Entity::getSkeleton() const
SkeletonInstanceHandle entity_get_skeleton(const EntityHandle handle)
{
    const Ogre::Entity* ent = static_cast<const Ogre::Entity*>(handle);
    Ogre::SkeletonInstance* skel = ent->getSkeleton();
    return static_cast<SkeletonInstanceHandle>(skel);
}

///Ogre::Entity::isHardwareAnimationEnabled()
int entity_is_hardware_animation_enabled(EntityHandle handle)
{
    Ogre::Entity* ent = static_cast<Ogre::Entity*>(handle);
    return ent->isHardwareAnimationEnabled();
}

/*
///Ogre::Entity::operator=(Ogre::Entity const&)
///Ogre::Entity::Entity(Ogre::Entity const&)
///Ogre::Entity::~Entity()
///Ogre::Entity::getSubEntity(unsigned int) const
///Ogre::Entity::getSubEntity(std::string const&) const
///Ogre::Entity::_updateRenderQueue(Ogre::RenderQueue*)
///Ogre::Entity::getMovableType() const
///Ogre::Entity::getAnimationState(std::string const&) const
///Ogre::Entity::getAllAnimationStates() const
///Ogre::Entity::getManualLodLevel(unsigned int) const
///Ogre::Entity::getNumManualLodLevels() const
///Ogre::Entity::getCurrentLodIndex()
///Ogre::Entity::setMeshLodBias(float, unsigned short, unsigned short)
///Ogre::Entity::setMaterialLodBias(float, unsigned short, unsigned short)
///Ogre::Entity::setPolygonModeOverrideable(bool)
///Ogre::Entity::attachObjectToBone(std::string const&, Ogre::MovableObject*, Ogre::Quaternion const&, Ogre::Vector3 const&)
///Ogre::Entity::detachObjectFromBone(std::string const&)
///Ogre::Entity::detachObjectFromBone(Ogre::MovableObject*)
///Ogre::Entity::detachAllObjectsFromBone()
///Ogre::Entity::getAttachedObjectIterator()
///Ogre::Entity::getWorldBoundingBox(bool) const
///Ogre::Entity::getWorldBoundingSphere(bool) const
///Ogre::Entity::getEdgeList()
///Ogre::Entity::hasEdgeList()
///Ogre::Entity::getShadowVolumeRenderableIterator(Ogre::ShadowTechnique, Ogre::Light const*, Ogre::HardwareIndexBufferSharedPtr*, bool, float, unsigned long)
///Ogre::Entity::_getBoneMatrices() const
///Ogre::Entity::_getNumBoneMatrices() const
///Ogre::Entity::_notifyAttached(Ogre::Node*, bool)
///Ogre::Entity::getSoftwareAnimationRequests() const
///Ogre::Entity::getSoftwareAnimationNormalsRequests() const
///Ogre::Entity::addSoftwareAnimationRequest(bool)
///Ogre::Entity::removeSoftwareAnimationRequest(bool)
///Ogre::Entity::shareSkeletonInstanceWith(Ogre::Entity*)
///Ogre::Entity::hasVertexAnimation() const
///Ogre::Entity::stopSharingSkeletonInstance()
///Ogre::Entity::sharesSkeletonInstance() const
///Ogre::Entity::getSkeletonInstanceSharingSet() const
///Ogre::Entity::refreshAvailableAnimationState()
///Ogre::Entity::_updateAnimation()
///Ogre::Entity::_isAnimated() const
///Ogre::Entity::_isSkeletonAnimated() const
///Ogre::Entity::_getSkelAnimVertexData() const
///Ogre::Entity::_getSoftwareVertexAnimVertexData() const
///Ogre::Entity::_getHardwareVertexAnimVertexData() const
///Ogre::Entity::_getSkelAnimTempBufferInfo()
///Ogre::Entity::_getVertexAnimTempBufferInfo()
///Ogre::Entity::getTypeFlags() const
///Ogre::Entity::getVertexDataForBinding()
///Ogre::Entity::chooseVertexDataForBinding(bool)
///Ogre::Entity::_getBuffersMarkedForAnimation() const
///Ogre::Entity::_markBuffersUsedForAnimation()
///Ogre::Entity::isInitialised() const
///Ogre::Entity::_initialise(bool)
///Ogre::Entity::_deinitialise()
///Ogre::Entity::backgroundLoadingComplete(Ogre::Resource*)
///Ogre::Entity::visitRenderables(Ogre::Renderable::Visitor*, bool)
///Ogre::Entity::_getMeshLodFactorTransformed() const
///Ogre::Entity::setSkipAnimationStateUpdate(bool)
///Ogre::Entity::getSkipAnimationStateUpdate() const
///Ogre::Entity::setAlwaysUpdateMainSkeleton(bool)
///Ogre::Entity::getAlwaysUpdateMainSkeleton() const
///Ogre::Entity::EntityShadowRenderable::operator=(Ogre::Entity::EntityShadowRenderable const&)
///Ogre::Entity::EntityShadowRenderable::EntityShadowRenderable(Ogre::Entity::EntityShadowRenderable const&)
///Ogre::Entity::EntityShadowRenderable::EntityShadowRenderable(Ogre::Entity*, Ogre::HardwareIndexBufferSharedPtr*, Ogre::VertexData const*, bool, Ogre::SubEntity*, bool)
///Ogre::Entity::EntityShadowRenderable::~EntityShadowRenderable()
///Ogre::Entity::EntityShadowRenderable::_createSeparateLightCap()
///Ogre::Entity::EntityShadowRenderable::getWorldTransforms(Ogre::Matrix4*) const
///Ogre::Entity::EntityShadowRenderable::getPositionBuffer()
///Ogre::Entity::EntityShadowRenderable::getWBuffer()
///Ogre::Entity::EntityShadowRenderable::rebindPositionBuffer(Ogre::VertexData const*, bool)
///Ogre::Entity::EntityShadowRenderable::isVisible() const
*/
