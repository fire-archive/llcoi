/******************************************************************************
 * scenemanager_bind.cpp - bindings for Ogre::SceneManager
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
#include <OgreRenderWindow.h>
#include <OgreCamera.h>
#include "ogre_manager.h"

void set_ambient_light_rgba(const float r, const float g, const float b, const float a)
{
    Ogre::Root::getSingletonPtr()->getSceneManager(OgreManager::getSingletonPtr()->get_active_scene_manager_name())->setAmbientLight(Ogre::ColourValue(r, g, b, a));
}

void set_ambient_light_rgb(const float r, const float g, const float b)
{
    Ogre::Root::getSingletonPtr()->getSceneManager(OgreManager::getSingletonPtr()->get_active_scene_manager_name())->setAmbientLight(Ogre::ColourValue(r, g, b));
}

SceneManagerHandle create_scene_manager(const char* type_name, const char* instance_name)
{
/*    va_list arg_list;
    const char* instance_name = NULL;
    va_start(arg_list, type_name);
    instance_name = va_arg(arg_list, const char*);
    va_end(arg_list);

    if(instance_name == NULL) instance_name = "default";*/
    
    OgreManager::getSingletonPtr()->set_active_scene_manager_name(instance_name);
    
    Ogre::SceneManager* sm = Ogre::Root::getSingletonPtr()->createSceneManager(Ogre::String(type_name), Ogre::String(instance_name));
    return reinterpret_cast<SceneManagerHandle>(sm);
}

SceneManagerHandle get_scene_manager()
{
    Ogre::SceneManager* sm = Ogre::Root::getSingletonPtr()->getSceneManager(OgreManager::getSingletonPtr()->get_active_scene_manager_name());
    return reinterpret_cast<SceneManagerHandle>(sm);
}

SceneManagerHandle get_scene_manager_by_name(const char* scene_manager_instance_name)
{
    Ogre::SceneManager* sm = Ogre::Root::getSingletonPtr()->getSceneManager(scene_manager_instance_name);
    return reinterpret_cast<SceneManagerHandle>(sm);
}

/*
Ogre::SceneManager::WORLD_GEOMETRY_TYPE_MASK
Ogre::SceneManager::ENTITY_TYPE_MASK
Ogre::SceneManager::FX_TYPE_MASK
Ogre::SceneManager::STATICGEOMETRY_TYPE_MASK
Ogre::SceneManager::LIGHT_TYPE_MASK
Ogre::SceneManager::FRUSTUM_TYPE_MASK
Ogre::SceneManager::USER_TYPE_MASK_LIMIT
Ogre::SceneManager::materialLess
Ogre::SceneManager::lightLess
Ogre::SceneManager::SkyDomeGenParameters
Ogre::SceneManager::SkyPlaneGenParameters
Ogre::SceneManager::SkyBoxGenParameters
Ogre::SceneManager::Listener
Ogre::SceneManager::SceneMgrQueuedRenderableVisitor
Ogre::SceneManager::RenderContext
Ogre::SceneManager::prepareShadowTextures(Ogre::Camera*, Ogre::Viewport*, Ogre::HashedVector<Ogre::Light*> const*)
Ogre::SceneManager::_pauseRendering()
Ogre::SceneManager::_resumeRendering(Ogre::SceneManager::RenderContext*)
Ogre::SceneManager::SceneManager(std::string const&)
Ogre::SceneManager::~SceneManager()
Ogre::SceneManager::getName() const
Ogre::SceneManager::getTypeName() const
Ogre::SceneManager::createCamera(std::string const&)
Ogre::SceneManager::getCamera(std::string const&) const
Ogre::SceneManager::hasCamera(std::string const&) const
Ogre::SceneManager::destroyCamera(Ogre::Camera*)
Ogre::SceneManager::destroyCamera(std::string const&)
Ogre::SceneManager::destroyAllCameras()
Ogre::SceneManager::createLight(std::string const&)
Ogre::SceneManager::createLight()
Ogre::SceneManager::getLight(std::string const&) const
Ogre::SceneManager::hasLight(std::string const&) const
Ogre::SceneManager::getLightClippingPlanes(Ogre::Light*)
Ogre::SceneManager::getLightScissorRect(Ogre::Light*, Ogre::Camera const*)
Ogre::SceneManager::destroyLight(std::string const&)
Ogre::SceneManager::destroyLight(Ogre::Light*)
Ogre::SceneManager::destroyAllLights()
Ogre::SceneManager::_notifyLightsDirty()
Ogre::SceneManager::_getLightsDirtyCounter() const
Ogre::SceneManager::_getLightsAffectingFrustum() const
Ogre::SceneManager::_populateLightList(Ogre::Vector3 const&, float, Ogre::HashedVector<Ogre::Light*>&, unsigned int)
Ogre::SceneManager::_populateLightList(Ogre::SceneNode const*, float, Ogre::HashedVector<Ogre::Light*>&, unsigned int)
Ogre::SceneManager::createSceneNode()
Ogre::SceneManager::createSceneNode(std::string const&)
Ogre::SceneManager::destroySceneNode(std::string const&)
Ogre::SceneManager::destroySceneNode(Ogre::SceneNode*)
Ogre::SceneManager::getRootSceneNode()
Ogre::SceneManager::getSceneNode(std::string const&) const
Ogre::SceneManager::hasSceneNode(std::string const&) const
Ogre::SceneManager::createEntity(std::string const&, std::string const&, std::string const&)
Ogre::SceneManager::createEntity(std::string const&)
Ogre::SceneManager::createEntity(std::string const&, Ogre::SceneManager::PrefabType)
Ogre::SceneManager::createEntity(Ogre::SceneManager::PrefabType)
Ogre::SceneManager::getEntity(std::string const&) const
Ogre::SceneManager::hasEntity(std::string const&) const
Ogre::SceneManager::destroyEntity(Ogre::Entity*)
Ogre::SceneManager::destroyEntity(std::string const&)
Ogre::SceneManager::destroyAllEntities()
Ogre::SceneManager::createManualObject(std::string const&)
Ogre::SceneManager::createManualObject()
Ogre::SceneManager::getManualObject(std::string const&) const
Ogre::SceneManager::hasManualObject(std::string const&) const
Ogre::SceneManager::destroyManualObject(Ogre::ManualObject*)
Ogre::SceneManager::destroyManualObject(std::string const&)
Ogre::SceneManager::destroyAllManualObjects()
Ogre::SceneManager::createBillboardChain(std::string const&)
Ogre::SceneManager::createBillboardChain()
Ogre::SceneManager::getBillboardChain(std::string const&) const
Ogre::SceneManager::hasBillboardChain(std::string const&) const
Ogre::SceneManager::destroyBillboardChain(Ogre::BillboardChain*)
Ogre::SceneManager::destroyBillboardChain(std::string const&)
Ogre::SceneManager::destroyAllBillboardChains()
Ogre::SceneManager::createRibbonTrail(std::string const&)
Ogre::SceneManager::createRibbonTrail()
Ogre::SceneManager::getRibbonTrail(std::string const&) const
Ogre::SceneManager::hasRibbonTrail(std::string const&) const
Ogre::SceneManager::destroyRibbonTrail(Ogre::RibbonTrail*)
Ogre::SceneManager::destroyRibbonTrail(std::string const&)
Ogre::SceneManager::destroyAllRibbonTrails()
Ogre::SceneManager::createParticleSystem(std::string const&, std::string const&)
Ogre::SceneManager::createParticleSystem(std::string const&, unsigned int, std::string const&)
Ogre::SceneManager::createParticleSystem(unsigned int, std::string const&)
Ogre::SceneManager::getParticleSystem(std::string const&) const
Ogre::SceneManager::hasParticleSystem(std::string const&) const
Ogre::SceneManager::destroyParticleSystem(Ogre::ParticleSystem*)
Ogre::SceneManager::destroyParticleSystem(std::string const&)
Ogre::SceneManager::destroyAllParticleSystems()
Ogre::SceneManager::clearScene()
Ogre::SceneManager::setAmbientLight(Ogre::ColourValue const&)
Ogre::SceneManager::getAmbientLight() const
Ogre::SceneManager::prepareWorldGeometry(std::string const&)
Ogre::SceneManager::prepareWorldGeometry(Ogre::SharedPtr<Ogre::DataStream>&, std::string const&)
Ogre::SceneManager::setWorldGeometry(std::string const&)
Ogre::SceneManager::setWorldGeometry(Ogre::SharedPtr<Ogre::DataStream>&, std::string const&)
Ogre::SceneManager::estimateWorldGeometry(std::string const&)
Ogre::SceneManager::estimateWorldGeometry(Ogre::SharedPtr<Ogre::DataStream>&, std::string const&)
Ogre::SceneManager::getSuggestedViewpoint(bool)
Ogre::SceneManager::setOption(std::string const&, void const*)
Ogre::SceneManager::getOption(std::string const&, void*)
Ogre::SceneManager::hasOption(std::string const&) const
Ogre::SceneManager::getOptionValues(std::string const&, std::vector<std::string, Ogre::STLAllocator<std::string, Ogre::CategorisedAllocPolicy<(Ogre::MemoryCategory)0> > >&)
Ogre::SceneManager::getOptionKeys(std::vector<std::string, Ogre::STLAllocator<std::string, Ogre::CategorisedAllocPolicy<(Ogre::MemoryCategory)0> > >&)
Ogre::SceneManager::_updateSceneGraph(Ogre::Camera*)
Ogre::SceneManager::_findVisibleObjects(Ogre::Camera*, Ogre::VisibleObjectsBoundsInfo*, bool)
Ogre::SceneManager::_applySceneAnimations()
Ogre::SceneManager::_renderVisibleObjects()
Ogre::SceneManager::_renderScene(Ogre::Camera*, Ogre::Viewport*, bool)
Ogre::SceneManager::_queueSkiesForRendering(Ogre::Camera*)
Ogre::SceneManager::_setDestinationRenderSystem(Ogre::RenderSystem*)
Ogre::SceneManager::setSkyPlane(bool, Ogre::Plane const&, std::string const&, float, float, bool, float, int, int, std::string const&)
Ogre::SceneManager::_setSkyPlane(bool, Ogre::Plane const&, std::string const&, float, float, unsigned char, float, int, int, std::string const&)
Ogre::SceneManager::isSkyPlaneEnabled() const
Ogre::SceneManager::getSkyPlaneNode() const
Ogre::SceneManager::getSkyPlaneGenParameters() const
Ogre::SceneManager::setSkyBox(bool, std::string const&, float, bool, Ogre::Quaternion const&, std::string const&)
Ogre::SceneManager::_setSkyBox(bool, std::string const&, float, unsigned char, Ogre::Quaternion const&, std::string const&)
Ogre::SceneManager::isSkyBoxEnabled() const
Ogre::SceneManager::getSkyBoxNode() const
Ogre::SceneManager::getSkyBoxGenParameters() const
Ogre::SceneManager::setSkyDome(bool, std::string const&, float, float, float, bool, Ogre::Quaternion const&, int, int, int, std::string const&)
Ogre::SceneManager::_setSkyDome(bool, std::string const&, float, float, float, unsigned char, Ogre::Quaternion const&, int, int, int, std::string const&)
Ogre::SceneManager::isSkyDomeEnabled() const
Ogre::SceneManager::getSkyDomeNode() const
Ogre::SceneManager::getSkyDomeGenParameters() const
Ogre::SceneManager::setFog(Ogre::FogMode, Ogre::ColourValue const&, float, float, float)
Ogre::SceneManager::getFogMode() const
Ogre::SceneManager::getFogColour() const
Ogre::SceneManager::getFogStart() const
Ogre::SceneManager::getFogEnd() const
Ogre::SceneManager::getFogDensity() const
Ogre::SceneManager::createBillboardSet(std::string const&, unsigned int)
Ogre::SceneManager::createBillboardSet(unsigned int)
Ogre::SceneManager::getBillboardSet(std::string const&) const
Ogre::SceneManager::hasBillboardSet(std::string const&) const
Ogre::SceneManager::destroyBillboardSet(Ogre::BillboardSet*)
Ogre::SceneManager::destroyBillboardSet(std::string const&)
Ogre::SceneManager::destroyAllBillboardSets()
Ogre::SceneManager::setDisplaySceneNodes(bool)
Ogre::SceneManager::getDisplaySceneNodes() const
Ogre::SceneManager::createAnimation(std::string const&, float)
Ogre::SceneManager::getAnimation(std::string const&) const
Ogre::SceneManager::hasAnimation(std::string const&) const
Ogre::SceneManager::destroyAnimation(std::string const&)
Ogre::SceneManager::destroyAllAnimations()
Ogre::SceneManager::createAnimationState(std::string const&)
Ogre::SceneManager::getAnimationState(std::string const&) const
Ogre::SceneManager::hasAnimationState(std::string const&) const
Ogre::SceneManager::destroyAnimationState(std::string const&)
Ogre::SceneManager::destroyAllAnimationStates()
Ogre::SceneManager::manualRender(Ogre::RenderOperation*, Ogre::Pass*, Ogre::Viewport*, Ogre::Matrix4 const&, Ogre::Matrix4 const&, Ogre::Matrix4 const&, bool)
Ogre::SceneManager::manualRender(Ogre::Renderable*, Ogre::Pass const*, Ogre::Viewport*, Ogre::Matrix4 const&, Ogre::Matrix4 const&, bool, bool, bool, Ogre::HashedVector<Ogre::Light*> const*)
Ogre::SceneManager::getRenderQueue()
Ogre::SceneManager::addRenderQueueListener(Ogre::RenderQueueListener*)
Ogre::SceneManager::removeRenderQueueListener(Ogre::RenderQueueListener*)
Ogre::SceneManager::addRenderObjectListener(Ogre::RenderObjectListener*)
Ogre::SceneManager::removeRenderObjectListener(Ogre::RenderObjectListener*)
Ogre::SceneManager::addSpecialCaseRenderQueue(unsigned char)
Ogre::SceneManager::removeSpecialCaseRenderQueue(unsigned char)
Ogre::SceneManager::clearSpecialCaseRenderQueues()
Ogre::SceneManager::setSpecialCaseRenderQueueMode(Ogre::SceneManager::SpecialCaseRenderQueueMode)
Ogre::SceneManager::getSpecialCaseRenderQueueMode()
Ogre::SceneManager::isRenderQueueToBeProcessed(unsigned char)
Ogre::SceneManager::setWorldGeometryRenderQueue(unsigned char)
Ogre::SceneManager::getWorldGeometryRenderQueue()
Ogre::SceneManager::showBoundingBoxes(bool)
Ogre::SceneManager::getShowBoundingBoxes() const
Ogre::SceneManager::_notifyAutotrackingSceneNode(Ogre::SceneNode*, bool)
Ogre::SceneManager::createAABBQuery(Ogre::AxisAlignedBox const&, unsigned long)
Ogre::SceneManager::createSphereQuery(Ogre::Sphere const&, unsigned long)
Ogre::SceneManager::createPlaneBoundedVolumeQuery(std::vector<Ogre::PlaneBoundedVolume, Ogre::STLAllocator<Ogre::PlaneBoundedVolume, Ogre::CategorisedAllocPolicy<(Ogre::MemoryCategory)0> > > const&, unsigned long)
Ogre::SceneManager::createRayQuery(Ogre::Ray const&, unsigned long)
Ogre::SceneManager::createIntersectionQuery(unsigned long)
Ogre::SceneManager::destroyQuery(Ogre::SceneQuery*)
Ogre::SceneManager::getCameraIterator()
Ogre::SceneManager::getCameras() const
Ogre::SceneManager::getAnimationIterator()
Ogre::SceneManager::getAnimations() const
Ogre::SceneManager::getAnimationStateIterator()
Ogre::SceneManager::setShadowTechnique(Ogre::ShadowTechnique)
Ogre::SceneManager::getShadowTechnique() const
Ogre::SceneManager::setShowDebugShadows(bool)
Ogre::SceneManager::getShowDebugShadows() const
Ogre::SceneManager::setShadowColour(Ogre::ColourValue const&)
Ogre::SceneManager::getShadowColour() const
Ogre::SceneManager::setShadowDirectionalLightExtrusionDistance(float)
Ogre::SceneManager::getShadowDirectionalLightExtrusionDistance() const
Ogre::SceneManager::setShadowFarDistance(float)
Ogre::SceneManager::getShadowFarDistance() const
Ogre::SceneManager::getShadowFarDistanceSquared() const
Ogre::SceneManager::setShadowIndexBufferSize(unsigned int)
Ogre::SceneManager::getShadowIndexBufferSize() const
Ogre::SceneManager::setShadowTextureSize(unsigned short)
Ogre::SceneManager::setShadowTextureConfig(unsigned int, unsigned short, unsigned short, Ogre::PixelFormat, unsigned short)
Ogre::SceneManager::setShadowTextureConfig(unsigned int, Ogre::ShadowTextureConfig const&)
Ogre::SceneManager::getShadowTextureConfigIterator() const
Ogre::SceneManager::setShadowTexturePixelFormat(Ogre::PixelFormat)
Ogre::SceneManager::setShadowTextureCount(unsigned int)
Ogre::SceneManager::getShadowTextureCount() const
Ogre::SceneManager::setShadowTextureCountPerLightType(Ogre::Light::LightTypes, unsigned int)
Ogre::SceneManager::getShadowTextureCountPerLightType(Ogre::Light::LightTypes) const
Ogre::SceneManager::setShadowTextureSettings(unsigned short, unsigned short, Ogre::PixelFormat, unsigned short)
Ogre::SceneManager::getShadowTexture(unsigned int)
Ogre::SceneManager::setShadowDirLightTextureOffset(float)
Ogre::SceneManager::getShadowDirLightTextureOffset() const
Ogre::SceneManager::setShadowTextureFadeStart(float)
Ogre::SceneManager::setShadowTextureFadeEnd(float)
Ogre::SceneManager::setShadowTextureSelfShadow(bool)
Ogre::SceneManager::getShadowTextureSelfShadow() const
Ogre::SceneManager::setShadowTextureCasterMaterial(std::string const&)
Ogre::SceneManager::setShadowTextureReceiverMaterial(std::string const&)
Ogre::SceneManager::setShadowCasterRenderBackFaces(bool)
Ogre::SceneManager::getShadowCasterRenderBackFaces() const
Ogre::SceneManager::setShadowCameraSetup(Ogre::SharedPtr<Ogre::ShadowCameraSetup> const&)
Ogre::SceneManager::getShadowCameraSetup() const
Ogre::SceneManager::setShadowUseInfiniteFarPlane(bool)
Ogre::SceneManager::isShadowTechniqueStencilBased() const
Ogre::SceneManager::isShadowTechniqueTextureBased() const
Ogre::SceneManager::isShadowTechniqueModulative() const
Ogre::SceneManager::isShadowTechniqueAdditive() const
Ogre::SceneManager::isShadowTechniqueIntegrated() const
Ogre::SceneManager::isShadowTechniqueInUse() const
Ogre::SceneManager::setShadowUseLightClipPlanes(bool)
Ogre::SceneManager::getShadowUseLightClipPlanes() const
Ogre::SceneManager::_setActiveCompositorChain(Ogre::CompositorChain*)
Ogre::SceneManager::setLateMaterialResolving(bool)
Ogre::SceneManager::isLateMaterialResolving() const
Ogre::SceneManager::_getActiveCompositorChain() const
Ogre::SceneManager::addListener(Ogre::SceneManager::Listener*)
Ogre::SceneManager::removeListener(Ogre::SceneManager::Listener*)
Ogre::SceneManager::createStaticGeometry(std::string const&)
Ogre::SceneManager::getStaticGeometry(std::string const&) const
Ogre::SceneManager::hasStaticGeometry(std::string const&) const
Ogre::SceneManager::destroyStaticGeometry(Ogre::StaticGeometry*)
Ogre::SceneManager::destroyStaticGeometry(std::string const&)
Ogre::SceneManager::destroyAllStaticGeometry()
Ogre::SceneManager::createInstancedGeometry(std::string const&)
Ogre::SceneManager::getInstancedGeometry(std::string const&) const
Ogre::SceneManager::destroyInstancedGeometry(Ogre::InstancedGeometry*)
Ogre::SceneManager::destroyInstancedGeometry(std::string const&)
Ogre::SceneManager::destroyAllInstancedGeometry()
Ogre::SceneManager::createInstanceManager(std::string const&, std::string const&, std::string const&, Ogre::InstanceManager::InstancingTechnique, unsigned int, unsigned short)
Ogre::SceneManager::destroyInstanceManager(std::string const&)
Ogre::SceneManager::destroyInstanceManager(Ogre::InstanceManager*)
Ogre::SceneManager::destroyAllInstanceManagers()
Ogre::SceneManager::getNumInstancesPerBatch(std::string const&, std::string const&, std::string const&, Ogre::InstanceManager::InstancingTechnique, unsigned int, unsigned short)
Ogre::SceneManager::createInstancedEntity(std::string const&, std::string const&)
Ogre::SceneManager::destroyInstancedEntity(Ogre::InstancedEntity*)
Ogre::SceneManager::_addDirtyInstanceManager(Ogre::InstanceManager*)
Ogre::SceneManager::createMovableObject(std::string const&, std::string const&, std::map<std::string, std::string, std::less<std::string>, Ogre::STLAllocator<std::pair<std::string const, std::string>, Ogre::CategorisedAllocPolicy<(Ogre::MemoryCategory)0> > > const*)
Ogre::SceneManager::createMovableObject(std::string const&, std::map<std::string, std::string, std::less<std::string>, Ogre::STLAllocator<std::pair<std::string const, std::string>, Ogre::CategorisedAllocPolicy<(Ogre::MemoryCategory)0> > > const*)
Ogre::SceneManager::destroyMovableObject(std::string const&, std::string const&)
Ogre::SceneManager::destroyMovableObject(Ogre::MovableObject*)
Ogre::SceneManager::destroyAllMovableObjectsByType(std::string const&)
Ogre::SceneManager::destroyAllMovableObjects()
Ogre::SceneManager::getMovableObject(std::string const&, std::string const&) const
Ogre::SceneManager::hasMovableObject(std::string const&, std::string const&) const
Ogre::SceneManager::getMovableObjectIterator(std::string const&)
Ogre::SceneManager::injectMovableObject(Ogre::MovableObject*)
Ogre::SceneManager::extractMovableObject(std::string const&, std::string const&)
Ogre::SceneManager::extractMovableObject(Ogre::MovableObject*)
Ogre::SceneManager::extractAllMovableObjectsByType(std::string const&)
Ogre::SceneManager::setVisibilityMask(unsigned int)
Ogre::SceneManager::getVisibilityMask()
Ogre::SceneManager::_getCombinedVisibilityMask() const
Ogre::SceneManager::setFindVisibleObjects(bool)
Ogre::SceneManager::getFindVisibleObjects()
Ogre::SceneManager::setNormaliseNormalsOnScale(bool)
Ogre::SceneManager::getNormaliseNormalsOnScale() const
Ogre::SceneManager::setFlipCullingOnNegativeScale(bool)
Ogre::SceneManager::getFlipCullingOnNegativeScale() const
Ogre::SceneManager::_injectRenderWithPass(Ogre::Pass*, Ogre::Renderable*, bool, bool, Ogre::HashedVector<Ogre::Light*> const*)
Ogre::SceneManager::_suppressRenderStateChanges(bool)
Ogre::SceneManager::_areRenderStateChangesSuppressed() const
Ogre::SceneManager::_setPass(Ogre::Pass const*, bool, bool)
Ogre::SceneManager::_suppressShadows(bool)
Ogre::SceneManager::_areShadowsSuppressed() const
Ogre::SceneManager::_renderQueueGroupObjects(Ogre::RenderQueueGroup*, Ogre::QueuedRenderableCollection::OrganisationMode)
Ogre::SceneManager::setQueuedRenderableVisitor(Ogre::SceneManager::SceneMgrQueuedRenderableVisitor*)
Ogre::SceneManager::getQueuedRenderableVisitor() const
Ogre::SceneManager::getDestinationRenderSystem()
Ogre::SceneManager::getCurrentViewport() const
Ogre::SceneManager::getVisibleObjectsBoundsInfo(Ogre::Camera const*) const
Ogre::SceneManager::getShadowCasterBoundsInfo(Ogre::Light const*, unsigned int) const
Ogre::SceneManager::setCameraRelativeRendering(bool)
Ogre::SceneManager::getCameraRelativeRendering() const
Ogre::SceneManager::addLodListener(Ogre::LodListener*)
Ogre::SceneManager::removeLodListener(Ogre::LodListener*)
Ogre::SceneManager::_notifyMovableObjectLodChanged(Ogre::MovableObjectLodChangedEvent&)
Ogre::SceneManager::_notifyEntityMeshLodChanged(Ogre::EntityMeshLodChangedEvent&)
Ogre::SceneManager::_notifyEntityMaterialLodChanged(Ogre::EntityMaterialLodChangedEvent&)
Ogre::SceneManager::_handleLodEvents()
Ogre::SceneManager::materialLess::~materialLess()
Ogre::SceneManager::materialLess::operator=(Ogre::SceneManager::materialLess const&)
Ogre::SceneManager::materialLess::materialLess(Ogre::SceneManager::materialLess const&)
Ogre::SceneManager::materialLess::materialLess()
Ogre::SceneManager::materialLess::operator()(Ogre::Material const*, Ogre::Material const*) const
Ogre::SceneManager::lightLess::~lightLess()
Ogre::SceneManager::lightLess::operator=(Ogre::SceneManager::lightLess const&)
Ogre::SceneManager::lightLess::lightLess(Ogre::SceneManager::lightLess const&)
Ogre::SceneManager::lightLess::lightLess()
Ogre::SceneManager::lightLess::operator()(Ogre::Light const*, Ogre::Light const*) const
Ogre::SceneManager::SkyDomeGenParameters::~SkyDomeGenParameters()
Ogre::SceneManager::SkyDomeGenParameters::operator=(Ogre::SceneManager::SkyDomeGenParameters const&)
Ogre::SceneManager::SkyDomeGenParameters::SkyDomeGenParameters(Ogre::SceneManager::SkyDomeGenParameters const&)
Ogre::SceneManager::SkyDomeGenParameters::SkyDomeGenParameters()
Ogre::SceneManager::SkyPlaneGenParameters::~SkyPlaneGenParameters()
Ogre::SceneManager::SkyPlaneGenParameters::operator=(Ogre::SceneManager::SkyPlaneGenParameters const&)
Ogre::SceneManager::SkyPlaneGenParameters::SkyPlaneGenParameters(Ogre::SceneManager::SkyPlaneGenParameters const&)
Ogre::SceneManager::SkyPlaneGenParameters::SkyPlaneGenParameters()
Ogre::SceneManager::SkyBoxGenParameters::~SkyBoxGenParameters()
Ogre::SceneManager::SkyBoxGenParameters::operator=(Ogre::SceneManager::SkyBoxGenParameters const&)
Ogre::SceneManager::SkyBoxGenParameters::SkyBoxGenParameters(Ogre::SceneManager::SkyBoxGenParameters const&)
Ogre::SceneManager::SkyBoxGenParameters::SkyBoxGenParameters()
Ogre::SceneManager::Listener::operator=(Ogre::SceneManager::Listener const&)
Ogre::SceneManager::Listener::Listener(Ogre::SceneManager::Listener const&)
Ogre::SceneManager::Listener::Listener()
Ogre::SceneManager::Listener::~Listener()
Ogre::SceneManager::Listener::preFindVisibleObjects(Ogre::SceneManager*, Ogre::SceneManager::IlluminationRenderStage, Ogre::Viewport*)
Ogre::SceneManager::Listener::postFindVisibleObjects(Ogre::SceneManager*, Ogre::SceneManager::IlluminationRenderStage, Ogre::Viewport*)
Ogre::SceneManager::Listener::shadowTexturesUpdated(unsigned int)
Ogre::SceneManager::Listener::shadowTextureCasterPreViewProj(Ogre::Light*, Ogre::Camera*, unsigned int)
Ogre::SceneManager::Listener::shadowTextureReceiverPreViewProj(Ogre::Light*, Ogre::Frustum*)
Ogre::SceneManager::Listener::sortLightsAffectingFrustum(Ogre::HashedVector<Ogre::Light*>&)
Ogre::SceneManager::Listener::sceneManagerDestroyed(Ogre::SceneManager*)
Ogre::SceneManager::SceneMgrQueuedRenderableVisitor::operator=(Ogre::SceneManager::SceneMgrQueuedRenderableVisitor const&)
Ogre::SceneManager::SceneMgrQueuedRenderableVisitor::SceneMgrQueuedRenderableVisitor(Ogre::SceneManager::SceneMgrQueuedRenderableVisitor const&)
Ogre::SceneManager::SceneMgrQueuedRenderableVisitor::SceneMgrQueuedRenderableVisitor()
Ogre::SceneManager::SceneMgrQueuedRenderableVisitor::~SceneMgrQueuedRenderableVisitor()
Ogre::SceneManager::SceneMgrQueuedRenderableVisitor::visit(Ogre::Renderable*)
Ogre::SceneManager::SceneMgrQueuedRenderableVisitor::visit(Ogre::Pass const*)
Ogre::SceneManager::SceneMgrQueuedRenderableVisitor::visit(Ogre::RenderablePass*)
Ogre::SceneManager::LightInfo::~LightInfo()
Ogre::SceneManager::LightInfo::operator=(Ogre::SceneManager::LightInfo const&)
Ogre::SceneManager::LightInfo::LightInfo(Ogre::SceneManager::LightInfo const&)
Ogre::SceneManager::LightInfo::LightInfo()
Ogre::SceneManager::LightInfo::operator==(Ogre::SceneManager::LightInfo const&) const
Ogre::SceneManager::LightInfo::operator!=(Ogre::SceneManager::LightInfo const&) const
Ogre::SceneManager::MovableObjectCollection::~MovableObjectCollection()
Ogre::SceneManager::MovableObjectCollection::MovableObjectCollection()
Ogre::SceneManager::LightClippingInfo::~LightClippingInfo()
Ogre::SceneManager::LightClippingInfo::operator=(Ogre::SceneManager::LightClippingInfo const&)
Ogre::SceneManager::LightClippingInfo::LightClippingInfo(Ogre::SceneManager::LightClippingInfo const&)
Ogre::SceneManager::LightClippingInfo::LightClippingInfo()
Ogre::SceneManager::lightsForShadowTextureLess::~lightsForShadowTextureLess()
Ogre::SceneManager::lightsForShadowTextureLess::operator=(Ogre::SceneManager::lightsForShadowTextureLess const&)
Ogre::SceneManager::lightsForShadowTextureLess::lightsForShadowTextureLess(Ogre::SceneManager::lightsForShadowTextureLess const&)
Ogre::SceneManager::lightsForShadowTextureLess::lightsForShadowTextureLess()
Ogre::SceneManager::lightsForShadowTextureLess::operator()(Ogre::Light const*, Ogre::Light const*) const
Ogre::SceneManager::RenderContext::~RenderContext()
Ogre::SceneManager::RenderContext::operator=(Ogre::SceneManager::RenderContext const&)
Ogre::SceneManager::RenderContext::RenderContext(Ogre::SceneManager::RenderContext const&)
Ogre::SceneManager::RenderContext::RenderContext()
Ogre::SceneManager::ShadowCasterSceneQueryListener::operator=(Ogre::SceneManager::ShadowCasterSceneQueryListener const&)
Ogre::SceneManager::ShadowCasterSceneQueryListener::ShadowCasterSceneQueryListener(Ogre::SceneManager::ShadowCasterSceneQueryListener const&)
Ogre::SceneManager::ShadowCasterSceneQueryListener::ShadowCasterSceneQueryListener(Ogre::SceneManager*)
Ogre::SceneManager::ShadowCasterSceneQueryListener::prepare(bool, std::vector<Ogre::PlaneBoundedVolume, Ogre::STLAllocator<Ogre::PlaneBoundedVolume, Ogre::CategorisedAllocPolicy<(Ogre::MemoryCategory)0> > > const*, Ogre::Light const*, Ogre::Camera const*, std::vector<Ogre::ShadowCaster*, Ogre::STLAllocator<Ogre::ShadowCaster*, Ogre::CategorisedAllocPolicy<(Ogre::MemoryCategory)0> > >*, float)
Ogre::SceneManager::ShadowCasterSceneQueryListener::queryResult(Ogre::MovableObject*)
Ogre::SceneManager::ShadowCasterSceneQueryListener::queryResult(Ogre::SceneQuery::WorldFragment*)
Ogre::SceneManager::ShadowCasterSceneQueryListener::~ShadowCasterSceneQueryListener()
Ogre::VisibleObjectsBoundsInfo::~VisibleObjectsBoundsInfo()
Ogre::VisibleObjectsBoundsInfo::operator=(Ogre::VisibleObjectsBoundsInfo const&)
Ogre::VisibleObjectsBoundsInfo::VisibleObjectsBoundsInfo(Ogre::VisibleObjectsBoundsInfo const&)
Ogre::VisibleObjectsBoundsInfo::VisibleObjectsBoundsInfo()
Ogre::VisibleObjectsBoundsInfo::reset()
Ogre::VisibleObjectsBoundsInfo::merge(Ogre::AxisAlignedBox const&, Ogre::Sphere const&, Ogre::Camera const*, bool)
Ogre::VisibleObjectsBoundsInfo::mergeNonRenderedButInFrustum(Ogre::AxisAlignedBox const&, Ogre::Sphere const&, Ogre::Camera const*)
Ogre::ViewPoint::~ViewPoint()
Ogre::ViewPoint::operator=(Ogre::ViewPoint const&)
Ogre::ViewPoint::ViewPoint(Ogre::ViewPoint const&)
Ogre::ViewPoint::ViewPoint()
*/