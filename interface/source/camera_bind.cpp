/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "camera_bind.h"
#include "binding_utils.h"

#include <OgreCamera.h>

void camera_set_near_clip_distance(CameraHandle camera_handle, float d)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(camera_handle);
    camera->setNearClipDistance( d );
}

void camera_set_far_clip_distance(CameraHandle camera_handle, float d)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(camera_handle);
    camera->setFarClipDistance( d );
}

void camera_set_aspect_ratio(CameraHandle camera_handle, float w, float h)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(camera_handle);
    camera->setAspectRatio(Ogre::Real(Ogre::Real(w)/Ogre::Real(h)));
}

void camera_set_auto_aspect_ratio(CameraHandle camera_handle, bool on)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(camera_handle);
    camera->setAutoAspectRatio(on);
}

void camera_set_fovy(CameraHandle camera_handle, float angle)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(camera_handle);
    camera->setFOVy((Ogre::Radian)angle);
}

void camera_set_frustum_offset(CameraHandle camera_handle, const int offset_x, const int offset_y)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(camera_handle);
    camera->setFrustumOffset(Ogre::Vector2(offset_x, offset_y));
}

void camera_set_focal_length(CameraHandle camera_handle, float fl)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(camera_handle);
    camera->setFocalLength(fl);
}

void camera_set_position(CameraHandle camera_handle, const float x, const float y, const float z)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(camera_handle);
    camera->setPosition(Ogre::Vector3(x, y, z));
}

void camera_get_position(CameraHandle handle, coiVector3* result)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(handle);
    const Ogre::Vector3& pos = camera->getPosition();
    result->x = pos.x;
    result->y = pos.y;
    result->z = pos.z;
}

// Ogre::Camera::lookAt(Ogre::Vector3 const&)
// Ogre::Camera::lookAt(float, float, float)
void camera_lookat(CameraHandle camera_handle, const float x, const float y, const float z)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(camera_handle);
    camera->lookAt(Ogre::Vector3(x, y, z));
}

//void setPolygonMode(PolygonMode sd)
void camera_set_polygon_mode(CameraHandle handle, polygon_mode sd)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(handle);
    camera->setPolygonMode(enum_converter(sd));
}

//PolygonMode getPolygonMode() const
polygon_mode camera_get_polygon_mode(const CameraHandle handle)
{
    const Ogre::Camera* cam = static_cast<const Ogre::Camera*>(handle);
    return enum_converter(cam->getPolygonMode());
}

//Ogre::Camera::move(Ogre::Vector3 const&)
void camera_move(CameraHandle handle, const float x, const float y, const float z)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(handle);
    camera->move(Ogre::Vector3(x, y, z));
}

//Ogre::Camera::moveRelative(Ogre::Vector3 const&)
void camera_move_relative(CameraHandle handle, const float x, const float y, const float z)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(handle);
    camera->moveRelative(Ogre::Vector3(x, y, z));
}

//Ogre::Frustum::setAspectRatio(float)
void camera_set_aspect_ratio_ex(CameraHandle handle, float ratio)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(handle);
    camera->setAspectRatio(Ogre::Real(ratio));
}

//Ogre::Frustum::getAspectRatio() const
float camera_get_aspect_ratio(CameraHandle handle)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(handle);
    return camera->getAspectRatio();
}

//Ogre::Camera::setDirection(float, float, float)
//Ogre::Camera::setDirection(Ogre::Vector3 const&)
void camera_set_direction(CameraHandle handle, const float x, const float y, const float z)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(handle);
    camera->setDirection(Ogre::Vector3(x, y, z));
}

// Ogre::Camera::getDirection() const
void camera_get_direction(CameraHandle handle, coiVector3* v3)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(handle);
    const Ogre::Vector3& v = camera->getDirection();
    v3->x = v.x;
    v3->y = v.y;
    v3->z = v.z;
}

//Ogre::Camera::roll(Ogre::Radian const&)
void camera_roll(CameraHandle handle, coiReal angle)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(handle);
    const Ogre::Radian r(angle);
    camera->roll(r);
}

//Ogre::Camera::yaw(Ogre::Radian const&)
void camera_yaw(CameraHandle handle, coiReal angle)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(handle);
    const Ogre::Radian r(angle);
    camera->yaw(r);
}

//Ogre::Camera::pitch(Ogre::Radian const&)
void camera_pitch(CameraHandle handle, coiReal angle)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(handle);
    const Ogre::Radian r(angle);
    camera->pitch(r);
}

//Ogre::Camera::rotate(Ogre::Vector3 const&, Ogre::Radian const&)
void camera_rotate(CameraHandle handle, const coiVector3* axis, coiReal angle)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(handle);
    const Ogre::Vector3 v(axis->x, axis->y, axis->z);
    Ogre::Radian r(angle);

    camera->rotate(v, r);
}

//Ogre::Camera::rotate(Ogre::Quaternion const&)
void camera_rotate_q(CameraHandle handle, const coiQuaternion* q)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(handle);
    const Ogre::Quaternion quat(q->w, q->x, q->y, q->z);

    camera->rotate(quat);
}

//Ogre::Camera::getSceneManager() const
SceneManagerHandle camera_get_scenemanager(CameraHandle handle)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(handle);
    return static_cast<SceneManagerHandle>(camera->getSceneManager());
    
}

// Ogre::Camera::getUp() const
void camera_get_up(CameraHandle handle, coiVector3* up)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(handle);
    const Ogre::Vector3& u = camera->getUp();

    up->x = u.x;
    up->y = u.y;
    up->z = u.z;
}

//Ogre::Camera::getRight() const
void camera_get_right(CameraHandle handle, coiVector3* right)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(handle);
    const Ogre::Vector3& u = camera->getRight();

    right->x = u.x;
    right->y = u.y;
    right->z = u.z;
}

//Ogre::Camera::setFixedYawAxis(bool, Ogre::Vector3 const&)
void camera_set_fixed_yaw_axis(CameraHandle handle, int on, const coiVector3* fixed_axis)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(handle);
    Ogre::Vector3 axis(fixed_axis->x, fixed_axis->y, fixed_axis->z);
    camera->setFixedYawAxis(on, axis);
}

//Ogre::Camera::getOrientation() const
void camera_get_orientation(CameraHandle handle, coiQuaternion* orientation)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(handle);
    const Ogre::Quaternion& getter = camera->getOrientation();

    orientation->w = getter.w;
    orientation->x = getter.x;
    orientation->y = getter.y;
    orientation->z = getter.z;

}

//Ogre::Camera::setOrientation(Ogre::Quaternion const&)
void camera_set_orientation(CameraHandle handle, const coiQuaternion* orientation)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(handle);
    const Ogre::Quaternion setter(orientation->w, orientation->x, orientation->y, orientation->z);
    camera->setOrientation(setter);
}

//Ogre::Camera::getDerivedOrientation() const
void camera_get_derived_orientation(CameraHandle handle, coiQuaternion* orientation)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(handle);
    const Ogre::Quaternion & getter = camera->getDerivedOrientation();

    orientation->w = getter.w;
    orientation->x = getter.x;
    orientation->y = getter.y;
    orientation->z = getter.z;
}

//Ogre::Camera::getDerivedPosition() const
void camera_get_derived_position(CameraHandle handle, coiVector3* position)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(handle);
    const Ogre::Vector3& getter = camera->getDerivedPosition();

    position->x = getter.x;
    position->y = getter.y;
    position->z = getter.z;
}

//Ogre::Camera::getDerivedDirection() const
void camera_get_derived_direction(CameraHandle handle, coiVector3* direction)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(handle);
    const Ogre::Vector3& getter = camera->getDerivedDirection();

    direction->x = getter.x;
    direction->y = getter.y;
    direction->z = getter.z;
}

//Ogre::Camera::getDerivedUp() const
void camera_get_derived_up(CameraHandle handle, coiVector3* up)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(handle);
    const Ogre::Vector3& getter = camera->getDerivedUp();

    up->x = getter.x;
    up->y = getter.y;
    up->z = getter.z;
}

//Ogre::Camera::getDerivedRight() const
void camera_get_derived_right(CameraHandle handle, coiVector3* right)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(handle);
    const Ogre::Vector3& getter = camera->getDerivedRight();

    right->x = getter.x;
    right->y = getter.y;
    right->z = getter.z;
}

//Ogre::Camera::setAutoTracking(bool, Ogre::SceneNode*, Ogre::Vector3 const&)
void camera_set_autotracking(CameraHandle handle, int on, SceneNodeHandle sn_handle, const coiVector3* offset)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(handle);
    Ogre::SceneNode* node= static_cast<Ogre::SceneNode*>(sn_handle);
    const Ogre::Vector3 setter(offset->x, offset->y, offset->z);
    camera->setAutoTracking(on, node, setter);
    
}

//Ogre::Camera::setLodBias(float)
void camera_set_lod_bias(CameraHandle handle, coiReal factor)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(handle);
    camera->setLodBias(factor);
}

//Ogre::Camera::getLodBias() const
coiReal camera_get_lod_bias(CameraHandle handle)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(handle);
    return camera->getLodBias();
}

//Ogre::Camera::getCameraToViewportRay(float, float, Ogre::Ray*) const
void camera_get_camera_to_viewport_ray(CameraHandle handle, coiReal screenx, coiReal screeny, RayHandle result)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(handle);
    Ogre::Ray* ray = static_cast<Ogre::Ray*>(result);
    camera->getCameraToViewportRay(screenx, screeny, ray);
}

//Ogre::Camera::setWindow(float, float, float, float)
void camera_set_window(CameraHandle handle, coiReal left, coiReal top, coiReal right, coiReal bottom)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(handle);
    camera->setWindow(left, top, right, bottom);
}

/*
Ogre::Camera::Listener
Ogre::Camera::Camera(std::string const&, Ogre::SceneManager*)
Ogre::Camera::~Camera()
Ogre::Camera::addListener(Ogre::Camera::Listener*)
Ogre::Camera::removeListener(Ogre::Camera::Listener*)
Ogre::Camera::setPolygonMode(Ogre::PolygonMode)
Ogre::Camera::getPolygonMode() const
Ogre::Camera::getPosition() const
Ogre::Camera::_renderScene(Ogre::Viewport*, bool)
Ogre::Camera::_notifyRenderedFaces(unsigned int)
Ogre::Camera::_notifyRenderedBatches(unsigned int)
Ogre::Camera::_getNumRenderedFaces() const
Ogre::Camera::_getNumRenderedBatches() const
Ogre::Camera::getRealOrientation() const
Ogre::Camera::getRealPosition() const
Ogre::Camera::getRealDirection() const
Ogre::Camera::getRealUp() const
Ogre::Camera::getRealRight() const
Ogre::Camera::getMovableType() const
Ogre::Camera::setLodCamera(Ogre::Camera const*)
Ogre::Camera::getLodCamera() const
Ogre::Camera::getCameraToViewportRay(float, float, Ogre::Ray*) const
Ogre::Camera::getCameraToViewportBoxVolume(float, float, float, float, bool)
Ogre::Camera::getCameraToViewportBoxVolume(float, float, float, float, Ogre::PlaneBoundedVolume*, bool)
Ogre::Camera::_getLodBiasInverse() const
Ogre::Camera::_autoTrack()
Ogre::Camera::resetWindow()
Ogre::Camera::isWindowSet() const
Ogre::Camera::getWindowPlanes() const
Ogre::Camera::getBoundingRadius() const
Ogre::Camera::getAutoTrackTarget() const
Ogre::Camera::getAutoTrackOffset() const
Ogre::Camera::getViewport() const
Ogre::Camera::_notifyViewport(Ogre::Viewport*)
Ogre::Camera::setAutoAspectRatio(bool)
Ogre::Camera::getAutoAspectRatio() const
Ogre::Camera::setCullingFrustum(Ogre::Frustum*)
Ogre::Camera::getCullingFrustum() const
Ogre::Camera::forwardIntersect(Ogre::Plane const&, std::vector<Ogre::Vector4, Ogre::STLAllocator<Ogre::Vector4, Ogre::CategorisedAllocPolicy<(Ogre::MemoryCategory)0> > >*) const
Ogre::Camera::isVisible(Ogre::AxisAlignedBox const&, Ogre::FrustumPlane*) const
Ogre::Camera::isVisible(Ogre::Sphere const&, Ogre::FrustumPlane*) const
Ogre::Camera::isVisible(Ogre::Vector3 const&, Ogre::FrustumPlane*) const
Ogre::Camera::getWorldSpaceCorners() const
Ogre::Camera::getFrustumPlane(unsigned short) const
Ogre::Camera::projectSphere(Ogre::Sphere const&, float*, float*, float*, float*) const
Ogre::Camera::getNearClipDistance() const
Ogre::Camera::getFarClipDistance() const
Ogre::Camera::getViewMatrix() const
Ogre::Camera::getViewMatrix(bool) const
Ogre::Camera::setUseRenderingDistance(bool)
Ogre::Camera::getUseRenderingDistance() const
Ogre::Camera::synchroniseBaseSettingsWith(Ogre::Camera const*)
Ogre::Camera::getPositionForViewUpdate() const
Ogre::Camera::getOrientationForViewUpdate() const
Ogre::Camera::Listener::operator=(Ogre::Camera::Listener const&)
Ogre::Camera::Listener::Listener(Ogre::Camera::Listener const&)
Ogre::Camera::Listener::Listener()
Ogre::Camera::Listener::~Listener()
Ogre::Camera::Listener::cameraPreRenderScene(Ogre::Camera*)
Ogre::Camera::Listener::cameraPostRenderScene(Ogre::Camera*)
Ogre::Camera::Listener::cameraDestroyed(Ogre::Camera*)
*/
/*
Ogre::Frustum::INFINITE_FAR_PLANE_ADJUST
Ogre::Frustum::Frustum(std::string const&)
Ogre::Frustum::~Frustum()
Ogre::Frustum::setFOVy(Ogre::Radian const&)
Ogre::Frustum::getFOVy() const
Ogre::Frustum::setNearClipDistance(float)
Ogre::Frustum::getNearClipDistance() const
Ogre::Frustum::setFarClipDistance(float)
Ogre::Frustum::getFarClipDistance() const
Ogre::Frustum::setFrustumOffset(Ogre::Vector2 const&)
Ogre::Frustum::setFrustumOffset(float, float)
Ogre::Frustum::getFrustumOffset() const
Ogre::Frustum::setFocalLength(float)
Ogre::Frustum::getFocalLength() const
Ogre::Frustum::setFrustumExtents(float, float, float, float)
Ogre::Frustum::resetFrustumExtents()
Ogre::Frustum::getFrustumExtents(float&, float&, float&, float&) const
Ogre::Frustum::getProjectionMatrixRS() const
Ogre::Frustum::getProjectionMatrixWithRSDepth() const
Ogre::Frustum::getProjectionMatrix() const
Ogre::Frustum::getViewMatrix() const
Ogre::Frustum::calcViewMatrixRelative(Ogre::Vector3 const&, Ogre::Matrix4&) const
Ogre::Frustum::setCustomViewMatrix(bool, Ogre::Matrix4 const&)
Ogre::Frustum::isCustomViewMatrixEnabled() const
Ogre::Frustum::setCustomProjectionMatrix(bool, Ogre::Matrix4 const&)
Ogre::Frustum::isCustomProjectionMatrixEnabled() const
Ogre::Frustum::getFrustumPlanes() const
Ogre::Frustum::getFrustumPlane(unsigned short) const
Ogre::Frustum::isVisible(Ogre::AxisAlignedBox const&, Ogre::FrustumPlane*) const
Ogre::Frustum::isVisible(Ogre::Sphere const&, Ogre::FrustumPlane*) const
Ogre::Frustum::isVisible(Ogre::Vector3 const&, Ogre::FrustumPlane*) const
Ogre::Frustum::getTypeFlags() const
Ogre::Frustum::getBoundingBox() const
Ogre::Frustum::getBoundingRadius() const
Ogre::Frustum::_updateRenderQueue(Ogre::RenderQueue*)
Ogre::Frustum::getMovableType() const
Ogre::Frustum::_notifyCurrentCamera(Ogre::Camera*)
Ogre::Frustum::getMaterial() const
Ogre::Frustum::getRenderOperation(Ogre::RenderOperation&)
Ogre::Frustum::getWorldTransforms(Ogre::Matrix4*) const
Ogre::Frustum::getSquaredViewDepth(Ogre::Camera const*) const
Ogre::Frustum::getLights() const
Ogre::Frustum::getWorldSpaceCorners() const
Ogre::Frustum::setProjectionType(Ogre::ProjectionType)
Ogre::Frustum::getProjectionType() const
Ogre::Frustum::setOrthoWindow(float, float)
Ogre::Frustum::setOrthoWindowHeight(float)
Ogre::Frustum::setOrthoWindowWidth(float)
Ogre::Frustum::getOrthoWindowHeight() const
Ogre::Frustum::getOrthoWindowWidth() const
Ogre::Frustum::enableReflection(Ogre::Plane const&)
Ogre::Frustum::enableReflection(Ogre::MovablePlane const*)
Ogre::Frustum::disableReflection()
Ogre::Frustum::isReflected() const
Ogre::Frustum::getReflectionMatrix() const
Ogre::Frustum::getReflectionPlane() const
Ogre::Frustum::projectSphere(Ogre::Sphere const&, float*, float*, float*, float*) const
Ogre::Frustum::enableCustomNearClipPlane(Ogre::MovablePlane const*)
Ogre::Frustum::enableCustomNearClipPlane(Ogre::Plane const&)
Ogre::Frustum::disableCustomNearClipPlane()
Ogre::Frustum::isCustomNearClipPlaneEnabled() const
Ogre::Frustum::visitRenderables(Ogre::Renderable::Visitor*, bool)
Ogre::Frustum::getPositionForViewUpdate() const
Ogre::Frustum::getOrientationForViewUpdate() const
Ogre::Frustum::getPlaneBoundedVolume()
Ogre::Frustum::setOrientationMode(Ogre::OrientationMode)
Ogre::Frustum::getOrientationMode() const
*/
/*
Ogre::MovableObject::Listener
Ogre::MovableObject::operator=(Ogre::MovableObject const&)
Ogre::MovableObject::MovableObject(Ogre::MovableObject const&)
Ogre::MovableObject::MovableObject()
Ogre::MovableObject::MovableObject(std::string const&)
Ogre::MovableObject::~MovableObject()
Ogre::MovableObject::_notifyCreator(Ogre::MovableObjectFactory*)
Ogre::MovableObject::_getCreator() const
Ogre::MovableObject::_notifyManager(Ogre::SceneManager*)
Ogre::MovableObject::_getManager() const
Ogre::MovableObject::getName() const
Ogre::MovableObject::getMovableType() const
Ogre::MovableObject::getParentNode() const
Ogre::MovableObject::getParentSceneNode() const
Ogre::MovableObject::isParentTagPoint() const
Ogre::MovableObject::_notifyAttached(Ogre::Node*, bool)
Ogre::MovableObject::isAttached() const
Ogre::MovableObject::detachFromParent()
Ogre::MovableObject::isInScene() const
Ogre::MovableObject::_notifyMoved()
Ogre::MovableObject::_notifyCurrentCamera(Ogre::Camera*)
Ogre::MovableObject::getBoundingBox() const
Ogre::MovableObject::getBoundingRadius() const
Ogre::MovableObject::getWorldBoundingBox(bool) const
Ogre::MovableObject::getWorldBoundingSphere(bool) const
Ogre::MovableObject::_updateRenderQueue(Ogre::RenderQueue*)
Ogre::MovableObject::setVisible(bool)
Ogre::MovableObject::getVisible() const
Ogre::MovableObject::isVisible() const
Ogre::MovableObject::setRenderingDistance(float)
Ogre::MovableObject::getRenderingDistance() const
Ogre::MovableObject::setUserAny(Ogre::Any const&)
Ogre::MovableObject::getUserAny() const
Ogre::MovableObject::getUserObjectBindings()
Ogre::MovableObject::getUserObjectBindings() const
Ogre::MovableObject::setRenderQueueGroup(unsigned char)
Ogre::MovableObject::setRenderQueueGroupAndPriority(unsigned char, unsigned short)
Ogre::MovableObject::getRenderQueueGroup() const
Ogre::MovableObject::_getParentNodeFullTransform() const
Ogre::MovableObject::setQueryFlags(unsigned int)
Ogre::MovableObject::addQueryFlags(unsigned int)
Ogre::MovableObject::removeQueryFlags(unsigned long)
Ogre::MovableObject::getQueryFlags() const
Ogre::MovableObject::setDefaultQueryFlags(unsigned int)
Ogre::MovableObject::getDefaultQueryFlags()
Ogre::MovableObject::setVisibilityFlags(unsigned int)
Ogre::MovableObject::addVisibilityFlags(unsigned int)
Ogre::MovableObject::removeVisibilityFlags(unsigned int)
Ogre::MovableObject::getVisibilityFlags() const
Ogre::MovableObject::setDefaultVisibilityFlags(unsigned int)
Ogre::MovableObject::getDefaultVisibilityFlags()
Ogre::MovableObject::setListener(Ogre::MovableObject::Listener*)
Ogre::MovableObject::getListener() const
Ogre::MovableObject::queryLights() const
Ogre::MovableObject::getLightMask() const
Ogre::MovableObject::setLightMask(unsigned int)
Ogre::MovableObject::_getLightList()
Ogre::MovableObject::getEdgeList()
Ogre::MovableObject::hasEdgeList()
Ogre::MovableObject::getShadowVolumeRenderableIterator(Ogre::ShadowTechnique, Ogre::Light const*, Ogre::HardwareIndexBufferSharedPtr*, bool, float, unsigned long)
Ogre::MovableObject::getLightCapBounds() const
Ogre::MovableObject::getDarkCapBounds(Ogre::Light const&, float) const
Ogre::MovableObject::setCastShadows(bool)
Ogre::MovableObject::getCastShadows() const
Ogre::MovableObject::getReceivesShadows()
Ogre::MovableObject::getPointExtrusionDistance(Ogre::Light const*) const
Ogre::MovableObject::getTypeFlags() const
Ogre::MovableObject::visitRenderables(Ogre::Renderable::Visitor*, bool)
Ogre::MovableObject::setDebugDisplayEnabled(bool)
Ogre::MovableObject::isDebugDisplayEnabled() const
Ogre::MovableObject::Listener::operator=(Ogre::MovableObject::Listener const&)
Ogre::MovableObject::Listener::Listener(Ogre::MovableObject::Listener const&)
Ogre::MovableObject::Listener::Listener()
Ogre::MovableObject::Listener::~Listener()
Ogre::MovableObject::Listener::objectDestroyed(Ogre::MovableObject*)
Ogre::MovableObject::Listener::objectAttached(Ogre::MovableObject*)
Ogre::MovableObject::Listener::objectDetached(Ogre::MovableObject*)
Ogre::MovableObject::Listener::objectMoved(Ogre::MovableObject*)
Ogre::MovableObject::Listener::objectRendering(Ogre::MovableObject const*, Ogre::Camera const*)
Ogre::MovableObject::Listener::objectQueryLights(Ogre::MovableObject const*)
*/
