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
#include "binding_utils.h"
#include "light_bind.h"

#include <OgreLight.h>

void destroy_light(LightHandle handle)
{
    Ogre::Light* light = static_cast<Ogre::Light*>(handle);
    delete light;
}

//Ogre::Light::setPosition() const
void light_set_position(LightHandle light_handle, const float x, const float y, const float z)
{
    Ogre::Light* light = static_cast<Ogre::Light*>(light_handle);
    light->setPosition(Ogre::Vector3(x, y, z));
}

//Ogre::Light::getPosition() const
void light_get_position_xyz(LightHandle handle, float* x, float* y, float* z)
{
    Ogre::Light* light = static_cast<Ogre::Light*>(handle);
    const Ogre::Vector3& p = light->getPosition();
    *x = p.x;
    *y = p.y;
    *z = p.z;
}

//Ogre::Light::getPosition() const
void light_get_position(LightHandle handle, coiVector3* pos)
{
    Ogre::Light* light = static_cast<Ogre::Light*>(handle);
    const Ogre::Vector3& p = light->getPosition();
    pos->x = p.x;
    pos->y = p.y;
    pos->z = p.z;
}

//Ogre::Light::setDirection(float, float, float)
void light_set_direction_xyz(LightHandle handle, const float x, const float y, const float z)
{
    Ogre::Light* light = static_cast<Ogre::Light*>(handle);
    light->setDirection(x,y,z);
}

//Ogre::Light::setDirection(Ogre::Vector3 const&)
void light_set_direction(LightHandle handle, const coiVector3* v)
{
    Ogre::Light* light = static_cast<Ogre::Light*>(handle);
    const Ogre::Vector3 direction(v->x, v->y, v->z);
    light->setDirection(direction);
}

//Ogre::Light::getDirection() const
void light_get_direction(LightHandle handle, coiVector3* v)
{
    Ogre::Light* light = static_cast<Ogre::Light*>(handle);
    const Ogre::Vector3& d = light->getDirection();
    v->x = d.x;
    v->y = d.y;
    v->z = d.z;
}

//Ogre::Light::setSpotlightRange(Ogre::Radian const&, Ogre::Radian const&, float)
void light_set_spotlight_range(LightHandle handle, const coiReal i, const coiReal o, coiReal f)
{
    Ogre::Light* light = static_cast<Ogre::Light*>(handle);
    const Ogre::Radian inner(i);
    const Ogre::Radian outer(o);
    Ogre::Real falloff(f);
    light->setSpotlightRange(inner, outer, falloff);

}

void light_set_type(LightHandle handle, light_types type)
{
    Ogre::Light* light = static_cast<Ogre::Light*>(handle);
    Ogre::Light::LightTypes lt = llcoi_light_types_to_ogre_light_types(type);
    light->setType(lt);
}

//Ogre::Light::setDiffuseColour(float, float, float)
//Ogre::Light::setDiffuseColour(Ogre::ColourValue const&)
void light_set_diffuse_colour(LightHandle handle, const coiColourValue* colour)
{
    Ogre::Light* light = static_cast<Ogre::Light*>(handle);
    Ogre::ColourValue cv(colour->r, colour->g, colour->b, colour->a);
    light->setDiffuseColour(cv);
}

//Ogre::Light::setSpecularColour(float, float, float)
//Ogre::Light::setSpecularColour(Ogre::ColourValue const&)
void light_set_specular_colour(LightHandle handle, const coiColourValue* colour)
{
    Ogre::Light* light = static_cast<Ogre::Light*>(handle);
    Ogre::ColourValue cv(colour->r, colour->g, colour->b, colour->a);
    light->setSpecularColour(cv);
}


/*
//Ogre::Light::operator=(Ogre::Light const&)
//Ogre::Light::Light(Ogre::Light const&)
//Ogre::Light::_calcTempSquareDist(Ogre::Vector3 const&)
//Ogre::Light::Light()
//Ogre::Light::Light(std::string const&)
//Ogre::Light::~Light()
//Ogre::Light::setType(Ogre::Light::LightTypes)
//Ogre::Light::getType() const
//Ogre::Light::getDiffuseColour() const
//Ogre::Light::getSpecularColour() const
//Ogre::Light::setAttenuation(float, float, float, float)
//Ogre::Light::getAttenuationRange() const
//Ogre::Light::getAttenuationConstant() const
//Ogre::Light::getAttenuationLinear() const
//Ogre::Light::getAttenuationQuadric() const
//Ogre::Light::setPosition(float, float, float)
//Ogre::Light::setPosition(Ogre::Vector3 const&)
//Ogre::Light::getSpotlightInnerAngle() const
//Ogre::Light::getSpotlightOuterAngle() const
//Ogre::Light::getSpotlightFalloff() const
//Ogre::Light::setSpotlightInnerAngle(Ogre::Radian const&)
//Ogre::Light::setSpotlightOuterAngle(Ogre::Radian const&)
//Ogre::Light::setSpotlightFalloff(float)
//Ogre::Light::setPowerScale(float)
//Ogre::Light::getPowerScale() const
//Ogre::Light::_notifyAttached(Ogre::Node*, bool)
//Ogre::Light::_notifyMoved()
//Ogre::Light::getBoundingBox() const
//Ogre::Light::_updateRenderQueue(Ogre::RenderQueue*)
//Ogre::Light::getMovableType() const
//Ogre::Light::getDerivedPosition(bool) const
//Ogre::Light::getDerivedDirection() const
//Ogre::Light::setVisible(bool)
//Ogre::Light::getBoundingRadius() const
//Ogre::Light::getAs4DVector(bool) const
//Ogre::Light::_getNearClipVolume(Ogre::Camera const*) const
//Ogre::Light::_getFrustumClipVolumes(Ogre::Camera const*) const
//Ogre::Light::getTypeFlags() const
//Ogre::Light::createAnimableValue(std::string const&)
//Ogre::Light::setCustomShadowCameraSetup(Ogre::SharedPtr<Ogre::ShadowCameraSetup> const&)
//Ogre::Light::resetCustomShadowCameraSetup()
//Ogre::Light::getCustomShadowCameraSetup() const
//Ogre::Light::visitRenderables(Ogre::Renderable::Visitor*, bool)
//Ogre::Light::_getIndexInFrame() const
//Ogre::Light::_notifyIndexInFrame(unsigned int)
//Ogre::Light::setShadowFarDistance(float)
//Ogre::Light::resetShadowFarDistance()
//Ogre::Light::getShadowFarDistance() const
//Ogre::Light::getShadowFarDistanceSquared() const
//Ogre::Light::setShadowNearClipDistance(float)
//Ogre::Light::getShadowNearClipDistance() const
//Ogre::Light::_deriveShadowNearClipDistance(Ogre::Camera const*) const
//Ogre::Light::setShadowFarClipDistance(float)
//Ogre::Light::getShadowFarClipDistance() const
//Ogre::Light::_deriveShadowFarClipDistance(Ogre::Camera const*) const
//Ogre::Light::_setCameraRelative(Ogre::Camera*)
//Ogre::Light::setCustomParameter(unsigned short, Ogre::Vector4 const&)
//Ogre::Light::getCustomParameter(unsigned short) const
//Ogre::Light::_updateCustomGpuParameter(unsigned short, Ogre::GpuProgramParameters::AutoConstantEntry const&, Ogre::GpuProgramParameters*) const
*/
