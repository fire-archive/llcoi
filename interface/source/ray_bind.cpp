/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "ray_bind.h"
#include <OgreRay.h>
#include <OgreVector3.h>
#include <OgrePlane.h>

RayHandle create_ray(const coiVector3* origin, const coiVector3* direction)
{
    const Ogre::Vector3 o(origin->x, origin->y, origin->z);
    const Ogre::Vector3 d(direction->x, direction->y, direction->z);
    Ogre::Ray* ray = new Ogre::Ray(o, d);

    return reinterpret_cast<RayHandle>(ray);
}

void destroy_ray(RayHandle handle)
{
    Ogre::Ray* ray = reinterpret_cast<Ogre::Ray*>(handle);
    delete ray;
}

//Ray::setOrigin
void ray_set_origin(RayHandle handle, const coiVector3* origin)
{
    Ogre::Ray* ray = reinterpret_cast<Ogre::Ray*>(handle);
    const Ogre::Vector3 setter(origin->x, origin->y, origin->z);
    ray->setOrigin(setter);
}

//Ray::getOrigin
void ray_get_origin(RayHandle handle, coiVector3* origin)
{
    Ogre::Ray* ray = reinterpret_cast<Ogre::Ray*>(handle);
    const Ogre::Vector3& getter = ray->getOrigin();

    origin->x = getter.x;
    origin->y = getter.y;
    origin->z = getter.z;
}

//Ray::setDirection
void ray_set_direction(RayHandle handle, const coiVector3* direction)
{
    Ogre::Ray* ray = reinterpret_cast<Ogre::Ray*>(handle);
    const Ogre::Vector3 setter(direction->x, direction->y, direction->z);
    ray->setDirection(setter);
}

//Ray::getDirection
void ray_get_direction(RayHandle handle, coiVector3* direction)
{
    Ogre::Ray* ray = reinterpret_cast<Ogre::Ray*>(handle);
    const Ogre::Vector3& getter = ray->getDirection();

    direction->x = getter.x;
    direction->y = getter.y;
    direction->z = getter.z;
}

//Ray::getPoint
void ray_get_point(RayHandle handle, coiReal units, coiVector3* point)
{
    Ogre::Ray* ray = reinterpret_cast<Ogre::Ray*>(handle);
    Ogre::Vector3 getter = ray->getPoint(units);

    point->x = getter.x;
    point->y = getter.y;
    point->z = getter.z;
}

//Ray::intersects(Plane)
void ray_intersects_plane(RayHandle handle, PlaneHandle query_handle, ray_pair* result)
{
    Ogre::Ray* ray = reinterpret_cast<Ogre::Ray*>(handle);
    Ogre::Plane* query = reinterpret_cast<Ogre::Plane*>(query_handle);
    std::pair<bool, Ogre::Real> r = ray->intersects(*query);

    result->intersects = r.first;
    result->distance   = r.second;
}

//Ray::intersects(AxisAlignedBox)
void ray_intersects_axisalignedbox(RayHandle handle, AxisAlignedBoxHandle query_handle, ray_pair* result)
{
    Ogre::Ray* ray = reinterpret_cast<Ogre::Ray*>(handle);
    Ogre::AxisAlignedBox* query = reinterpret_cast<Ogre::AxisAlignedBox*>(query_handle);
    std::pair<bool, Ogre::Real> r = ray->intersects(*query);

    result->intersects = r.first;
    result->distance   = r.second;
}

//Ray::intersects(Sphere)
void ray_intersects_sphere(RayHandle handle, SphereHandle query_handle, ray_pair* result)
{
    Ogre::Ray* ray = reinterpret_cast<Ogre::Ray*>(handle);
    Ogre::Sphere* query = reinterpret_cast<Ogre::Sphere*>(query_handle);
    std::pair<bool, Ogre::Real> r = ray->intersects(*query);

    result->intersects = r.first;
    result->distance   = r.second;
}


