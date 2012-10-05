/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "planeboundedvolume_bind.h"
#include "binding_utils.h"  // for llcoi_plane_side_to_ogre_plane_side
#include <OgrePlaneBoundedVolume.h>
#include <OgrePlane.h>
#include <OgreSphere.h>
#include <OgreRay.h>

PlaneBoundedVolumeHandle create_planeboundedvolume(plane_side the_outside)
{
    Ogre::Plane::Side side = llcoi_plane_side_to_ogre_plane_side(the_outside);
    Ogre::PlaneBoundedVolume* pbv = new Ogre::PlaneBoundedVolume(side);
    return reinterpret_cast<PlaneBoundedVolumeHandle>(pbv);
}

void destroy_planeboundedvolume(PlaneBoundedVolumeHandle handle)
{
    Ogre::PlaneBoundedVolume* pbv = reinterpret_cast<Ogre::PlaneBoundedVolume*>(handle);
    delete pbv;
}

// bool intersects(AxisAlignedBox&) const
int planeboundedvolume_intersects_axisalignedbox(PlaneBoundedVolumeHandle handle, AxisAlignedBoxHandle query)
{
    Ogre::PlaneBoundedVolume* pbv = reinterpret_cast<Ogre::PlaneBoundedVolume*>(handle);
    Ogre::AxisAlignedBox* q = reinterpret_cast<Ogre::AxisAlignedBox*>(query);
    return pbv->intersects(*q);
}
// bool intersects(AxisAlignedBox&) const
int planeboundedvolume_intersects_sphere(PlaneBoundedVolumeHandle handle, SphereHandle query)
{
    Ogre::PlaneBoundedVolume* pbv = reinterpret_cast<Ogre::PlaneBoundedVolume*>(handle);
    Ogre::Sphere* q = reinterpret_cast<Ogre::Sphere*>(query);
    return pbv->intersects(*q);
}

void planeboundedvolume_intersects_ray(PlaneBoundedVolumeHandle handle, RayHandle query, ray_pair* result)
{
    Ogre::PlaneBoundedVolume* pbv = reinterpret_cast<Ogre::PlaneBoundedVolume*>(handle);
    Ogre::Ray* ray = reinterpret_cast<Ogre::Ray*>(query);
    std::pair<bool, Ogre::Real> r = pbv->intersects(*ray);

    result->intersects = r.first;
    result->distance   = r.second;
}

