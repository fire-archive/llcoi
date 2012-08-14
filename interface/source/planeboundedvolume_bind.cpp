/******************************************************************************
 * planeboundedvolume_bind.cpp -  bindings for Ogre::PlaneBoundedVolume
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

