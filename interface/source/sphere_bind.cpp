/******************************************************************************
 * sphere_bind.cpp -  bindings for Ogre::Sphere
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

#include "sphere_bind.h"
#include <OgreSphere.h>
#include <OgreVector3.h>
#include <OgreAxisAlignedBox.h>

SphereHandle create_sphere(const coiVector3* center, coiReal radius)
{
    Ogre::Vector3 c(center->x, center->y, center->z);

    Ogre::Sphere* sphere = new Ogre::Sphere(c, radius);
    return reinterpret_cast<SphereHandle>(sphere);
}

void destroy_sphere(SphereHandle handle)
{
    Ogre::Sphere* sphere = reinterpret_cast<Ogre::Sphere*>(handle);
    delete sphere;
}

//void setRadius(Real)
void sphere_set_radius(SphereHandle handle, coiReal radius)
{
    Ogre::Sphere* sphere = reinterpret_cast<Ogre::Sphere*>(handle);
    sphere->setRadius(radius);
}

//Real getRadius(void) const
coiReal sphere_get_radius(SphereHandle handle)
{
    Ogre::Sphere* sphere = reinterpret_cast<Ogre::Sphere*>(handle);
    return sphere->getRadius();
}

//void setCenter(Vector3)
void sphere_set_center(SphereHandle handle, const coiVector3* center)
{
    Ogre::Sphere* sphere = reinterpret_cast<Ogre::Sphere*>(handle);
    Ogre::Vector3 c(center->x, center->y, center->z);
    sphere->setCenter(c);
}

//Real getCenter(void) const
void sphere_get_center(SphereHandle handle, coiVector3* center)
{
    Ogre::Sphere* sphere = reinterpret_cast<Ogre::Sphere*>(handle);
    Ogre::Vector3 c = sphere->getCenter();

    center->x = c.x;
    center->y = c.y;
    center->z = c.z;
}

// bool intersects(Sphere&) const
int sphere_intersects_sphere(SphereHandle handle, SphereHandle query)
{
    Ogre::Sphere* sphere = reinterpret_cast<Ogre::Sphere*>(handle);
    Ogre::Sphere* q = reinterpret_cast<Ogre::Sphere*>(query);
    return sphere->intersects(*q);
}

// bool intersects(AxisAlignedBox&) const
int sphere_intersects_axisalignedbox(SphereHandle handle, AxisAlignedBoxHandle query)
{
    Ogre::Sphere* sphere = reinterpret_cast<Ogre::Sphere*>(handle);
    Ogre::AxisAlignedBox* q = reinterpret_cast<Ogre::AxisAlignedBox*>(query);
    return sphere->intersects(*q);
}

// bool intersects(Plane&) const
int sphere_intersects_plane(SphereHandle handle, PlaneHandle query)
{
    Ogre::Sphere* sphere = reinterpret_cast<Ogre::Sphere*>(handle);
    Ogre::Plane* q = reinterpret_cast<Ogre::Plane*>(query);
    return sphere->intersects(*q);
}

// bool intersects(Vector3&) const
int sphere_intersects_vector3(SphereHandle handle, const coiVector3* query)
{
    Ogre::Sphere* sphere = reinterpret_cast<Ogre::Sphere*>(handle);
    Ogre::Vector3 q(query->x, query->y, query->z);
    return sphere->intersects(q);
}


// void merge(const Sphere&)
void sphere_merge(SphereHandle handle, SphereHandle other_sphere)
{
    Ogre::Sphere* sphere = reinterpret_cast<Ogre::Sphere*>(handle);
    Ogre::Sphere* other  = reinterpret_cast<Ogre::Sphere*>(other_sphere);
    sphere->merge(*other);
}
