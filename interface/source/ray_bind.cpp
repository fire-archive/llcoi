/******************************************************************************
 * ray_bind.cpp -  bindings for Ogre::Ray
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

#include "ray_bind.h"
#include <OgreRay.h>
#include <OgreVector3.h>

RayHandle create_ray(const coiVector3* origin, const coiVector3* direction)
{
    Ogre::Vector3 o(origin->x, origin->y, origin->z);
    Ogre::Vector3 d(direction->x, direction->y, direction->z);
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
    Ogre::Vector3 setter(origin->x, origin->y, origin->z);
    ray->setOrigin(setter);
}

//Ray::getOrigin
void ray_get_origin(RayHandle handle, coiVector3* origin)
{
    Ogre::Ray* ray = reinterpret_cast<Ogre::Ray*>(handle);
    Ogre::Vector3 getter = ray->getOrigin();

    origin->x = getter.x;
    origin->y = getter.y;
    origin->z = getter.z;
}

//Ray::setDirection
void ray_set_direction(RayHandle handle, const coiVector3* direction)
{
    Ogre::Ray* ray = reinterpret_cast<Ogre::Ray*>(handle);
    Ogre::Vector3 setter(direction->x, direction->y, direction->z);
    ray->setDirection(setter);
}

//Ray::getDirection
void ray_get_direction(RayHandle handle, coiVector3* direction)
{
    Ogre::Ray* ray = reinterpret_cast<Ogre::Ray*>(handle);
    Ogre::Vector3 getter = ray->getDirection();

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

