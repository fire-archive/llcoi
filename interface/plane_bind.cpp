/******************************************************************************
 * plane_bind.cpp - bindings for Ogre::Plane
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
#include <OgrePlane.h>

PlaneHandle plane_create_plane()
{
    Ogre::Plane* plane = new Ogre::Plane;
    return reinterpret_cast<PlaneHandle>(plane);

}

PlaneHandle plane_create_plane_normal(float x, float y, float z, float distance)
{
    Ogre::Plane* plane = new Ogre::Plane(Ogre::Vector3(x,y,z), distance);
    return reinterpret_cast<PlaneHandle>(plane);
}

void plane_destroy_plane(PlaneHandle handle)
{
    Ogre::Plane* plane = reinterpret_cast<Ogre::Plane*>(handle);
    delete plane;
}

void plane_get_normal(PlaneHandle handle, coiVector3* normal)
{
    Ogre::Plane* plane = reinterpret_cast<Ogre::Plane*>(handle);
    Ogre::Vector3 n = plane->normal;
    normal->x = n.x;
    normal->y = n.y;
    normal->z = n.z;
}

void plane_set_normal(PlaneHandle handle, const coiVector3* normal)
{
    Ogre::Plane* plane = reinterpret_cast<Ogre::Plane*>(handle);
    plane->normal = Ogre::Vector3(normal->x, normal->y, normal->z);
}

coiReal plane_get_d(PlaneHandle handle)
{
    Ogre::Plane* plane = reinterpret_cast<Ogre::Plane*>(handle);
    return plane->d;
}

void plane_set_d(PlaneHandle handle, coiReal d)
{
    Ogre::Plane* plane = reinterpret_cast<Ogre::Plane*>(handle);
    plane->d = d;
}
