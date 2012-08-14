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

//Real setRadius(void) const
coiReal sphere_get_radius(SphereHandle handle)
{
    Ogre::Sphere* sphere = reinterpret_cast<Ogre::Sphere*>(handle);
    return sphere->getRadius();
}

