/******************************************************************************
 * sphere_bind.h -  bindings for Ogre::Sphere
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
#pragma once

typedef void* SphereHandle;
typedef void* PlaneHandle;

#include "ogre_interface.h"

DLL SphereHandle create_sphere(const coiVector3* center, coiReal radius);
DLL void destroy_sphere(SphereHandle handle);
//Real getRadius(void) const
DLL void sphere_set_radius(SphereHandle handle, coiReal radius);
//void getRadius(Real)
DLL coiReal sphere_get_radius(SphereHandle handle);
//void setCenter(Vector3)
DLL void sphere_set_center(SphereHandle handle, const coiVector3* center);
//Real getCenter(void) const
DLL void sphere_get_center(SphereHandle handle, coiVector3* center);
// bool intersects(Sphere&) const
DLL int sphere_intersects_sphere(SphereHandle handle, SphereHandle query);
// bool intersects(AxisAlignedBox&) const
DLL int sphere_intersects_axisalignedbox(SphereHandle handle, AxisAlignedBoxHandle query);
// bool intersects(Plane&) const
DLL int sphere_intersects_plane(SphereHandle handle, PlaneHandle query);
// bool intersects(Vector3&) const
DLL int sphere_intersects_vector3(SphereHandle handle, const coiVector3* query);
// void merge(const Sphere&)
DLL void sphere_merge(SphereHandle handle, SphereHandle other_sphere);
