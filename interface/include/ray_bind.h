/******************************************************************************
 * ray_bind.h -  bindings for Ogre::Ray
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
#ifndef RAY_BIND_H 
#define RAY_BIND_H

#include "ogre_interface.h"
#define RayHandle void*
#define PlaneHandle void*
#define AxisAlignedBoxHandle void*

typedef struct
{
    int intersects;
    coiReal distance;
} ray_pair;

DLL RayHandle create_ray(const coiVector3* origin, const coiVector3* direction);
DLL void destroy_ray(RayHandle handle);
//Ray::setOrigin
DLL void ray_set_origin(RayHandle handle, const coiVector3* origin);
//Ray::getOrigin
DLL void ray_get_origin(RayHandle handle, coiVector3* origin);
//Ray::setDirection
DLL void ray_set_direction(RayHandle handle, const coiVector3* direction);
//Ray::getDirection
DLL void ray_get_direction(RayHandle handle, coiVector3* direction);
//Ray::getPoint
DLL void ray_get_point(RayHandle handle, coiReal units, coiVector3* point);
//Ray::intersects(Plane)
DLL void ray_intersects_plane(RayHandle handle, PlaneHandle plane_handle, ray_pair* result);
//Ray::intersects(AxisAlignedBox)
DLL void ray_intersects_axisalignedbox(RayHandle handle, AxisAlignedBoxHandle query_handle, ray_pair* result);

#endif

