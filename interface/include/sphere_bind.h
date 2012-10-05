/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#pragma once

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
