/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2014, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#pragma once

#include "ogre_interface.h"

DLL QuaternionHandle quaternion_create();
DLL void quaternion_from_rotation_matrix(QuaternionHandle quat, coiMatrix3 *rot);
DLL void quaternion_to_rotation_matrix(QuaternionHandle quat, coiMatrix3 *rot);
DLL QuaternionHandle quaternion_from_values(coiReal fW, coiReal fX, coiReal fY, coiReal fZ);
DLL void quaternion_from_angle_axis(QuaternionHandle handle, coiRadian rfAngle, Vector3Handle vecHandle);
DLL coiReal quaternion_get_w(QuaternionHandle quat);
DLL coiReal quaternion_get_x(QuaternionHandle quat);
DLL coiReal quaternion_get_y(QuaternionHandle quat);
DLL coiReal quaternion_get_z(QuaternionHandle quat);
DLL QuaternionHandle quaternion_multiply_quaternion(QuaternionHandle lhs, QuaternionHandle rhs);
DLL QuaternionHandle quaternion_subtract_quaternion(QuaternionHandle lhs, QuaternionHandle rhs);
DLL QuaternionHandle quaternion_unit_inverse(QuaternionHandle lhs, QuaternionHandle rhs);
