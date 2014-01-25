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

// Vector3
// TODO: Probably change these to pass pointers to coiV3 rather than passing the 
// structs on the stack, as was done with coiColourValue. 

DLL Vector3Handle vector3_create();
DLL coiReal vector3_x(Vector3Handle handle);
DLL coiReal vector3_y(Vector3Handle handle);
DLL coiReal vector3_z(Vector3Handle handle);

//Vector3::operator !=
DLL int vector3_notequals_vector3(Vector3Handle lhs, Vector3Handle rhs);

//Vector3::operator ==
DLL int vector3_equals_vector3(Vector3Handle lhs, Vector3Handle rhs);

//Vector3::operator +
DLL coiVector3 vecotr3_add_vector3(Vector3Handle lhs, Vector3Handle rhs);

//Vector3::operator +=
DLL Vector3Handle vector3_update_add_vector3(Vector3Handle lhs, Vector3Handle rhs);

//Vector3::operator -
DLL coiVector3 vector3_subtract_vector3(Vector3Handle lhs, Vector3Handle rhs);

//Vector3::operator -=
DLL void vector3_update_subtract_vector3(Vector3Handle lhs, Vector3Handle rhs);

//Vector3::operator - 
DLL coiVector3 vector3_negate(Vector3Handle handle);

// Vector3::operator/ 
DLL coiVector3 vector3_divide_vector3(Vector3Handle lhs, Vector3Handle rhs);

// Vector3::operator*
DLL coiVector3 vector3_multiply_vector3(coiVector3 lhs, coiVector3 rhs);

// Vector3::isNaN
DLL int vector3_is_nan(coiVector3 v3);

//Vector3::primaryAxis
DLL coiVector3 vector3_primary_axis(coiVector3);

// Vector3::ZERO
DLL coiVector3 vector3_ZERO();

// Vector3::UNIT_X
DLL coiVector3 vector3_UNIT_X();

// Vector3::UNIT_Y
DLL coiVector3 vector3_UNIT_Y();

// Vector3::UNIT_Z
DLL coiVector3 vector3_UNIT_Z();

// Vector3::NEGATIVE_UNIT_X
DLL coiVector3 vector3_NEGATIVE_UNIT_X();

// Vector3::NEGATIVE_UNIT_Y
DLL coiVector3 vector3_NEGATIVE_UNIT_Y();

// Vector3::NEGATIVE_UNIT_Z
DLL coiVector3 vector3_NEGATIVE_UNIT_Z();

// Vector3::UNIT_SCALE
DLL coiVector3 vector3_UNIT_SCALE();
