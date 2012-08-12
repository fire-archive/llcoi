/******************************************************************************
 * vector3_bind.h - bindings for Ogre::Vector3
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
#ifndef VECTOR3_BIND_H
#define VECTOR3_BIND_H

#include "ogre_interface.h"

// Vector3
// TODO: Probably change these to pass pointers to coiV3 rather than passing the 
// structs on the stack, as shown above with ColourValue. 
//Vector3::operator !=
DLL int vector3_notequals_vector3(coiVector3 lhs, coiVector3 rhs);

//Vector3::operator ==
DLL int vector3_equals_vector3(coiVector3 lhs, coiVector3 rhs);

//Vector3::operator +
DLL coiVector3 vector3_add_vector3(coiVector3 lhs, coiVector3 rhs);

//Vector3::operator +=
DLL void vector3_update_add_vector3(coiVector3 lhs, coiVector3 rhs);

//Vector3::operator -
DLL coiVector3 vector3_subtract_vector3(coiVector3 lhs, coiVector3 rhs);

//Vector3::operator -=
DLL void vector3_update_subtract_vector3(coiVector3 lhs, coiVector3 rhs);

//Vector3::operator - 
DLL coiVector3 vector3_negate(coiVector3 v3);

// Vector3::operator/ 
DLL coiVector3 vector3_divide_vector3(coiVector3 lhs, coiVector3 rhs);

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
#endif
