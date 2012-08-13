/******************************************************************************
 * vector3_bind.cpp - bindings for Ogre::Vector3
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

#include "vector3_bind.h"
#include <OgreVector3.h>

//Vector3::operator !=
int vector3_notequals_vector3(coiVector3& lhs, coiVector3& rhs)
{
    return  ( lhs.x != rhs.x || lhs.y != rhs.y || lhs.z != rhs.z );
}

//Vector3::operator ==
int vector3_equals_vector3(coiVector3& lhs, coiVector3& rhs)
{
    return  ( lhs.x == rhs.x || lhs.y == rhs.y || lhs.z == rhs.z );
}

//Vector3::operator +
coiVector3 vector3_add_vector3(coiVector3& lhs, coiVector3& rhs)
{
    coiVector3 result;
    result.x = lhs.x + rhs.x;
    result.y = lhs.y + rhs.y;
    result.z = lhs.z + rhs.z;
    return result;
}

//Vector3::operator +=
void vector3_update_add_vector3(coiVector3& lhs, coiVector3& rhs)
{
    lhs.x += rhs.x;
    lhs.y += rhs.y;
    lhs.z += rhs.z;
}

//Vector3::operator -
coiVector3 vector3_subtract_vector3(coiVector3& lhs, coiVector3& rhs)
{
    coiVector3 result;
    result.x = lhs.x - rhs.x;
    result.y = lhs.y - rhs.y;
    result.z = lhs.z - rhs.z;
    return result;
}

//Vector3::operator -=
void vector3_update_subtract_vector3(coiVector3& lhs, coiVector3& rhs)
{
    lhs.x -= rhs.x;
    lhs.y -= rhs.y;
    lhs.z -= rhs.z;
}

//Vector3::operator - 
coiVector3 vector3_negate(coiVector3& v3)
{
    coiVector3 result;
    result.x = -v3.x;
    result.y = -v3.y;
    result.z = -v3.z;
    return result;
}

// Vector3::operator/ 
coiVector3 vector3_divide_vector3(coiVector3& lhs, coiVector3& rhs)
{
    coiVector3 result;
    result.x = lhs.x / rhs.x;
    result.y = lhs.y / rhs.y;
    result.z = lhs.z / rhs.z;
    return result;
}


coiVector3 vector3_multiply_vector3(coiVector3 lhs, coiVector3 rhs)
{
    coiVector3 result;
    result.x = lhs.x * rhs.x;
    result.y = lhs.y * rhs.y;
    result.z = lhs.z * rhs.z;
    return result;
}

int vector3_is_nan(coiVector3& v3)
{
    Ogre::Vector3 vector3(v3.x, v3.y, v3.z);
    return (vector3.isNaN()) ? 1 : 0;
}

coiVector3 vector3_primary_axis(coiVector3& v3)
{
    Ogre::Vector3 vector3(v3.x, v3.y, v3.z);
    Ogre::Vector3 tmp = vector3.primaryAxis();
    coiVector3 result;
    result.x = tmp.x;
    result.y = tmp.y;
    result.z = tmp.z;

    return result;
}

coiVector3 vector3_ZERO()
{
    Ogre::Vector3 v3(Ogre::Vector3::ZERO);
    coiVector3 result;
    result.x = v3.x;
    result.y = v3.y;
    result.z = v3.z;
    return result;
}

coiVector3 vector3_UNIT_X()
{
    Ogre::Vector3 v3(Ogre::Vector3::UNIT_X);
    coiVector3 result;
    result.x = v3.x;
    result.y = v3.y;
    result.z = v3.z;
    return result;
}

coiVector3 vector3_UNIT_Y()
{
    Ogre::Vector3 v3(Ogre::Vector3::UNIT_Y);
    coiVector3 result;
    result.x = v3.x;
    result.y = v3.y;
    result.z = v3.z;
    return result;
}
coiVector3 vector3_UNIT_Z()
{
    Ogre::Vector3 v3(Ogre::Vector3::UNIT_Z);
    coiVector3 result;
    result.x = v3.x;
    result.y = v3.y;
    result.z = v3.z;
    return result;
}

coiVector3 vector3_NEGATIVE_UNIT_X()
{
    Ogre::Vector3 v3(Ogre::Vector3::NEGATIVE_UNIT_X);
    coiVector3 result;
    result.x = v3.x;
    result.y = v3.y;
    result.z = v3.z;
    return result;
}

coiVector3 vector3_NEGATIVE_UNIT_Y()
{
    Ogre::Vector3 v3(Ogre::Vector3::NEGATIVE_UNIT_Y);
    coiVector3 result;
    result.x = v3.x;
    result.y = v3.y;
    result.z = v3.z;
    return result;
}

coiVector3 vector3_NEGATIVE_UNIT_Z()
{
    Ogre::Vector3 v3(Ogre::Vector3::NEGATIVE_UNIT_Z);
    coiVector3 result;
    result.x = v3.x;
    result.y = v3.y;
    result.z = v3.z;
    return result;
}

coiVector3 vector3_UNIT_SCALE()
{
    Ogre::Vector3 v3(Ogre::Vector3::UNIT_SCALE);
    coiVector3 result;
    result.x = v3.x;
    result.y = v3.y;
    result.z = v3.z;
    return result;
}
