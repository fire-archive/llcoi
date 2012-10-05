/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
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
