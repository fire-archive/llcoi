/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2014, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "ogre_interface.h"
#include <OgreVector2.h>

Vector2Handle vector2_create() {
    Ogre::Vector2 *vector2 = new Ogre::Vector2();
    return reinterpret_cast<Vector2Handle>(vector2);
}

Vector2Handle vector2_create_from_values(coiReal fX, coiReal fY) {
    Ogre::Vector2 *vector2 = new Ogre::Vector2(fX, fY);
    return reinterpret_cast<Vector2Handle>(vector2);
}

Vector2Handle vector2_multiply_scalar(Vector2Handle lhs, coiReal scalar) {
        Ogre::Vector2 *lhs_bind = reinterpret_cast<Ogre::Vector2*>(lhs);
        *lhs_bind = lhs_bind->operator*(scalar); 
        return reinterpret_cast<Vector2Handle>(lhs_bind);
}

coiReal vector2_x(Vector2Handle handle) {
    Ogre::Vector2 *vector2 = reinterpret_cast<Ogre::Vector2*>(handle);
    return coiReal(vector2->x);
}

coiReal vector2_y(Vector2Handle handle) {
    Ogre::Vector2 *vector2 = reinterpret_cast<Ogre::Vector2*>(handle);
    return coiReal(vector2->y);
}

void vector2_set_x(Vector2Handle handle, coiReal real) {
    Ogre::Vector2 *vector2 = reinterpret_cast<Ogre::Vector2*>(handle);
    vector2->x = Ogre::Real(real);
}

void vector2_set_y(Vector2Handle handle, coiReal real) {
    Ogre::Vector2 *vector2 = reinterpret_cast<Ogre::Vector2*>(handle);
    vector2->y = Ogre::Real(real);
}
