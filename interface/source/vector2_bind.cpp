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
