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

DLL Vector2Handle vector2_create();
DLL Vector2Handle vector2_create_from_values(coiReal fX, coiReal fY);
DLL Vector2Handle vector2_multiply_scalar(Vector2Handle lhs, coiReal scalar);
