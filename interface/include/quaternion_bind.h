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

DLL QuaternionHandle quaternion_from_rotation_matrix(coiMatrix3 *rot);
DLL void quaternion_to_rotation_matrix(QuaternionHandle quat, coiMatrix3 *rot);
