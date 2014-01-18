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

DLL coiMatrix3 matrix_3_from_euler_angles_y_x_z( coiRadian y_angle, 
	coiRadian x_angle, coiRadian z_angle);
DLL void matrix_3_to_euler_angles_y_x_z(coiMatrix3 mat, coiRadian *y_angle, 
	coiRadian *x_angle, coiRadian *z_angle);
	
