/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2014, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "matrix3_bind.h"
#include <OgreMatrix3.h>

coiMatrix3 matrix_3_from_euler_angles_Y_X_Z( coiRadian y_angle, 
	coiRadian x_angle, coiRadian z_angle) 
{
	Ogre::Matrix3* mat = new Ogre::Matrix3
	mat->FromEulerAnglesYXZ(y_angle, x_angle, z_angle);
	
	return mat.m
}
