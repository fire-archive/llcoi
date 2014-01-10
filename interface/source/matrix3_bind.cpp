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
	Ogre::Matrix3* mat = new Ogre::Matrix3;
	Ogre::Radian yangle(y_angle), xangle(x_angle), zangle(z_angle);
	
	mat->FromEulerAnglesYXZ(yangle, xangle, zangle);
	coiMatrix3 coimat;
	coimat.m[0][0] = *mat[0][0]; 
	coimat.m[0][1] = *mat[0][1];
	coimat.m[0][2] = *mat[0][2];
	coimat.m[1][0] = *mat[1][0];   
	coimat.m[1][1] = *mat[1][1];
	coimat.m[1][2] = *mat[1][2];
	coimat.m[2][0] = *mat[2][0];
	coimat.m[2][1] = *mat[2][1];
	coimat.m[2][2] = *mat[2][2]; 
	delete mat;     
	 
	return coimat;
}
