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
#include <OgreQuaternion.h>
#include <OgreMatrix3.h>

QuaternionHandle quaternion_from_rotation_matrix(coiMatrix3 *rot) {
	Ogre::Matrix3 * mat = new Ogre::Matrix3(rot->m[0][0], rot->m[0][1], 
		rot->m[0][2], rot->m[1][0], rot->m[1][1], rot->m[1][2], rot->m[2][0], rot->m[2][1],
		rot->m[2][2]);
	Ogre::Quaternion * quat = new Ogre::Quaternion(*mat);
	delete mat;   
    return reinterpret_cast<QuaternionHandle>(quat);
}

void quaternion_to_rotation_matrix(QuaternionHandle handle, coiMatrix3 *rot) {
	Ogre::Matrix3 *mat = new Ogre::Matrix3();
	Ogre::Quaternion *quat = reinterpret_cast<Ogre::Quaternion*>(handle);
	quat->ToRotationMatrix(*mat);
	rot->m[0][0] = *mat[0][0];
	rot->m[0][1] = *mat[0][1];
	rot->m[0][2] = *mat[0][2];
	rot->m[1][0] = *mat[1][0];
	rot->m[1][1] = *mat[1][1];
	rot->m[1][2] = *mat[1][2];
	rot->m[2][0] = *mat[2][0];
	rot->m[2][1] = *mat[2][1];
	rot->m[2][2] = *mat[2][2]; 
	delete mat;
}
