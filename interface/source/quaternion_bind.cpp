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

coiQuaternion quaternion_from_rotation_matrix(coiMatrix3 rot) {
	Ogre::Matrix3 * mat = new Ogre::Matrix3(rot.m[0][0], rot.m[0][1], 
		rot.m[0][2], rot.m[1][0], rot.m[1][1], rot.m[1][2], rot.m[2][0], rot.m[2][1],
		rot.m[2][2]);
	Ogre::Quaternion * quat = new Ogre::Quaternion();
	quat->FromRotationMatrix(*mat);
	delete mat;
	coiQuaternion coiquat;
	coiquat.w = quat->w;
	coiquat.x = quat->x;
	coiquat.y = quat->y;
	coiquat.z = quat->z;
    delete quat;
    
    return coiquat;
}
