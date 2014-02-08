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

QuaternionHandle quaternion_create() {
	Ogre::Quaternion * quat = new Ogre::Quaternion();
	return reinterpret_cast<QuaternionHandle>(quat);
}

void quaternion_from_rotation_matrix(QuaternionHandle handle, coiMatrix3 *rot) {
	Ogre::Matrix3 * mat = new Ogre::Matrix3(rot->m[0][0], rot->m[0][1], 
		rot->m[0][2], rot->m[1][0], rot->m[1][1], rot->m[1][2], rot->m[2][0], rot->m[2][1],
		rot->m[2][2]);
	Ogre::Quaternion *quat = reinterpret_cast<Ogre::Quaternion*>(handle);
	quat->FromRotationMatrix(*mat);
	delete mat;
}

QuaternionHandle quaternion_from_values(coiReal fW, coiReal fX, coiReal fY, coiReal fZ){
	Ogre::Quaternion * quat = new Ogre::Quaternion(fW, fX, fY, fZ);  
        return reinterpret_cast<QuaternionHandle>(quat);
}

void quaternion_from_angle_axis(QuaternionHandle handle, coiRadian rfAngle, Vector3Handle vecHandle) {
        Ogre::Quaternion *quat = reinterpret_cast<Ogre::Quaternion*>(handle);
        Ogre::Vector3 *rkAxis = reinterpret_cast<Ogre::Vector3*>(vecHandle);
        quat->FromAngleAxis(Ogre::Radian(rfAngle), *rkAxis);
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

coiReal quaternion_get_w(QuaternionHandle handle) {
	Ogre::Quaternion *quat = reinterpret_cast<Ogre::Quaternion*>(handle);
	return coiReal(quat->w);
}

coiReal quaternion_get_x(QuaternionHandle handle) {
	Ogre::Quaternion *quat = reinterpret_cast<Ogre::Quaternion*>(handle);
	return coiReal(quat->x);
}

coiReal quaternion_get_y(QuaternionHandle handle) {
	Ogre::Quaternion *quat = reinterpret_cast<Ogre::Quaternion*>(handle);
	return coiReal(quat->y);
}

coiReal quaternion_get_z(QuaternionHandle handle) {
	Ogre::Quaternion *quat = reinterpret_cast<Ogre::Quaternion*>(handle);
	return coiReal(quat->z);
}

QuaternionHandle quaternion_multiply_quaternion(QuaternionHandle lhs, QuaternionHandle rhs) {
        Ogre::Quaternion *lhs_bind = reinterpret_cast<Ogre::Quaternion*>(lhs);
        Ogre::Quaternion *rhs_bind = reinterpret_cast<Ogre::Quaternion*>(rhs);
        *lhs_bind = lhs_bind->operator*(*rhs_bind); 
        return reinterpret_cast<QuaternionHandle>(lhs_bind);
}

QuaternionHandle quaternion_multiply_scalar(QuaternionHandle lhs, coiReal scalar) {
        Ogre::Quaternion *lhs_bind = reinterpret_cast<Ogre::Quaternion*>(lhs);
        *lhs_bind = lhs_bind->operator*(scalar); 
        return reinterpret_cast<QuaternionHandle>(lhs_bind);
}

QuaternionHandle quaternion_subtract_quaternion(QuaternionHandle lhs, QuaternionHandle rhs) {
        Ogre::Quaternion *lhs_bind = reinterpret_cast<Ogre::Quaternion*>(lhs);
        Ogre::Quaternion *rhs_bind = reinterpret_cast<Ogre::Quaternion*>(rhs);
        *lhs_bind = lhs_bind->operator-(*rhs_bind); 
        return reinterpret_cast<QuaternionHandle>(lhs_bind);
}

QuaternionHandle quaternion_unit_inverse(QuaternionHandle q) {
        Ogre::Quaternion *quat = reinterpret_cast<Ogre::Quaternion*>(q);
        *quat = quat->UnitInverse(); 
        return reinterpret_cast<QuaternionHandle>(quat);
}

coiReal quaternion_normalise(QuaternionHandle q) {
        Ogre::Quaternion *quat = reinterpret_cast<Ogre::Quaternion*>(q);
        return quat->normalise();
}

void quaternion_to_angle_axis_degree(QuaternionHandle q, coiDegree dAngle, Vector3Handle Axis) {
        Ogre::Quaternion *quat = static_cast<Ogre::Quaternion*>(q);
        Ogre::Vector3 *vec = static_cast<Ogre::Vector3*>(Axis);
        Ogre::Degree degree(dAngle);
        quat->ToAngleAxis(degree, *vec);
}

QuaternionHandle quaternion_slerp(coiReal fT, QuaternionHandle a, QuaternionHandle b, int shortestPath) {
        Ogre::Quaternion *quatA = static_cast<Ogre::Quaternion*>(a);
        Ogre::Quaternion *quatB = static_cast<Ogre::Quaternion*>(b);
        Ogre::Quaternion::Slerp(fT, *quatA, *quatB, shortestPath);
}
