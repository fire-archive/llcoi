/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "ogre_interface.h"
#include "plane_bind.h"

#include <OgreRoot.h>
#include <OgrePlane.h>

PlaneHandle plane_create_plane()
{
    Ogre::Plane* plane = new Ogre::Plane;
    return reinterpret_cast<PlaneHandle>(plane);

}

PlaneHandle plane_create_plane_normal(float x, float y, float z, float distance)
{
    Ogre::Plane* plane = new Ogre::Plane(Ogre::Vector3(x,y,z), distance);
    return reinterpret_cast<PlaneHandle>(plane);
}

void plane_destroy_plane(PlaneHandle handle)
{
    Ogre::Plane* plane = reinterpret_cast<Ogre::Plane*>(handle);
    delete plane;
}

void plane_get_normal(PlaneHandle handle, coiVector3* normal)
{
    Ogre::Plane* plane = reinterpret_cast<Ogre::Plane*>(handle);
    Ogre::Vector3 n = plane->normal;
    normal->x = n.x;
    normal->y = n.y;
    normal->z = n.z;
}

void plane_set_normal(PlaneHandle handle, const coiVector3* normal)
{
    Ogre::Plane* plane = reinterpret_cast<Ogre::Plane*>(handle);
    plane->normal = Ogre::Vector3(normal->x, normal->y, normal->z);
}

coiReal plane_get_d(PlaneHandle handle)
{
    Ogre::Plane* plane = reinterpret_cast<Ogre::Plane*>(handle);
    return plane->d;
}

void plane_set_d(PlaneHandle handle, coiReal d)
{
    Ogre::Plane* plane = reinterpret_cast<Ogre::Plane*>(handle);
    plane->d = d;
}


// PlaneList (typedef vector<Plane>::type PlaneList)
PlaneListHandle create_planelist()
{
    Ogre::PlaneList* pl = new Ogre::PlaneList;
    return reinterpret_cast<PlaneListHandle>(pl);
}

void destroy_planelist(PlaneListHandle handle)
{
    Ogre::PlaneList* pl = reinterpret_cast<Ogre::PlaneList*>(handle);
    delete pl;
}
