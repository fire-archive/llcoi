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
#include "axisalignedbox_bind.h"
#include "binding_utils.h" // llcoi_extent_to_ogre_extent
#include "OgreAxisAlignedBox.h"
#include "OgreVector3.h"

AxisAlignedBoxHandle create_axis_aligned_box()
{
    Ogre::AxisAlignedBox* box = new Ogre::AxisAlignedBox;
    return reinterpret_cast<AxisAlignedBoxHandle>(box);
}

AxisAlignedBoxHandle create_axis_aligned_box_ex(Extent e)
{
    Ogre::AxisAlignedBox::Extent ce = llcoi_extent_to_ogre_extent(e);
    Ogre::AxisAlignedBox* box = new Ogre::AxisAlignedBox(ce);
    return reinterpret_cast<AxisAlignedBoxHandle>(box);
}

AxisAlignedBoxHandle create_axis_aligned_box_v3(const coiVector3* min, const coiVector3* max)
{
    const Ogre::Vector3 _min(min->x, min->y, min->z);
    const Ogre::Vector3 _max(max->x, max->y, max->z);
    Ogre::AxisAlignedBox* box = new Ogre::AxisAlignedBox(_min, _max);
    return reinterpret_cast<AxisAlignedBoxHandle>(box);
}

void destroy_axis_aligned_box(AxisAlignedBoxHandle handle)
{
    Ogre::AxisAlignedBox* box = reinterpret_cast<Ogre::AxisAlignedBox*>(handle);
    delete box;
}

void axisalignedbox_get_size(AxisAlignedBoxHandle handle, coiVector3* size)
{
    Ogre::AxisAlignedBox* box = reinterpret_cast<Ogre::AxisAlignedBox*>(handle);
    const Ogre::Vector3& s = box->getSize();

    size->x = s.x;
    size->y = s.y;
    size->z = s.z;
}


void axisalignedbox_get_minimum(AxisAlignedBoxHandle handle, coiVector3* minimum)
{
    Ogre::AxisAlignedBox* box = reinterpret_cast<Ogre::AxisAlignedBox*>(handle);
    const Ogre::Vector3& getter = box->getMinimum();

    minimum->x = getter.x;
    minimum->y = getter.y;
    minimum->z = getter.z;
}

void axisalignedbox_get_maximum(AxisAlignedBoxHandle handle, coiVector3* max)
{
    Ogre::AxisAlignedBox* box = reinterpret_cast<Ogre::AxisAlignedBox*>(handle);
    const Ogre::Vector3& getter = box->getMaximum();

    max->x = getter.x;
    max->y = getter.y;
    max->z = getter.z;
}

void axisalignedbox_set_minimum(AxisAlignedBoxHandle handle, const coiVector3* min)
{
    Ogre::AxisAlignedBox* box = reinterpret_cast<Ogre::AxisAlignedBox*>(handle);
    const Ogre::Vector3 setter(min->x, min->y, min->z);
    box->setMinimum(setter);
}

void axisalignedbox_set_minimum_x(AxisAlignedBoxHandle handle, coiReal x)
{
    Ogre::AxisAlignedBox* box = reinterpret_cast<Ogre::AxisAlignedBox*>(handle);
    box->setMinimumX(x);
}

void axisalignedbox_set_minimum_y(AxisAlignedBoxHandle handle, coiReal y)
{
    Ogre::AxisAlignedBox* box = reinterpret_cast<Ogre::AxisAlignedBox*>(handle);
    box->setMinimumY(y);
}

void axisalignedbox_set_minimum_z(AxisAlignedBoxHandle handle, coiReal z)
{
    Ogre::AxisAlignedBox* box = reinterpret_cast<Ogre::AxisAlignedBox*>(handle);
    box->setMinimumZ(z);
}


void axisalignedbox_set_maximum(AxisAlignedBoxHandle handle, const coiVector3* max)
{
    Ogre::AxisAlignedBox* box = reinterpret_cast<Ogre::AxisAlignedBox*>(handle);
    const Ogre::Vector3 setter(max->x, max->y, max->z);
    box->setMaximum(setter);
}

void axisalignedbox_set_maximum_x(AxisAlignedBoxHandle handle, coiReal x)
{
    Ogre::AxisAlignedBox* box = reinterpret_cast<Ogre::AxisAlignedBox*>(handle);
    box->setMaximumX(x);
}

void axisalignedbox_set_maximum_y(AxisAlignedBoxHandle handle, coiReal y)
{
    Ogre::AxisAlignedBox* box = reinterpret_cast<Ogre::AxisAlignedBox*>(handle);
    box->setMaximumY(y);
}

void axisalignedbox_set_maximum_z(AxisAlignedBoxHandle handle, coiReal z)
{
    Ogre::AxisAlignedBox* box = reinterpret_cast<Ogre::AxisAlignedBox*>(handle);
    box->setMaximumZ(z);
}

void axisalignedbox_set_extents(AxisAlignedBoxHandle handle, const coiVector3* min, const coiVector3* max)
{
    Ogre::AxisAlignedBox* box = reinterpret_cast<Ogre::AxisAlignedBox*>(handle);
    const Ogre::Vector3 _min(min->x, min->y, min->z);
    const Ogre::Vector3 _max(max->x, max->y, max->z);
    box->setExtents(_min, _max);
}

void axisalignedbox_get_corner(AxisAlignedBoxHandle handle, CornerEnum e, coiVector3* c)
{
    Ogre::AxisAlignedBox* box = reinterpret_cast<Ogre::AxisAlignedBox*>(handle);
    Ogre::AxisAlignedBox::CornerEnum corner = llcoi_cornerenum_to_ogre_cornerenum(e);

    const Ogre::Vector3& result = box->getCorner(corner);

    c->x = result.x;
    c->y = result.y;
    c->z = result.z;
}


