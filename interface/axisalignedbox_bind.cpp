/******************************************************************************
 * axisalignedbox_bind.cpp - bindings for Ogre::Entity
 ******************************************************************************
 * This file is part of
 *     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 *                          
 * Low Level C Ogre Interface (llcoi)
 *
 * See http://code.google.com/p/llcoi/ for more information.
 *
 * Copyright (c) 2011, Llcoi Team
 * 
 * License: MIT
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 ******************************************************************************/

#include "ogre_interface.h"
#include "OgreAxisAlignedBox.h"
#include "OgreVector3.h"

AxisAlignedBoxHandle create_axis_aligned_box()
{
    Ogre::AxisAlignedBox* box = new Ogre::AxisAlignedBox;
    return reinterpret_cast<AxisAlignedBoxHandle>(box);
}

//AxisAlignedBoxHandle create_axis_aligned_box_ex(Extent e);
AxisAlignedBoxHandle create_axis_aligned_box_v3(const coiVector3* min, const coiVector3* max)
{
    Ogre::Vector3 _min(min->x, min->y, min->z);
    Ogre::Vector3 _max(max->x, max->y, max->z);
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
    Ogre::Vector3 s = box->getSize();

    size->x = s.x;
    size->y = s.y;
    size->z = s.z;
}



