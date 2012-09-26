/******************************************************************************
 * axisalignedbox_bind.h - bindings for Ogre::Entity
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
#pragma once

typedef void* AxisAlignedBoxHandle;

#include "ogre_interface.h"

// Ogre::AxisAlignedBox
DLL AxisAlignedBoxHandle create_axis_aligned_box();
DLL AxisAlignedBoxHandle create_axis_aligned_box_ex(Extent e);
DLL AxisAlignedBoxHandle create_axis_aligned_box_v3(const coiVector3* min, const coiVector3* max);
DLL void destroy_axis_aligned_box(AxisAlignedBoxHandle handle);
DLL void axisalignedbox_get_size(AxisAlignedBoxHandle handle, coiVector3* size);
DLL void axisalignedbox_get_minimum(AxisAlignedBoxHandle handle, coiVector3* minimum);
DLL void axisalignedbox_get_maximum(AxisAlignedBoxHandle handle, coiVector3* maximum);
DLL void axisalignedbox_set_minimum_x(AxisAlignedBoxHandle handle, coiReal x);
DLL void axisalignedbox_set_minimum_y(AxisAlignedBoxHandle handle, coiReal y);
DLL void axisalignedbox_set_minimum_z(AxisAlignedBoxHandle handle, coiReal z);
DLL void axisalignedbox_set_minimum(AxisAlignedBoxHandle handle, const coiVector3* min);
DLL void axisalignedbox_set_maximum(AxisAlignedBoxHandle handle, const coiVector3* max);
DLL void axisalignedbox_set_maximum_x(AxisAlignedBoxHandle handle, coiReal x);
DLL void axisalignedbox_set_maximum_y(AxisAlignedBoxHandle handle, coiReal y);
DLL void axisalignedbox_set_maximum_z(AxisAlignedBoxHandle handle, coiReal z);
DLL void axisalignedbox_set_extents(AxisAlignedBoxHandle handle, const coiVector3* min, const coiVector3* max);
DLL void axisalignedbox_get_corner(AxisAlignedBoxHandle handle, CornerEnum e, coiVector3* corner);
