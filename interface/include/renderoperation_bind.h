/******************************************************************************
 * renderoperation_bind.h -  bindings for Ogre::RenderOperation
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
#ifndef RENDEROPERATION_BIND_H
#define RENDEROPERATION_BIND_H
#include "ogre_interface.h"

#define RenderOperationHandle void*

typedef enum
{
    /// A list of points, 1 vertex per point
    OT_POINT_LIST = 1,
    /// A list of lines, 2 vertices per line
    OT_LINE_LIST = 2,
    /// A strip of connected lines, 1 vertex per line plus 1 start vertex
    OT_LINE_STRIP = 3,
    /// A list of triangles, 3 vertices per triangle
    OT_TRIANGLE_LIST = 4,
    /// A strip of triangles, 3 vertices for the first triangle, and 1 per triangle after that 
    OT_TRIANGLE_STRIP = 5,
    /// A fan of triangles, 3 vertices for the first triangle, and 1 per triangle after that
    OT_TRIANGLE_FAN = 6
} operation_type;




#endif
