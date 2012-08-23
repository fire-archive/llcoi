/******************************************************************************
 * ogre_interface.h - linkage include file
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
#ifndef LLCOI_OGRE_INTERFACE
#define LLCOI_OGRE_INTERFACE

// Detect platform
#if defined( WINCE )
#   if !defined( PLATFORM_WIN_CE )
#       define PLATFORM_WIN_CE
#   endif
#elif defined( WIN32 ) || defined( _WINDOWS )
#   if !defined( PLATFORM_WIN )
#       define PLATFORM_WIN
#   endif
#elif defined( __APPLE__ ) || defined( __APPLE_CC__ )
#   if !defined( PLATFORM_MAC )
#      define PLATFORM_MAC
#   endif
#else
#   if !defined( PLATFORM_LINUX )
#       define PLATFORM_LINUX
#   endif
#endif

#if defined(LLCOI_BUILD_DYNAMIC)
#   if defined( WIN32 ) || defined( _WINDOWS )
#       ifndef llcoi_EXPORTS
#           define DLL __declspec(dllimport)
#       else
#           define DLL extern "C" __declspec(dllexport)
#       endif
#   else
#       ifndef llcoi_EXPORTS
#           define DLL
#       else
#           if defined( __GNUC__ ) && __GNUC__ >= 4
#               define DLL extern "C" __attribute__ ((visibility("default")))
#           else
#               define DLL extern "C"
#           endif
#       endif
#   endif
#else
#   if defined( LLCOI_BUILD_STATIC )
#       if defined( __GNUC__ ) && __GNUC__ >= 4
#           define DLL extern "C" __attribute__ ((visibility("default")))
#       else
#           define DLL extern "C"
#       endif
#   else
#       define DLL
#   endif
#endif

#include <stddef.h> // for size_t

//defines

#define coiReal float

#define COI_DECLARE_HANDLE(name) struct name##__ { int unused; }; typedef struct name##__ *name

// COI_DECLARE_HANDLE(CameraHandle);
// COI_DECLARE_HANDLE(EntityHandle);
// COI_DECLARE_HANDLE(SceneNodeHandle);
// COI_DECLARE_HANDLE(LightHandle);
// COI_DECLARE_HANDLE(RenderWindowHandle);
// COI_DECLARE_HANDLE(RootHandle);
// COI_DECLARE_HANDLE(RenderSystemHandle);
// COI_DECLARE_HANDLE(SceneManagerHandle);
// COI_DECLARE_HANDLE(ViewportHandle);

#define NameValuePairListHandle void*

// From OgrePlatform.h
typedef unsigned int uint32;
typedef unsigned short uint16;
typedef unsigned char uint8;
typedef int int32;
typedef short int16;
typedef char int8;

// OgreColourValue.h
typedef uint32 RGBA;
typedef uint32 ARGB;
typedef uint32 ABGR;
typedef uint32 BGRA;

// OgreSceneManager.h
typedef uint16 SceneTypeMask;

typedef struct
{
    float w;
    float x;
    float y;
    float z;
} coiQuaternion;

typedef struct
{
    float x;
    float y;
    float z;
} coiVector3;

typedef struct
{
    coiReal m[3][3];
} coiMatrix3;

typedef struct
{
    coiReal m[4][4];
} coiMatrix4;

typedef struct
{
    coiVector3 position;
    coiQuaternion orientation;
} ViewPoint;


typedef struct
{
    float r;
    float g;
    float b;
    float a;
} coiColourValue;


typedef struct
{
    float lastFPS;
    float avgFPS;
    float bestFPS;
    float worstFPS;
    unsigned long bestFrameTime;
    unsigned long worstFrameTime;
    size_t triangleCount;
    size_t batchCount;
} FrameStats;



typedef enum
{
    SF_NONE           = 0,
    SF_FPS            = 1,
    SF_AVG_FPS        = 2,
    SF_BEST_FPS       = 4,
    SF_WORST_FPS      = 8,
    SF_TRIANGLE_COUNT = 16,
    SF_ALL            = 0xFFFF
} stat_flags;

typedef enum
{
    FB_FRONT,
    FB_BACK,
    FB_AUTO
} frame_buffer;


typedef enum 
{
    HBU_STATIC = 1,
    HBU_DYNAMIC = 2,
    HBU_WRITE_ONLY = 4,
    HBU_DISCARDABLE = 8,
    HBU_STATIC_WRITE_ONLY = 5, 
    HBU_DYNAMIC_WRITE_ONLY = 6,
    HBU_DYNAMIC_WRITE_ONLY_DISCARDABLE = 14
} hardware_buffer_usage;


typedef enum
{
    LT_POINT = 0,
    LT_DIRECTIONAL = 1,
    LT_SPOTLIGHT = 2
} light_types;


typedef enum
{
    TS_LOCAL,
    TS_PARENT,
    TS_WORLD
} transform_space;

typedef enum
{
    NO_SIDE,
    POSITIVE_SIDE,
    NEGATIVE_SIDE,
    BOTH_SIDE
} plane_side;

typedef enum
{
    EXTENT_NULL,
    EXTENT_FINITE,
    EXTENT_INFINITE
} Extent;

typedef enum {
    FAR_LEFT_BOTTOM = 0,
    FAR_LEFT_TOP = 1,
    FAR_RIGHT_TOP = 2,
    FAR_RIGHT_BOTTOM = 3,
    NEAR_RIGHT_BOTTOM = 7,
    NEAR_LEFT_BOTTOM = 6,
    NEAR_LEFT_TOP = 5,
    NEAR_RIGHT_TOP = 4
} CornerEnum;

typedef enum 
{
    /// Return no world geometry hits at all
    WFT_NONE,
    /// Return pointers to convex plane-bounded regions
    WFT_PLANE_BOUNDED_REGION,
    /// Return a single intersection point (typically RaySceneQuery only)
    WFT_SINGLE_INTERSECTION,
    /// Custom geometry as defined by the SceneManager
    WFT_CUSTOM_GEOMETRY,
    /// General RenderOperation structure
    WFT_RENDER_OPERATION
} world_fragment_type;


typedef enum
{
    LL_LOW = 1,
    LL_NORMAL = 2,
    LL_BOREME = 3
} logging_level;

typedef enum
{
    LML_TRIVIAL = 1,
    LML_NORMAL = 2,
    LML_CRITICAL = 3
} log_message_level;


typedef enum
{
    OR_DEGREE_0       = 0,
    OR_DEGREE_90      = 1,
    OR_DEGREE_180     = 2,
    OR_DEGREE_270     = 3,

    OR_PORTRAIT       = OR_DEGREE_0,
    OR_LANDSCAPERIGHT = OR_DEGREE_90,
    OR_LANDSCAPELEFT  = OR_DEGREE_270
} orientation_mode;

typedef enum
{
    PT_ORTHOGRAPHIC,
    PT_PERSPECTIVE
} projection_type;

typedef enum
{
    FRUSTUM_PLANE_NEAR   = 0,
    FRUSTUM_PLANE_FAR    = 1,
    FRUSTUM_PLANE_LEFT   = 2,
    FRUSTUM_PLANE_RIGHT  = 3,
    FRUSTUM_PLANE_TOP    = 4,
    FRUSTUM_PLANE_BOTTOM = 5
} frustum_plane;


typedef enum
{
    GMM_RELATIVE,
    GMM_PIXELS,
    GMM_RELATIVE_ASPECT_ADJUSTED
} gui_metrics_mode;

typedef enum
{
    GHA_LEFT,
    GHA_CENTER,
    GHA_RIGHT
} gui_horizontal_alignment;

typedef enum
{
    GVA_TOP,
    GVA_CENTER,
    GVA_BOTTOM
} gui_vertical_alignment;



typedef enum
{
    Left,
    Right,
    Center
} textarea_overlayelement_alignment;

#endif
