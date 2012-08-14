/******************************************************************************
 * scenequery_bind.h -  bindings for Ogre::SceneQuery
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
#ifndef SCENEQUERY_BIND_H
#define SCENEQUERY_BIND_H
#include "ogre_interface.h"

#define SceneQueryHandle void*
#define SceneQueryListenerHandle void*
#define SceneManagerHandle void*
#define RenderOperationHandle void*
#define PlaneListHandle void*
#define MovableObjectHandle void*
#define SceneQueryResultHandle void*

typedef struct world_fragment
{
    world_fragment_type fragment_type;
    coiVector3 single_intersection;
    PlaneListHandle planes;
    void* geometry;
    RenderOperationHandle render_op;
    
} world_fragment;


// No create/destroy methods for these, as this is the job of the SceneManager.
// SceneQuery::setQueryMask(uint32 mask)
DLL void scenequery_set_query_mask(SceneQueryHandle handle, uint32 mask);
//uint32 SceneQuery::getQueryMask(void) const
DLL uint32 scenequery_get_query_mask(SceneQueryHandle handle);
//void SceneQuery::setWorldFragmentType(enum WorldFragmentType wft);
DLL void scenequery_set_world_fragment_type(SceneQueryHandle handle, world_fragment_type wft);
//WorldFragmentType SceneQuery::getWorldFragmentType(void) const;
DLL world_fragment_type scenequery_get_world_fragment_type(SceneQueryHandle handle);

typedef int(*SceneQueryFragmentResult)(const world_fragment* frag, void* userdata);
typedef int(*SceneQueryObjectResult)(MovableObjectHandle handle, void* userdata);


// SceneQueryListener
DLL SceneQueryListenerHandle create_scenequerylistener(SceneQueryFragmentResult fragment_callback, SceneQueryObjectResult object_callback, void* userdata);
DLL void destroy_scenequerylistener(SceneQueryListenerHandle handle);

DLL int scenequeryresult_movables_count(SceneQueryResultHandle handle);
DLL MovableObjectHandle scenequeryresult_movables_at(SceneQueryResultHandle handle, int index);

DLL int scenequeryresult_worldfragments_count(SceneQueryResultHandle handle, int index);
DLL void scenequeryresult_worldfragments_at(SceneQueryResultHandle handle, int index, world_fragment* result);


#endif
