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

typedef void* RenderOperationHandle;
typedef void* VertexDataHandle;
typedef void* IndexDataHandle;

#include "ogre_interface.h"

DLL RenderOperationHandle create_renderoperation();
DLL void destroy_renderoperation(RenderOperationHandle handle);

//VertexData *vertexData;
DLL VertexDataHandle renderoperation_get_vertex_data(RenderOperationHandle handle);
DLL void renderoperation_set_vertex_data(RenderOperationHandle handle, VertexDataHandle vertex_data);
//OperationType operationType;
DLL operation_type renderoperation_get_operation_type(RenderOperationHandle handle);
DLL void renderoperation_set_operation_type(RenderOperationHandle handle, operation_type op_type);
//bool useIndexes;
DLL int renderoperation_get_use_indexes(RenderOperationHandle handle);
DLL void renderoperation_set_use_indexes(RenderOperationHandle, bool use_indexes);
//IndexData *indexData;
DLL IndexDataHandle renderoperation_get_index_data(RenderOperationHandle handle);
DLL void renderoperation_set_index_data(RenderOperationHandle handle, IndexDataHandle index_data);
//TODO: const Renderable* srcRenderable;
//size_t numberOfInstances;
DLL size_t renderoperation_get_number_of_instances(RenderOperationHandle handle);
DLL void renderoperation_set_number_of_instances(RenderOperationHandle handle, size_t num);
//bool useGlobalInstancingVertexBufferIsAvailable;
DLL int renderoperation_get_use_global_instancing_vertex_buffer_is_available(RenderOperationHandle handle);
DLL void renderoperation_set_use_global_instancing_vertex_buffer_is_available(RenderOperationHandle handle, int use);
