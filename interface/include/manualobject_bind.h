/******************************************************************************
 * manualobject_bind.h - bindings for Ogre::ManualObject
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
#ifndef MANUALOBJECT_BIND_H
#define MANUALOBJECT_BIND_H

#include "ogre_interface.h"
#define ManualObjectHandle void*

//ManualObject(const String& name);
DLL ManualObjectHandle create_manualobject(const char* name);
//~ManualObject();
DLL void destroy_manualobject(ManualObjectHandle handle);
//void clear(void);
DLL void manualobject_clear(ManualObjectHandle handle);
//void estimateVertexCount(size_t vcount);
DLL void manualobject_estimate_vertex_count(ManualObjectHandle handle, size_t vcount);
//void estimateIndexCount(size_t icount);
DLL void manualobject_estimate_index_count(ManualObjectHandle handle, size_t icount);
//void begin(const String& materialName, RenderOperation::OperationType opType = RenderOperation::OT_TRIANGLE_LIST, const String & groupName = ResourceGroupManager::DEFAULT_RESOURCE_GROUP_NAME);
DLL void manualobject_begin(ManualObjectHandle handle, const char* material_name, operation_type op_type, const char* group_name);
//void setDynamic(bool dyn) 
DLL void manualobject_set_dynamic(ManualObjectHandle handle, int dyn);
//bool getDynamic() const
DLL int manualobject_get_dynamic(const ManualObjectHandle handle);
//void beginUpdate(size_t sectionIndex);
DLL void manualobject_begin_update(ManualObjectHandle handle, size_t section_index);
//void position(const Vector3& pos);
//void position(Real x, Real y, Real z);
DLL void manualobject_position(ManualObjectHandle handle, const coiVector3* pos);
//void normal(const Vector3& norm);
//void normal(Real x, Real y, Real z);
DLL void manualobject_normal(ManualObjectHandle handle, const coiVector3* norm);
//void tangent(const Vector3& tan);
//void tangent(Real x, Real y, Real z);
DLL void manualobject_tangent(ManualObjectHandle handle, const coiVector3* tan);
//void textureCoord(Real u);
DLL void manualobject_texture_coord_u(ManualObjectHandle handle, coiReal u);
//void textureCoord(Real u, Real v);
//void textureCoord(Real u, Real v, Real w);
//void textureCoord(Real x, Real y, Real z, Real w);
//void textureCoord(const Vector2& uv);
DLL void manualobject_texture_coord_uv(ManualObjectHandle handle, const coiVector3* uv);
//void textureCoord(const Vector3& uvw);
DLL void manualobject_texture_coord_uvw(ManualObjectHandle handle, const coiVector3* uvw);
//void textureCoord(const Vector4& xyzw);
DLL void manualobject_texture_coord_xyxw(ManualObjectHandle handle, const coiVector3* xyzw);
//void colour(const ColourValue& col);
DLL void manualobject_colour(ManualObjectHandle handle, const coiColourValue* col);
//void colour(Real r, Real g, Real b, Real a = 1.0f);
//void index(uint32 idx);
DLL void manualobject_index(ManualObjectHandle handle, uint32 idx);
//void triangle(uint32 i1, uint32 i2, uint32 i3);
DLL void manualobject_triangle(ManualObjectHandle handle, uint32 i1, uint32 i2, uint32 i3);
//void quad(uint32 i1, uint32 i2, uint32 i3, uint32 i4);
DLL void manualobject_quad(ManualObjectHandle handle, uint32 i1, uint32 i2, uint32 i3, uint32 i4);
//size_t getCurrentVertexCount() const;
DLL size_t  manualobject_get_current_vertex_count(const ManualObjectHandle handle);
//size_t getCurrentIndexCount() const;
DLL size_t manualobject_get_current_index_count(const ManualObjectHandle handle);
//ManualObjectSection* end(void);
//void setMaterialName(size_t subIndex, const String& name, const String & group = ResourceGroupManager::DEFAULT_RESOURCE_GROUP_NAME);
//MeshPtr convertToMesh(const String& meshName, const String& groupName = ResourceGroupManager::DEFAULT_RESOURCE_GROUP_NAME);
//void setUseIdentityProjection(bool useIdentityProjection);
//bool getUseIdentityProjection(void) const;
//void setUseIdentityView(bool useIdentityView);
//bool getUseIdentityView(void) const;
//void setBoundingBox(const AxisAlignedBox& box);
//TODO: ManualObjectSection* getSection(unsigned int index) const;
//unsigned int getNumSections(void) const;
//void setKeepDeclarationOrder(bool keepOrder);
//bool getKeepDeclarationOrder() const;
//const String& getMovableType(void) const;
//const AxisAlignedBox& getBoundingBox(void) const;
//Real getBoundingRadius(void) const;
//void _updateRenderQueue(RenderQueue* queue);
//EdgeData* getEdgeList(void);
//bool hasEdgeList(void);
//ShadowRenderableListIterator getShadowVolumeRenderableIterator(ShadowTechnique shadowTechnique, const Light* light, HardwareIndexBufferSharedPtr* indexBuffer,  bool extrudeVertices, Real extrusionDist, unsigned long flags = 0);

#endif
