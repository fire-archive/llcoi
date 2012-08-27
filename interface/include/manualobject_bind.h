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
#define ManualObjectSectionHandle void*
#define MeshHandle void*
#define AxisAlignedBoxHandle void*

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
DLL void manualobject_texture_coord_uv(ManualObjectHandle handle, const coiVector2* uv);
//void textureCoord(const Vector3& uvw);
DLL void manualobject_texture_coord_uvw(ManualObjectHandle handle, const coiVector3* uvw);
//void textureCoord(const Vector4& xyzw);
DLL void manualobject_texture_coord_xyxw(ManualObjectHandle handle, const coiVector4* xyzw);
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
DLL ManualObjectSectionHandle manualobject_end(ManualObjectHandle handle);
//void setMaterialName(size_t subIndex, const String& name, const String & group = ResourceGroupManager::DEFAULT_RESOURCE_GROUP_NAME);
DLL void manualobject_set_material_name(ManualObjectHandle handle, size_t sub_index, const char* name, const char* group);
//MeshPtr convertToMesh(const String& meshName, const String& groupName = ResourceGroupManager::DEFAULT_RESOURCE_GROUP_NAME);
DLL MeshHandle manualobject_convert_to_mesh(ManualObjectHandle handle, const char* mesh_name, const char* group_name);
//void setUseIdentityProjection(bool useIdentityProjection);
DLL void manualobject_set_use_identity_projection(ManualObjectHandle handle, bool use_identity_projection);
//bool getUseIdentityProjection(void) const;
DLL int manualobject_get_use_identity_projection(const ManualObjectHandle handle);
//void setUseIdentityView(bool useIdentityView);
DLL void manualobject_set_use_identity_view(ManualObjectHandle handle, int use_identity_view);
//bool getUseIdentityView(void) const;
DLL int manualobject_get_use_identity_view(const ManualObjectHandle handle);
//void setBoundingBox(const AxisAlignedBox& box);
DLL void manualobject_set_bounding_box(ManualObjectHandle handle, const AxisAlignedBoxHandle box);
//ManualObjectSection* getSection(unsigned int index) const;
DLL ManualObjectSectionHandle manualobject_get_section(const ManualObjectHandle handle, unsigned int index);
//unsigned int getNumSections(void) const;
DLL unsigned int manualobject_get_num_sections(const ManualObjectHandle handle);
//void setKeepDeclarationOrder(bool keepOrder);
DLL void manualobject_set_keep_declaration_order(ManualObjectHandle handle, int keep_order);
//bool getKeepDeclarationOrder() const;
DLL int manualobject_get_keep_declaration_order(const ManualObjectHandle handle);
//const String& getMovableType(void) const;
DLL const char* manualobject_get_movable_type(const ManualObjectHandle handle);
//const AxisAlignedBox& getBoundingBox(void) const;
DLL const AxisAlignedBoxHandle manualobject_get_bounding_box(const ManualObjectHandle handle);
//Real getBoundingRadius(void) const;
DLL coiReal manualobject_get_bounding_radius(const ManualObjectHandle handle);
//TODO: void _updateRenderQueue(RenderQueue* queue);
//TODO: EdgeData* getEdgeList(void);
//bool hasEdgeList(void);
DLL int manualobject_has_edge_list(ManualObjectHandle handle);
//TODO: ShadowRenderableListIterator getShadowVolumeRenderableIterator(ShadowTechnique shadowTechnique, const Light* light, HardwareIndexBufferSharedPtr* indexBuffer,  bool extrudeVertices, Real extrusionDist, unsigned long flags = 0);

//Ogre::ManualObjectSection
//ManualObjectSection(ManualObject* parent, const String& materialName, RenderOperation::OperationType opType, const String & groupName = ResourceGroupManager::DEFAULT_RESOURCE_GROUP_NAME); 
DLL ManualObjectSectionHandle create_manualobjectsection(ManualObjectHandle parent, const char* material_name, operation_type op_type, const char* group_name);
//~ManualObjectSection();
DLL void destroy_manualobjectsection(ManualObjectSectionHandle handle);
//RenderOperation* getRenderOperation(void);
//manualobjectsection_

#endif
