/******************************************************************************
 * manualobject_bind.cpp - bindings for Ogre::ManualObject
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
#include "manualobject_bind.h"
#include "binding_utils.h"
#include <OgreManualObject.h>

//ManualObject(const String& name)
ManualObjectHandle create_manualobject(const char* name)
{
    Ogre::ManualObject* obj = new Ogre::ManualObject(Ogre::String(name));
    return static_cast<ManualObjectHandle>(obj);
}

//~ManualObject()
void destroy_manualobject(ManualObjectHandle handle)
{
    Ogre::ManualObject* obj = static_cast<Ogre::ManualObject*>(handle);
    delete obj;
}

//void clear(void)
void manualobject_clear(ManualObjectHandle handle)
{
    Ogre::ManualObject* obj = static_cast<Ogre::ManualObject*>(handle);
    obj->clear();
}

//void estimateVertexCount(size_t vcount)
void manualobject_estimate_vertex_count(ManualObjectHandle handle, size_t vcount)
{
    Ogre::ManualObject* obj = static_cast<Ogre::ManualObject*>(handle);
    obj->estimateVertexCount(vcount);
}

//void estimateIndexCount(size_t icount)
void manualobject_estimate_index_count(ManualObjectHandle handle, size_t icount)
{
    Ogre::ManualObject* obj = static_cast<Ogre::ManualObject*>(handle);
    obj->estimateIndexCount(icount);
}

//void begin(const String& materialName, RenderOperation::OperationType opType = RenderOperation::OT_TRIANGLE_LIST, const String & groupName = ResourceGroupManager::DEFAULT_RESOURCE_GROUP_NAME)
void manualobject_begin(ManualObjectHandle handle, const char* material_name, operation_type op_type, const char* group_name)
{
    Ogre::ManualObject* obj = static_cast<Ogre::ManualObject*>(handle);
    Ogre::RenderOperation::OperationType opType = llcoi_operation_type_to_ogre(op_type);
    obj->begin(Ogre::String(material_name), opType, Ogre::String(group_name));
}

//void setDynamic(bool dyn)
void manualobject_set_dynamic(ManualObjectHandle handle, int dyn)
{
    Ogre::ManualObject* obj = static_cast<Ogre::ManualObject*>(handle);
    obj->setDynamic(dyn);
}

//bool getDynamic() const
int manualobject_get_dynamic(const ManualObjectHandle handle)
{
    const Ogre::ManualObject* obj = static_cast<const Ogre::ManualObject*>(handle);
    return obj->getDynamic();
}

//void beginUpdate(size_t sectionIndex)
void manualobject_begin_update(ManualObjectHandle handle, size_t section_index)
{
    Ogre::ManualObject* obj = static_cast<Ogre::ManualObject*>(handle);
    obj->beginUpdate(section_index);
}

//void position(const Vector3& pos)
//void position(Real x, Real y, Real z)
void manualobject_position(ManualObjectHandle handle, const coiVector3* pos)
{

}

//void normal(const Vector3& norm)
//void normal(Real x, Real y, Real z)
void manualobject_normal(ManualObjectHandle handle, const coiVector3* norm)
{

}

//void tangent(const Vector3& tan)
//void tangent(Real x, Real y, Real z)
void manualobject_tangent(ManualObjectHandle handle, const coiVector3* tan)
{

}

//void textureCoord(Real u)
void manualobject_texture_coord_u(ManualObjectHandle handle, coiReal u)
{

}

//void textureCoord(Real u, Real v)
//void textureCoord(Real u, Real v, Real w)
//void textureCoord(Real x, Real y, Real z, Real w)
//void textureCoord(const Vector2& uv)
void manualobject_texture_coord_uv(ManualObjectHandle handle, const coiVector3* uv)
{

}

//void textureCoord(const Vector3& uvw)
void manualobject_texture_coord_uvw(ManualObjectHandle handle, const coiVector3* uvw)
{

}

//void textureCoord(const Vector4& xyzw)
void manualobject_texture_coord_xyxw(ManualObjectHandle handle, const coiVector3* xyzw)
{

}

//void colour(const ColourValue& col)
void manualobject_colour(ManualObjectHandle handle, const coiColourValue* col)
{

}

//void colour(Real r, Real g, Real b, Real a = 1.0f)
//void index(uint32 idx)
void manualobject_index(ManualObjectHandle handle, uint32 idx)
{

}

//void triangle(uint32 i1, uint32 i2, uint32 i3)
void manualobject_triangle(ManualObjectHandle handle, uint32 i1, uint32 i2, uint32 i3)
{

}

//void quad(uint32 i1, uint32 i2, uint32 i3, uint32 i4)
void manualobject_quad(ManualObjectHandle handle, uint32 i1, uint32 i2, uint32 i3, uint32 i4)
{

}

//size_t getCurrentVertexCount() const
size_t  manualobject_get_current_vertex_count(const ManualObjectHandle handle)
{

}

//size_t getCurrentIndexCount() const
size_t manualobject_get_current_index_count(const ManualObjectHandle handle)
{

}


/*
Ogre::ManualObject::ManualObjectSection
Ogre::ManualObject::ManualObjectSectionShadowRenderable
Ogre::ManualObject::operator=(Ogre::ManualObject const&)
Ogre::ManualObject::ManualObject(Ogre::ManualObject const&)
Ogre::ManualObject::ManualObject(std::string const&)
Ogre::ManualObject::~ManualObject()
Ogre::ManualObject::clear()
Ogre::ManualObject::estimateVertexCount(unsigned int)
Ogre::ManualObject::estimateIndexCount(unsigned int)
Ogre::ManualObject::begin(std::string const&, Ogre::RenderOperation::OperationType, std::string const&)
Ogre::ManualObject::setDynamic(bool)
Ogre::ManualObject::getDynamic() const
Ogre::ManualObject::beginUpdate(unsigned int)
Ogre::ManualObject::position(Ogre::Vector3 const&)
Ogre::ManualObject::position(float, float, float)
Ogre::ManualObject::normal(Ogre::Vector3 const&)
Ogre::ManualObject::normal(float, float, float)
Ogre::ManualObject::tangent(Ogre::Vector3 const&)
Ogre::ManualObject::tangent(float, float, float)
Ogre::ManualObject::textureCoord(float)
Ogre::ManualObject::textureCoord(float, float)
Ogre::ManualObject::textureCoord(float, float, float)
Ogre::ManualObject::textureCoord(float, float, float, float)
Ogre::ManualObject::textureCoord(Ogre::Vector2 const&)
Ogre::ManualObject::textureCoord(Ogre::Vector3 const&)
Ogre::ManualObject::textureCoord(Ogre::Vector4 const&)
Ogre::ManualObject::colour(Ogre::ColourValue const&)
Ogre::ManualObject::colour(float, float, float, float)
Ogre::ManualObject::index(unsigned int)
Ogre::ManualObject::triangle(unsigned int, unsigned int, unsigned int)
Ogre::ManualObject::quad(unsigned int, unsigned int, unsigned int, unsigned int)
Ogre::ManualObject::end()
Ogre::ManualObject::setMaterialName(unsigned int, std::string const&, std::string const&)
Ogre::ManualObject::convertToMesh(std::string const&, std::string const&)
Ogre::ManualObject::setUseIdentityProjection(bool)
Ogre::ManualObject::getUseIdentityProjection() const
Ogre::ManualObject::setUseIdentityView(bool)
Ogre::ManualObject::getUseIdentityView() const
Ogre::ManualObject::setBoundingBox(Ogre::AxisAlignedBox const&)
Ogre::ManualObject::getSection(unsigned int) const
Ogre::ManualObject::getNumSections() const
Ogre::ManualObject::setKeepDeclarationOrder(bool)
Ogre::ManualObject::getKeepDeclarationOrder() const
Ogre::ManualObject::getMovableType() const
Ogre::ManualObject::getBoundingBox() const
Ogre::ManualObject::getBoundingRadius() const
Ogre::ManualObject::_updateRenderQueue(Ogre::RenderQueue*)
Ogre::ManualObject::getEdgeList()
Ogre::ManualObject::hasEdgeList()
Ogre::ManualObject::getShadowVolumeRenderableIterator(Ogre::ShadowTechnique, Ogre::Light const*, Ogre::HardwareIndexBufferSharedPtr*, bool, float, unsigned long)
Ogre::ManualObject::visitRenderables(Ogre::Renderable::Visitor*, bool)
Ogre::ManualObject::ManualObjectSection::operator=(Ogre::ManualObject::ManualObjectSection const&)
Ogre::ManualObject::ManualObjectSection::ManualObjectSection(Ogre::ManualObject::ManualObjectSection const&)
Ogre::ManualObject::ManualObjectSection::ManualObjectSection(Ogre::ManualObject*, std::string const&, Ogre::RenderOperation::OperationType, std::string const&)
Ogre::ManualObject::ManualObjectSection::~ManualObjectSection()
Ogre::ManualObject::ManualObjectSection::getRenderOperation()
Ogre::ManualObject::ManualObjectSection::getMaterialName() const
Ogre::ManualObject::ManualObjectSection::getMaterialGroup() const
Ogre::ManualObject::ManualObjectSection::setMaterialName(std::string const&, std::string const&)
Ogre::ManualObject::ManualObjectSection::set32BitIndices(bool)
Ogre::ManualObject::ManualObjectSection::get32BitIndices() const
Ogre::ManualObject::ManualObjectSection::getMaterial() const
Ogre::ManualObject::ManualObjectSection::getRenderOperation(Ogre::RenderOperation&)
Ogre::ManualObject::ManualObjectSection::getWorldTransforms(Ogre::Matrix4*) const
Ogre::ManualObject::ManualObjectSection::getSquaredViewDepth(Ogre::Camera const*) const
Ogre::ManualObject::ManualObjectSection::getLights() const
Ogre::ManualObject::ManualObjectSectionShadowRenderable::operator=(Ogre::ManualObject::ManualObjectSectionShadowRenderable const&)
Ogre::ManualObject::ManualObjectSectionShadowRenderable::ManualObjectSectionShadowRenderable(Ogre::ManualObject::ManualObjectSectionShadowRenderable const&)
Ogre::ManualObject::ManualObjectSectionShadowRenderable::ManualObjectSectionShadowRenderable(Ogre::ManualObject*, Ogre::HardwareIndexBufferSharedPtr*, Ogre::VertexData const*, bool, bool)
Ogre::ManualObject::ManualObjectSectionShadowRenderable::~ManualObjectSectionShadowRenderable()
Ogre::ManualObject::ManualObjectSectionShadowRenderable::getWorldTransforms(Ogre::Matrix4*) const
Ogre::ManualObject::ManualObjectSectionShadowRenderable::getPositionBuffer()
Ogre::ManualObject::ManualObjectSectionShadowRenderable::getWBuffer()
Ogre::ManualObject::TempVertex::~TempVertex()
Ogre::ManualObject::TempVertex::operator=(Ogre::ManualObject::TempVertex const&)
Ogre::ManualObject::TempVertex::TempVertex(Ogre::ManualObject::TempVertex const&)
Ogre::ManualObject::TempVertex::TempVertex()
*/
