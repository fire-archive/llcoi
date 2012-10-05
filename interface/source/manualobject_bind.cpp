/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
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
void manualobject_position(ManualObjectHandle handle, const coiVector3* p)
{
    Ogre::ManualObject* obj = static_cast<Ogre::ManualObject*>(handle);
    const Ogre::Vector3 pos(p->x, p->y, p->z);
    obj->position(pos);
}

//void normal(const Vector3& norm)
//void normal(Real x, Real y, Real z)
void manualobject_normal(ManualObjectHandle handle, const coiVector3* n)
{
    Ogre::ManualObject* obj = static_cast<Ogre::ManualObject*>(handle);
    const Ogre::Vector3 norm(n->x, n->y, n->z);
    obj->normal(norm);
}

//void tangent(const Vector3& tan)
//void tangent(Real x, Real y, Real z)
void manualobject_tangent(ManualObjectHandle handle, const coiVector3* t)
{
    Ogre::ManualObject* obj = static_cast<Ogre::ManualObject*>(handle);
    const Ogre::Vector3 tan(t->x, t->y, t->z);
    obj->tangent(tan);
}

//void textureCoord(Real u)
void manualobject_texture_coord_u(ManualObjectHandle handle, coiReal u)
{
    Ogre::ManualObject* obj = static_cast<Ogre::ManualObject*>(handle);
    obj->textureCoord(u);
}

//void textureCoord(Real u, Real v)
//void textureCoord(Real u, Real v, Real w)
//void textureCoord(Real x, Real y, Real z, Real w)
//void textureCoord(const Vector2& uv)
void manualobject_texture_coord_uv(ManualObjectHandle handle, const coiVector2* uv)
{
    Ogre::ManualObject* obj = static_cast<Ogre::ManualObject*>(handle);
    const Ogre::Vector2 UV(uv->x, uv->y);
    obj->textureCoord(UV);
}

//void textureCoord(const Vector3& uvw)
void manualobject_texture_coord_uvw(ManualObjectHandle handle, const coiVector3* uvw)
{
    Ogre::ManualObject* obj = static_cast<Ogre::ManualObject*>(handle);
    const Ogre::Vector3 UVW(uvw->x, uvw->y, uvw->z);
    obj->textureCoord(UVW);
}

//void textureCoord(const Vector4& xyzw)
void manualobject_texture_coord_xyxw(ManualObjectHandle handle, const coiVector4* xyzw)
{
    Ogre::ManualObject* obj = static_cast<Ogre::ManualObject*>(handle);
    const Ogre::Vector3 XYZW(xyzw->x, xyzw->y, xyzw->z);
    obj->textureCoord(XYZW);
}

//void colour(const ColourValue& col)
void manualobject_colour(ManualObjectHandle handle, const coiColourValue* col)
{
    Ogre::ManualObject* obj = static_cast<Ogre::ManualObject*>(handle);
    Ogre::ColourValue c(col->r, col->b, col->g, col->a);
    obj->colour(c);
}

//void colour(Real r, Real g, Real b, Real a = 1.0f)
//void index(uint32 idx)
void manualobject_index(ManualObjectHandle handle, uint32 idx)
{
    Ogre::ManualObject* obj = static_cast<Ogre::ManualObject*>(handle);
    obj->index(idx);
}

//void triangle(uint32 i1, uint32 i2, uint32 i3)
void manualobject_triangle(ManualObjectHandle handle, uint32 i1, uint32 i2, uint32 i3)
{
    Ogre::ManualObject* obj = static_cast<Ogre::ManualObject*>(handle);
    obj->triangle(i1, i2, i3);
}

//void quad(uint32 i1, uint32 i2, uint32 i3, uint32 i4)
void manualobject_quad(ManualObjectHandle handle, uint32 i1, uint32 i2, uint32 i3, uint32 i4)
{
    Ogre::ManualObject* obj = static_cast<Ogre::ManualObject*>(handle);
    obj->quad(i1, i2, i3, i4);
}

//size_t getCurrentVertexCount() const
size_t  manualobject_get_current_vertex_count(const ManualObjectHandle handle)
{
    const Ogre::ManualObject* obj = static_cast<const Ogre::ManualObject*>(handle);
    return obj->getCurrentVertexCount();
}

//size_t getCurrentIndexCount() const
size_t manualobject_get_current_index_count(const ManualObjectHandle handle)
{
    const Ogre::ManualObject* obj = static_cast<const Ogre::ManualObject*>(handle);
    return obj->getCurrentIndexCount();
}

//ManualObjectSection* end(void)
ManualObjectSectionHandle manualobject_end(ManualObjectHandle handle)
{
    Ogre::ManualObject* obj = static_cast<Ogre::ManualObject*>(handle);
    Ogre::ManualObject::ManualObjectSection* section = obj->end();
    return static_cast<ManualObjectSectionHandle>(section);
}

//void setMaterialName(size_t subIndex, const String& name, const String & group = ResourceGroupManager::DEFAULT_RESOURCE_GROUP_NAME)
void manualobject_set_material_name(ManualObjectHandle handle, size_t sub_index, const char* name, const char* group)
{
    Ogre::ManualObject* obj = static_cast<Ogre::ManualObject*>(handle);
    obj->setMaterialName(sub_index, Ogre::String(name), Ogre::String(group));
}

//MeshPtr convertToMesh(const String& meshName, const String& groupName = ResourceGroupManager::DEFAULT_RESOURCE_GROUP_NAME)
MeshHandle manualobject_convert_to_mesh(ManualObjectHandle handle, const char* mesh_name, const char* group_name)
{
    Ogre::ManualObject* obj = static_cast<Ogre::ManualObject*>(handle);
    Ogre::MeshPtr ptr = obj->convertToMesh(Ogre::String(mesh_name), Ogre::String(group_name));
    return static_cast<MeshHandle>(ptr.get());
}


//void setUseIdentityProjection(bool useIdentityProjection)
void manualobject_set_use_identity_projection(ManualObjectHandle handle, bool use_identity_projection)
{
    Ogre::ManualObject* obj = static_cast<Ogre::ManualObject*>(handle);
    obj->setUseIdentityProjection(use_identity_projection);
}

//bool getUseIdentityProjection(void) const
int manualobject_get_use_identity_projection(const ManualObjectHandle handle)
{
    const Ogre::ManualObject* obj = static_cast<const Ogre::ManualObject*>(handle);
    return obj->getUseIdentityProjection();
}


//void setUseIdentityView(bool useIdentityView)
void manualobject_set_use_identity_view(ManualObjectHandle handle, int use_identity_view)
{
    Ogre::ManualObject* obj = static_cast<Ogre::ManualObject*>(handle);
    obj->setUseIdentityView(use_identity_view);
}

//bool getUseIdentityView(void) const
int manualobject_get_use_identity_view(const ManualObjectHandle handle)
{
    const Ogre::ManualObject* obj = static_cast<const Ogre::ManualObject*>(handle);
    return obj->getUseIdentityView();
}

//void setBoundingBox(const AxisAlignedBox& box)
void manualobject_set_bounding_box(ManualObjectHandle handle, const AxisAlignedBoxHandle box)
{
    Ogre::ManualObject* obj = static_cast<Ogre::ManualObject*>(handle);
    const Ogre::AxisAlignedBox* aabb = static_cast<const Ogre::AxisAlignedBox*>(box);
    obj->setBoundingBox(*aabb);
}

//ManualObjectSection* getSection(unsigned int index) const
ManualObjectSectionHandle manualobject_get_section(const ManualObjectHandle handle, unsigned int index)
{
    const Ogre::ManualObject* obj = static_cast<const Ogre::ManualObject*>(handle);
    Ogre::ManualObject::ManualObjectSection* section = obj->getSection(index);
    return static_cast<ManualObjectSectionHandle>(section);
}

//unsigned int getNumSections(void) const
unsigned int manualobject_get_num_sections(const ManualObjectHandle handle)
{
    const Ogre::ManualObject* obj = static_cast<const Ogre::ManualObject*>(handle);
    return obj->getNumSections();
}

//void setKeepDeclarationOrder(bool keepOrder)
void manualobject_set_keep_declaration_order(ManualObjectHandle handle, int keep_order)
{
    Ogre::ManualObject* obj = static_cast<Ogre::ManualObject*>(handle);
    obj->setKeepDeclarationOrder(keep_order);
}

//bool getKeepDeclarationOrder() const
int manualobject_get_keep_declaration_order(const ManualObjectHandle handle)
{
    const Ogre::ManualObject* obj = static_cast<const Ogre::ManualObject*>(handle);
    return obj->getKeepDeclarationOrder();
}

//const String& getMovableType(void) const
const char* manualobject_get_movable_type(const ManualObjectHandle handle)
{
    const Ogre::ManualObject* obj = static_cast<const Ogre::ManualObject*>(handle);
    return obj->getMovableType().c_str();
}

//const AxisAlignedBox& getBoundingBox(void) const
AxisAlignedBoxHandle manualobject_get_bounding_box(const ManualObjectHandle handle)
{
    const Ogre::ManualObject* obj = static_cast<const Ogre::ManualObject*>(handle);
    const Ogre::AxisAlignedBox& box = obj->getBoundingBox();
    // TODO: find way to not have to remove constness
    return static_cast<AxisAlignedBoxHandle>(const_cast<Ogre::AxisAlignedBox*>(&box));
}

//Real getBoundingRadius(void) const
coiReal manualobject_get_bounding_radius(const ManualObjectHandle handle)
{
    const Ogre::ManualObject* obj = static_cast<const Ogre::ManualObject*>(handle);
    return obj->getBoundingRadius();
}

//TODO: void _updateRenderQueue(RenderQueue* queue)
//TODO: EdgeData* getEdgeList(void)
//bool hasEdgeList(void)
int manualobject_has_edge_list(ManualObjectHandle handle)
{
    Ogre::ManualObject* obj = static_cast<Ogre::ManualObject*>(handle);
    return obj->hasEdgeList();
}

//TODO: ShadowRenderableListIterator getShadowVolumeRenderableIterator(ShadowTechnique shadowTechnique, const Light* light, HardwareIndexBufferSharedPtr* indexBuffer,  bool extrudeVertices, Real extrusionDist, unsigned long flags = 0)


// Ogre::ManualObject::ManualObjectSection

//ManualObjectSection(ManualObject* parent, const String& materialName, RenderOperation::OperationType opType, const String & groupName = ResourceGroupManager::DEFAULT_RESOURCE_GROUP_NAME); 
ManualObjectSectionHandle create_manualobjectsection(ManualObjectHandle parent, const char* material_name, operation_type op_type, const char* group_name)
{
    Ogre::ManualObject* obj = static_cast<Ogre::ManualObject*>(parent); 
    Ogre::RenderOperation::OperationType opType = llcoi_operation_type_to_ogre(op_type);
    Ogre::ManualObject::ManualObjectSection* section = new Ogre::ManualObject::ManualObjectSection(obj, Ogre::String(material_name), opType, Ogre::String(group_name));
    return static_cast<ManualObjectSectionHandle>(section);
}

void destroy_manualobjectsection(ManualObjectSectionHandle handle)
{
    Ogre::ManualObject::ManualObjectSection* section = static_cast<Ogre::ManualObject::ManualObjectSection*>(handle); 
    delete section;
}


//RenderOperation* getRenderOperation(void)
RenderOperationHandle manualobjectsection_get_render_operation(ManualObjectSectionHandle handle)
{
    Ogre::ManualObject::ManualObjectSection* section = static_cast<Ogre::ManualObject::ManualObjectSection*>(handle);
    return static_cast<RenderOperationHandle>(section->getRenderOperation());
}

//const String& getMaterialName(void) const
const char* manualobjectsection_get_material_name(const ManualObjectSectionHandle handle)
{
    const Ogre::ManualObject::ManualObjectSection* section = static_cast<const Ogre::ManualObject::ManualObjectSection*>(handle);
    return section->getMaterialName().c_str();
}

//const String& getMaterialGroup(void) const
const char* manualobjectsection_get_material_group(const ManualObjectSectionHandle handle)
{
    const Ogre::ManualObject::ManualObjectSection* section = static_cast<const Ogre::ManualObject::ManualObjectSection*>(handle);
    return section->getMaterialGroup().c_str();
}

//void setMaterialName(const String& name, const String& groupName = ResourceGroupManager::AUTODETECT_RESOURCE_GROUP_NAME )
void manualobjectsection_set_material_name(ManualObjectSectionHandle handle, const char* name, const char* group_name)
{
    Ogre::ManualObject::ManualObjectSection* section = static_cast<Ogre::ManualObject::ManualObjectSection*>(handle);
    section->setMaterialName(Ogre::String(name), Ogre::String(group_name));
}

//void set32BitIndices(bool n32)
void manualobjectsection_set_32_bit_indices(ManualObjectSectionHandle handle, int n32)
{
    Ogre::ManualObject::ManualObjectSection* section = static_cast<Ogre::ManualObject::ManualObjectSection*>(handle);
    section->set32BitIndices(n32);
}

//bool get32BitIndices() const
int manualobjectsection_get_32_bit_indices(const ManualObjectSectionHandle handle)
{
    const Ogre::ManualObject::ManualObjectSection* section = static_cast<const Ogre::ManualObject::ManualObjectSection*>(handle);
    return section->get32BitIndices();
}

//TODO:const MaterialPtr& getMaterial(void) const
//void getRenderOperation(RenderOperation& op)
void manualobjectsection_renderable_get_render_operation(ManualObjectSectionHandle handle, RenderOperationHandle renderOp)
{
    Ogre::ManualObject::ManualObjectSection* section = static_cast<Ogre::ManualObject::ManualObjectSection*>(handle);
    Ogre::RenderOperation* op = static_cast<Ogre::RenderOperation*>(renderOp);
    section->getRenderOperation(*op);
}

//void getWorldTransforms(Matrix4* xform) const
void manualobjectsection_get_world_transforms(const ManualObjectSectionHandle handle, coiMatrix4* xform)
{
    const Ogre::ManualObject::ManualObjectSection* section = static_cast<const Ogre::ManualObject::ManualObjectSection*>(handle);
    Ogre::Matrix4 m;
    section->getWorldTransforms(&m);
    ogre_matrix4_to_llcoi_matrix4(m, *xform);
}

//Real getSquaredViewDepth(const Ogre::Camera *) const
coiReal manualobjectsection_get_squared_view_depth(const ManualObjectSectionHandle handle, const CameraHandle cam)
{
    const Ogre::ManualObject::ManualObjectSection* section = static_cast<const Ogre::ManualObject::ManualObjectSection*>(handle);
    const Ogre::Camera* c = static_cast<const Ogre::Camera*>(cam);
    return section->getSquaredViewDepth(c);
}

//TODO: const LightList &getLights(void) const

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
