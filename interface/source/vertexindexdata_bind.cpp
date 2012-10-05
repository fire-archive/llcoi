/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "vertexindexdata_bind.h"
#include <OgreVertexIndexData.h>

//TODO: VertexData(HardwareBufferManagerBase* mgr = 0);
//TODO: VertexData(VertexDeclaration* dcl, VertexBufferBinding* bind);
//~VertexData();
void destroy_vertexdata(VertexDataHandle handle)
{
    Ogre::VertexData* vdata = static_cast<Ogre::VertexData*>(handle);
    delete vdata;
}

//(see OgreHardwareVertexBuffer.h): VertexDeclaration* vertexDeclaration;
//(see OgreHardwareVertexBuffer.h) VertexBufferBinding* vertexBufferBinding;
//size_t vertexStart;
size_t vertexdata_vertex_start(VertexDataHandle handle)
{
    Ogre::VertexData* vdata = static_cast<Ogre::VertexData*>(handle); 
    return vdata->vertexStart;
}

//size_t vertexCount;
size_t vertexdata_vertex_count(VertexDataHandle handle)
{
    Ogre::VertexData* vdata = static_cast<Ogre::VertexData*>(handle);
    return vdata->vertexCount;
}

//typedef vector<HardwareAnimationData>::type HardwareAnimationDataList;
//HardwareAnimationDataList hwAnimationDataList;
//size_t hwAnimDataItemsUsed;
size_t vertexdata_hw_anim_data_items_used(VertexDataHandle handle)
{
    Ogre::VertexData* vdata = static_cast<Ogre::VertexData*>(handle);
    return vdata->hwAnimDataItemsUsed;
}

//VertexData* clone(bool copyData = true, HardwareBufferManagerBase* mgr = 0) const;
//FIXME: return to this once HardwareBufferManagerBase is wrapped.
VertexDataHandle vertexdata_clone(const VertexDataHandle handle, int copy_data)
{
    const Ogre::VertexData* vdata = static_cast<const Ogre::VertexData*>(handle);
    Ogre::VertexData* cloned = vdata->clone(copy_data);
    return static_cast<VertexDataHandle>(cloned);
}

//void prepareForShadowVolume(void);
void vertexdata_prepare_for_shadow_volume(VertexDataHandle handle)
{
    Ogre::VertexData* vdata = static_cast<Ogre::VertexData*>(handle);
    vdata->prepareForShadowVolume();
}

//HardwareVertexBufferSharedPtr hardwareShadowVolWBuffer;
//TODO: void reorganiseBuffers(VertexDeclaration* newDeclaration, const BufferUsageList& bufferUsage, HardwareBufferManagerBase* mgr = 0);
//TODO: void reorganiseBuffers(VertexDeclaration* newDeclaration, HardwareBufferManagerBase* mgr = 0);
//void closeGapsInBindings(void);
void vertexdata_close_gaps_in_bindings(VertexDataHandle handle)
{
    Ogre::VertexData* vdata = static_cast<Ogre::VertexData*>(handle);
    vdata->closeGapsInBindings();
}

//void removeUnusedBuffers(void);
void vertexdata_remove_unused_buffers(VertexDataHandle handle)
{
    Ogre::VertexData* vdata = static_cast<Ogre::VertexData*>(handle);
    vdata->removeUnusedBuffers();
}

//TODO:void convertPackedColour(VertexElementType srcType, VertexElementType destType);
//ushort allocateHardwareAnimationElements(ushort count, bool animateNormals);
unsigned short vertexdata_allocate_hardware_animation_elements(VertexDataHandle handle, unsigned short count, int animate_normals)
{
    Ogre::VertexData* vdata = static_cast<Ogre::VertexData*>(handle);
    return vdata->allocateHardwareAnimationElements(count, animate_normals);
}

//IndexData();
IndexDataHandle create_indexdata()
{
    Ogre::IndexData* idata = new Ogre::IndexData;
    return static_cast<IndexDataHandle>(idata);
}

//~IndexData();
void destroy_indexdata(IndexDataHandle handle)
{
    Ogre::IndexData* idata = static_cast<Ogre::IndexData*>(handle);
    delete idata;
}

//HardwareIndexBufferSharedPtr indexBuffer;
//size_t indexStart;
size_t indexdata_index_start(IndexDataHandle handle)
{
    Ogre::IndexData* idata = static_cast<Ogre::IndexData*>(handle);
    return idata->indexStart;
}

//size_t indexCount;
size_t indexdata_index_count(IndexDataHandle handle)
{
    Ogre::IndexData* idata = static_cast<Ogre::IndexData*>(handle);
    return idata->indexCount;
}

//IndexData* clone(bool copyData = true, HardwareBufferManagerBase* mgr = 0) const;
IndexDataHandle indexdata_clone(const IndexDataHandle handle, int copy_data)
{
    const Ogre::IndexData* idata = static_cast<const Ogre::IndexData*>(handle);
    Ogre::IndexData* cloned = idata->clone(copy_data);
}

//void optimiseVertexCacheTriList(void);
void indexdata_optimise_vertex_cache_tri_list(IndexDataHandle handle)
{
    Ogre::IndexData* idata = static_cast<Ogre::IndexData*>(handle);
    idata->optimiseVertexCacheTriList();
}

