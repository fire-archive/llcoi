/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "renderoperation_bind.h"
#include "binding_utils.h"
#include <OgreRenderOperation.h>

RenderOperationHandle create_renderoperation()
{
    Ogre::RenderOperation* renderOp = new Ogre::RenderOperation;
    return static_cast<RenderOperationHandle>(renderOp);
}

void destroy_renderoperation(RenderOperationHandle handle)
{
    Ogre::RenderOperation* renderOp = static_cast<Ogre::RenderOperation*>(handle); 
    delete renderOp;
}


//VertexData *vertexData
VertexDataHandle renderoperation_get_vertex_data(RenderOperationHandle handle)
{
    Ogre::RenderOperation* renderOp = static_cast<Ogre::RenderOperation*>(handle);
    return static_cast<VertexDataHandle>(renderOp->vertexData);
}

void renderoperation_set_vertex_data(RenderOperationHandle handle, VertexDataHandle vertex_data)
{
    Ogre::RenderOperation* renderOp = static_cast<Ogre::RenderOperation*>(handle); 
    renderOp->vertexData = static_cast<Ogre::VertexData*>(vertex_data);

}

//OperationType operationType
operation_type renderoperation_get_operation_type(RenderOperationHandle handle)
{
    Ogre::RenderOperation* renderOp = static_cast<Ogre::RenderOperation*>(handle);
    return ogre_operation_type_to_llcoi(renderOp->operationType);
}

void renderoperation_set_operation_type(RenderOperationHandle handle, operation_type op_type)
{
    Ogre::RenderOperation* renderOp = static_cast<Ogre::RenderOperation*>(handle);
    renderOp->operationType = llcoi_operation_type_to_ogre(op_type);
}

//bool useIndexes
int renderoperation_get_use_indexes(RenderOperationHandle handle)
{
    Ogre::RenderOperation* renderOp = static_cast<Ogre::RenderOperation*>(handle);
    return renderOp->useIndexes;
}

void renderoperation_set_use_indexes(RenderOperationHandle handle, bool use_indexes)
{
    Ogre::RenderOperation* renderOp = static_cast<Ogre::RenderOperation*>(handle);
    renderOp->useIndexes = use_indexes;
}

//IndexData *indexData
IndexDataHandle renderoperation_get_index_data(RenderOperationHandle handle)
{
    Ogre::RenderOperation* renderOp = static_cast<Ogre::RenderOperation*>(handle);
    return static_cast<IndexDataHandle>(renderOp->indexData);
}

void renderoperation_set_index_data(RenderOperationHandle handle, IndexDataHandle index_data)
{
    Ogre::RenderOperation* renderOp = static_cast<Ogre::RenderOperation*>(handle);
    renderOp->indexData = static_cast<Ogre::IndexData*>(index_data);
}

//TODO: const Renderable* srcRenderable

//size_t numberOfInstances
size_t renderoperation_get_number_of_instances(RenderOperationHandle handle)
{
    Ogre::RenderOperation* renderOp = static_cast<Ogre::RenderOperation*>(handle);
    return renderOp->numberOfInstances;
}

void renderoperation_set_number_of_instances(RenderOperationHandle handle, size_t num)
{
    Ogre::RenderOperation* renderOp = static_cast<Ogre::RenderOperation*>(handle);
    renderOp->numberOfInstances = num;
}

//bool useGlobalInstancingVertexBufferIsAvailable
int renderoperation_get_use_global_instancing_vertex_buffer_is_available(RenderOperationHandle handle)
{
    Ogre::RenderOperation* renderOp = static_cast<Ogre::RenderOperation*>(handle);
    return renderOp->useGlobalInstancingVertexBufferIsAvailable;
}

void renderoperation_set_use_global_instancing_vertex_buffer_is_available(RenderOperationHandle handle, int use)
{
    Ogre::RenderOperation* renderOp = static_cast<Ogre::RenderOperation*>(handle);
    renderOp->useGlobalInstancingVertexBufferIsAvailable = use;
}


