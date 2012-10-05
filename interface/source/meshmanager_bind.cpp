/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "ogre_interface.h"
#include "meshmanager_bind.h"
#include "binding_utils.h" // llcoi_hbu_to_ogre_hbu

#include <OgreRoot.h>
#include <OgrePlane.h>
#include <OgreMeshManager.h>

MeshHandle meshmanager_create_plane(const char* name, const char* group_name,
                                    PlaneHandle plane_handle, float width,
                                    float height, int xsegments, int ysegments,
                                    int normals, unsigned short num_tex_coord_sets,
                                    float utile, float vtile, coiVector3* up_vector,
                                    hardware_buffer_usage vertex_buffer_usage,
                                    hardware_buffer_usage index_buffer_usage,
                                    int vertex_shadow_buffer, int index_shadow_buffer)
{
    Ogre::HardwareBuffer::Usage vbu = llcoi_hbu_to_ogre_hbu(vertex_buffer_usage);
    Ogre::HardwareBuffer::Usage ibu = llcoi_hbu_to_ogre_hbu(index_buffer_usage);

    Ogre::Plane *plane = reinterpret_cast<Ogre::Plane*>(plane_handle);
    Ogre::Vector3 upVector(up_vector->x, up_vector->y, up_vector->z);
    Ogre::MeshPtr ptr = Ogre::MeshManager::getSingletonPtr()->createPlane(Ogre::String(name),
                                                                          Ogre::String(group_name),
                                                                          *plane,
                                                                          width,
                                                                          height,
                                                                          xsegments,
                                                                          ysegments,
                                                                          normals, num_tex_coord_sets, utile, vtile, upVector, vbu, ibu, vertex_shadow_buffer, index_shadow_buffer);

    // Note: it'd be a very bad idea to delete this puppy.
    return reinterpret_cast<MeshHandle>(ptr.get());
}

