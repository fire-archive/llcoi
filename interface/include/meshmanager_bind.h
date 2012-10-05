/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#pragma once

#include "ogre_interface.h"

DLL MeshHandle meshmanager_create_plane(const char* name, const char* group_name,
                                        PlaneHandle plane, float width,
                                        float height, int xsegments, int ysegments,
                                        int normals, unsigned short num_tex_coord_sets,
                                        float utile, float vtile, coiVector3* up_vector,
                                        hardware_buffer_usage vertex_buffer_usage,
                                        hardware_buffer_usage index_buffer_usage,
                                        int vertex_shadow_buffer, int index_shadow_buffer);
