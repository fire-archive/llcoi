/******************************************************************************
 * meshmanager_bind.cpp - bindings for Ogre::MeshManager
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

