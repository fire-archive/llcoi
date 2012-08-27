/******************************************************************************
 * scenemanager_bind.h - bindings for Ogre::SceneManager
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
#ifndef SCENEMANAGER_BIND_H
#define SCENEMANAGER_BIND_H

#include "ogre_interface.h"

#define SceneManagerHandle void*
#define SceneNodeHandle void*
#define CameraHandle void*
#define EntityHandle void*
#define ViewportHandle void*
#define LightHandle void*
#define SceneQueryHandle void*
#define RaySceneQueryHandle void*
#define RayHandle void*
#define ManualObjectHandle void*

// Ogre::SceneManager calls
DLL EntityHandle scenemanager_create_entity(SceneManagerHandle handle, const char* name, const char* mesh_name, const char* group_name);
//createManualObject(std::string const&)
DLL ManualObjectHandle scenemanager_create_manual_object(SceneManagerHandle handle, const char* name);
//createManualObject()
DLL ManualObjectHandle scenemanager_create_manual_object_unnamed(SceneManagerHandle handle);
//getManualObject(std::string const&) const
DLL ManualObjectHandle scenemanager_get_manual_object(const SceneManagerHandle handle, const char* name);
//hasManualObject(std::string const&) const
DLL int scenemanager_has_manual_object(const SceneManagerHandle handle, const char* name);
//destroyManualObject(Ogre::ManualObject*)
DLL void scenemanager_destroy_manual_object(SceneManagerHandle handle, ManualObjectHandle obj);
//destroyManualObject(std::string const&)
DLL void scenemanager_destroy_manual_object_by_name(SceneManagerHandle handle, const char* name);
//destroyAllManualObjects()
DLL void scenemanager_destroy_all_manual_objects(SceneManagerHandle handle);

DLL SceneNodeHandle scenemanager_get_root_scene_node(SceneManagerHandle handle);
// Does use OgreManager.
DLL SceneManagerHandle create_scene_manager(const char* type_name, const char* instance_name);
DLL SceneManagerHandle get_scene_manager_by_name(const char* scene_manager_instance_name);
DLL SceneManagerHandle get_scene_manager();

DLL LightHandle scenemanager_create_light(SceneManagerHandle handle, const char* name);

DLL void scenemanager_set_sky_box(SceneManagerHandle handle, int enable, const char* material_name, float distance,
                                  int draw_first, const coiQuaternion* orientation,
                                  const char* group_name);

DLL void scenemanager_set_sky_dome(SceneManagerHandle handle, int enable, const char* material_name, float curvature,
                               float tiling, float distance, int draw_first, const coiQuaternion* orientation,
                               int xsegments, int ysegments, int ysegments_keep, const char* group_name);

// SceneManager functions

DLL void set_ambient_light_rgba(const float r, const float g, const float b, const float a);

DLL void set_ambient_light_rgb(const float r, const float g, const float b);

DLL void scene_manager_log_name();

DLL const char* scenemanager_get_name(SceneManagerHandle handle);
//void SceneManager::destroyQuery(Ogre::SceneQuery* query);
DLL void scenemanager_destroy_scenequery(SceneManagerHandle handle, SceneQueryHandle query);
//RaySceneQuery* SceneManager::createRayQuery(const Ray& ray, unsigned long mask = 0xFFFFFFFF)
DLL RaySceneQueryHandle scenemanager_create_rayquery(SceneQueryHandle handle, RayHandle ray_handle, unsigned long mask);


#endif
