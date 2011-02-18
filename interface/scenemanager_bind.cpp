/******************************************************************************
 * scenemanager_bind.cpp - bindings for Ogre::SceneManager
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

#include <OgreRoot.h>
#include <OgreRenderWindow.h>
#include <OgreCamera.h>

extern const char* active_scene_manager_name;

void set_ambient_light_rgba(const float r, const float g, const float b, const float a)
{
    Ogre::Root::getSingletonPtr()->getSceneManager(active_scene_manager_name)->setAmbientLight(Ogre::ColourValue(r, g, b, a));
}

void set_ambient_light_rgb(const float r, const float g, const float b)
{
    Ogre::Root::getSingletonPtr()->getSceneManager(active_scene_manager_name)->setAmbientLight(Ogre::ColourValue(r, g, b));
}

SceneManagerHandle create_scene_manager(const char* type_name, const char* instance_name)
{
/*    va_list arg_list;
    const char* instance_name = NULL;
    va_start(arg_list, type_name);
    instance_name = va_arg(arg_list, const char*);
    va_end(arg_list);

    if(instance_name == NULL) instance_name = "default";*/
    
    active_scene_manager_name = instance_name;
    
    Ogre::SceneManager* sm = Ogre::Root::getSingletonPtr()->createSceneManager(Ogre::String(type_name), Ogre::String(instance_name));
    return reinterpret_cast<SceneManagerHandle>(sm);
}

SceneManagerHandle get_scene_manager()
{
    Ogre::SceneManager* sm = Ogre::Root::getSingletonPtr()->getSceneManager(active_scene_manager_name);
    return reinterpret_cast<SceneManagerHandle>(sm);
}

SceneManagerHandle get_scene_manager_by_name(const char* scene_manager_instance_name)
{
    Ogre::SceneManager* sm = Ogre::Root::getSingletonPtr()->getSceneManager(scene_manager_instance_name);
    return reinterpret_cast<SceneManagerHandle>(sm);
}
