/******************************************************************************
 * main.cpp - C++ code - main entries
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
#include <ogre_interface.h>

#include <OgreRoot.h>
#include <OgreEntity.h>
#include "ogre_manager.h"

void log_message(const char* message)
{
	Ogre::LogManager::getSingletonPtr()->logMessage(message);
}

SceneNodeHandle create_child_scenenode(const char* node_name)
{
    Ogre::SceneNode* scenenode = Ogre::Root::getSingletonPtr()->getSceneManager(OgreManager::getSingletonPtr()->get_active_scene_manager_name())->getRootSceneNode()->createChildSceneNode(node_name);
    return reinterpret_cast<SceneNodeHandle>(scenenode);
}

void attach_entity_to_scenenode(EntityHandle entity_handle, SceneNodeHandle scenenode_handle)
{
    Ogre::Entity* object = reinterpret_cast<Ogre::Entity*>(entity_handle);
    Ogre::SceneNode* node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    node->attachObject(object);
}

void light_set_position(LightHandle light_handle, const float x, const float y, const float z)
{
    Ogre::Light* light = reinterpret_cast<Ogre::Light*>(light_handle);
    light->setPosition(Ogre::Vector3(x, y, z));
}

void set_default_num_mipmaps(int number)
{
    Ogre::TextureManager::getSingleton().setDefaultNumMipmaps(number);
}

#ifdef PLATFORM_WIN
BOOL APIENTRY DllMain( HANDLE /*hModule*/, DWORD /*ul_reason_for_call*/, LPVOID /*lpReserved*/ )
{
#if defined( _MSC_VER ) && defined( _DEBUG )
    //_crtBreakAlloc = 1397;
    _CrtSetDbgFlag ( _CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF );
#endif

    /*switch( ul_reason_for_call )
    {
    case DLL_PROCESS_DETACH:
        _CrtDumpMemoryLeaks();
        break;
    }*/

    return TRUE;
}
#endif
