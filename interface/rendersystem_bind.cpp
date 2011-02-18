/******************************************************************************
 * rendersystem_bind.cpp - bindings for Ogre::RenderSystem
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
#include <OgreRenderSystem.h>

void add_render_system(RenderSystemHandle render_system)
{
    Ogre::RenderSystem* rs = reinterpret_cast<Ogre::RenderSystem*>(render_system);
    Ogre::Root::getSingletonPtr()->addRenderSystem(rs);
}

void set_render_system(RenderSystemHandle render_system)
{
    Ogre::RenderSystem* rs = reinterpret_cast<Ogre::RenderSystem*>(render_system);
    Ogre::Root::getSingletonPtr()->setRenderSystem(rs);
}

RenderSystemHandle get_render_system_by_name(const char* render_system_name)
{
    Ogre::RenderSystem* rs = Ogre::Root::getSingletonPtr()->getRenderSystemByName(render_system_name);
    return reinterpret_cast<RenderSystemHandle>(rs);
}

RenderSystemHandle get_render_system()
{
    return reinterpret_cast<RenderSystemHandle>(Ogre::Root::getSingletonPtr()->getRenderSystem());
}

void render_system_set_config_option(RenderSystemHandle render_system_handle, const char* option, const char* value)
{
    Ogre::RenderSystem* rs = reinterpret_cast<Ogre::RenderSystem*>(render_system_handle);
    rs->setConfigOption(option, value);
}
