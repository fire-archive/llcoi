/******************************************************************************
 * scenequery_bind.cpp -  bindings for Ogre::SceneQuery
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

#include "binding_utils.h" // llcoi_wft_to_ogre_wft
#include "scenequery_bind.h"
#include <OgreSceneQuery.h>
#include <OgreSceneManager.h>

// SceneQuery::setQueryMask(uint32 mask)
void scenequery_set_query_mask(SceneQueryHandle handle, uint32 mask)
{
    Ogre::SceneQuery* query = reinterpret_cast<Ogre::SceneQuery*>(handle);
    query->setQueryMask(mask);
}

//uint32 SceneQuery::getQueryMask(void) const
uint32 scenequery_get_query_mask(SceneQueryHandle handle)
{
    Ogre::SceneQuery* query = reinterpret_cast<Ogre::SceneQuery*>(handle);
    return query->getQueryMask();
}

//void setWorldFragmentType(enum WorldFragmentType wft);
void scenequery_set_world_fragment_type(SceneQueryHandle handle, world_fragment_type wft)
{
    Ogre::SceneQuery* query = reinterpret_cast<Ogre::SceneQuery*>(handle);
    Ogre::SceneQuery::WorldFragmentType WFT = llcoi_wft_to_ogre_wft(wft);
    query->setWorldFragmentType(WFT);
}

//WorldFragmentType SceneQuery::getWorldFragmentType(void) const;
world_fragment_type scenequery_get_world_fragment_type(SceneQueryHandle handle)
{
    Ogre::SceneQuery* query = reinterpret_cast<Ogre::SceneQuery*>(handle);
    Ogre::SceneQuery::WorldFragmentType wft = query->getWorldFragmentType();
    return ogre_wft_to_llcoi_wft(wft);
}
