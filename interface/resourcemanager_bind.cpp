/******************************************************************************
 * resourcemanager_bind.cpp - bindings for Ogre::ResourceManager
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
#include <OgreConfigFile.h>

void setup_resources(const char* resources_cfg)
{
    // set up resources
    // Load resource paths from config file
    Ogre::ConfigFile cf;
    cf.load(resources_cfg);
 
    // Go through all sections & settings in the file
    Ogre::ConfigFile::SectionIterator seci = cf.getSectionIterator();
 
    Ogre::String secName, typeName, archName;
    while (seci.hasMoreElements())
    {
        secName = seci.peekNextKey();
        Ogre::ConfigFile::SettingsMultiMap *settings = seci.getNext();
        Ogre::ConfigFile::SettingsMultiMap::iterator i;
        for (i = settings->begin(); i != settings->end(); ++i)
        {
            typeName = i->first;
            archName = i->second;
            Ogre::ResourceGroupManager::getSingleton().addResourceLocation(
                archName, typeName, secName);
        }
    }
}

void add_resource_location(const char* location, const char* type, const char* group)
{
    Ogre::ResourceGroupManager::getSingleton().addResourceLocation(location, type, group);
}

void initialise_all_resourcegroups()
{
    Ogre::ResourceGroupManager::getSingleton().initialiseAllResourceGroups();
}

/*
Ogre::ResourceManager::ResourcePool
Ogre::ResourceManager::ResourceManager()
Ogre::ResourceManager::~ResourceManager()
Ogre::ResourceManager::create(std::string const&, std::string const&, bool, Ogre::ManualResourceLoader*, std::map<std::string, std::string, std::less<std::string>, Ogre::STLAllocator<std::pair<std::string const, std::string>, Ogre::CategorisedAllocPolicy<(Ogre::MemoryCategory)0> > > const*)
Ogre::ResourceManager::createOrRetrieve(std::string const&, std::string const&, bool, Ogre::ManualResourceLoader*, std::map<std::string, std::string, std::less<std::string>, Ogre::STLAllocator<std::pair<std::string const, std::string>, Ogre::CategorisedAllocPolicy<(Ogre::MemoryCategory)0> > > const*)
Ogre::ResourceManager::setMemoryBudget(unsigned int)
Ogre::ResourceManager::getMemoryBudget() const
Ogre::ResourceManager::getMemoryUsage() const
Ogre::ResourceManager::unload(std::string const&)
Ogre::ResourceManager::unload(unsigned long long)
Ogre::ResourceManager::unloadAll(bool)
Ogre::ResourceManager::reloadAll(bool)
Ogre::ResourceManager::unloadUnreferencedResources(bool)
Ogre::ResourceManager::reloadUnreferencedResources(bool)
Ogre::ResourceManager::remove(Ogre::SharedPtr<Ogre::Resource>&)
Ogre::ResourceManager::remove(std::string const&)
Ogre::ResourceManager::remove(unsigned long long)
Ogre::ResourceManager::removeAll()
Ogre::ResourceManager::removeUnreferencedResources(bool)
Ogre::ResourceManager::getByName(std::string const&, std::string const&)
Ogre::ResourceManager::getByHandle(unsigned long long)
Ogre::ResourceManager::resourceExists(std::string const&)
Ogre::ResourceManager::resourceExists(unsigned long long)
Ogre::ResourceManager::_notifyResourceTouched(Ogre::Resource*)
Ogre::ResourceManager::_notifyResourceLoaded(Ogre::Resource*)
Ogre::ResourceManager::_notifyResourceUnloaded(Ogre::Resource*)
Ogre::ResourceManager::prepare(std::string const&, std::string const&, bool, Ogre::ManualResourceLoader*, std::map<std::string, std::string, std::less<std::string>, Ogre::STLAllocator<std::pair<std::string const, std::string>, Ogre::CategorisedAllocPolicy<(Ogre::MemoryCategory)0> > > const*, bool)
Ogre::ResourceManager::load(std::string const&, std::string const&, bool, Ogre::ManualResourceLoader*, std::map<std::string, std::string, std::less<std::string>, Ogre::STLAllocator<std::pair<std::string const, std::string>, Ogre::CategorisedAllocPolicy<(Ogre::MemoryCategory)0> > > const*, bool)
Ogre::ResourceManager::getScriptPatterns() const
Ogre::ResourceManager::parseScript(Ogre::SharedPtr<Ogre::DataStream>&, std::string const&)
Ogre::ResourceManager::getLoadingOrder() const
Ogre::ResourceManager::getResourceType() const
Ogre::ResourceManager::setVerbose(bool)
Ogre::ResourceManager::getVerbose()
Ogre::ResourceManager::getResourcePool(std::string const&)
Ogre::ResourceManager::destroyResourcePool(Ogre::ResourceManager::ResourcePool*)
Ogre::ResourceManager::destroyResourcePool(std::string const&)
Ogre::ResourceManager::destroyAllResourcePools()
Ogre::ResourceManager::getResourceIterator()
Ogre::ResourceManager::ResourcePool::ResourcePool(std::string const&)
Ogre::ResourceManager::ResourcePool::~ResourcePool()
Ogre::ResourceManager::ResourcePool::getName() const
Ogre::ResourceManager::ResourcePool::clear()
*/