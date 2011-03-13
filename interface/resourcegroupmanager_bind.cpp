/******************************************************************************
 * resourcegroupmanager_bind.cpp - bindings for Ogre::ResourceGroupManager
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
#include <OgreResourceManager.h>
#include <OgreResourceGroupManager.h>
#include <OgreResource.h>


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

// Ogre::ResourceGroupManager::addResourceLocation(std::string const&, std::string const&, std::string const&, bool)
void add_resource_location(const char* location, const char* type, const char* group)
{
    Ogre::ResourceGroupManager::getSingleton().addResourceLocation(location, type, group);
}

// Ogre::ResourceGroupManager::initialiseAllResourceGroups()
void initialise_all_resourcegroups()
{
    Ogre::ResourceGroupManager::getSingleton().initialiseAllResourceGroups();
}


/*
Ogre::ResourceGroupManager::DEFAULT_RESOURCE_GROUP_NAME
Ogre::ResourceGroupManager::INTERNAL_RESOURCE_GROUP_NAME
Ogre::ResourceGroupManager::AUTODETECT_RESOURCE_GROUP_NAME
Ogre::ResourceGroupManager::RESOURCE_SYSTEM_NUM_REFERENCE_COUNTS
Ogre::ResourceGroupManager::ResourceDeclaration
Ogre::ResourceGroupManager::ResourceLocation
Ogre::ResourceGroupManager::ResourceGroupManager()
Ogre::ResourceGroupManager::~ResourceGroupManager()
Ogre::ResourceGroupManager::createResourceGroup(std::string const&, bool)
Ogre::ResourceGroupManager::initialiseResourceGroup(std::string const&)
Ogre::ResourceGroupManager::prepareResourceGroup(std::string const&, bool, bool)
Ogre::ResourceGroupManager::loadResourceGroup(std::string const&, bool, bool)
Ogre::ResourceGroupManager::unloadResourceGroup(std::string const&, bool)
Ogre::ResourceGroupManager::unloadUnreferencedResourcesInGroup(std::string const&, bool)
Ogre::ResourceGroupManager::clearResourceGroup(std::string const&)
Ogre::ResourceGroupManager::destroyResourceGroup(std::string const&)
Ogre::ResourceGroupManager::isResourceGroupInitialised(std::string const&)
Ogre::ResourceGroupManager::isResourceGroupLoaded(std::string const&)
Ogre::ResourceGroupManager::resourceGroupExists(std::string const&)
Ogre::ResourceGroupManager::removeResourceLocation(std::string const&, std::string const&)
Ogre::ResourceGroupManager::resourceLocationExists(std::string const&, std::string const&)
Ogre::ResourceGroupManager::declareResource(std::string const&, std::string const&, std::string const&, std::map<std::string, std::string, std::less<std::string>, Ogre::STLAllocator<std::pair<std::string const, std::string>, Ogre::CategorisedAllocPolicy<(Ogre::MemoryCategory)0> > > const&)
Ogre::ResourceGroupManager::declareResource(std::string const&, std::string const&, std::string const&, Ogre::ManualResourceLoader*, std::map<std::string, std::string, std::less<std::string>, Ogre::STLAllocator<std::pair<std::string const, std::string>, Ogre::CategorisedAllocPolicy<(Ogre::MemoryCategory)0> > > const&)
Ogre::ResourceGroupManager::undeclareResource(std::string const&, std::string const&)
Ogre::ResourceGroupManager::openResource(std::string const&, std::string const&, bool, Ogre::Resource*)
Ogre::ResourceGroupManager::openResources(std::string const&, std::string const&)
Ogre::ResourceGroupManager::listResourceNames(std::string const&, bool)
Ogre::ResourceGroupManager::listResourceFileInfo(std::string const&, bool)
Ogre::ResourceGroupManager::findResourceNames(std::string const&, std::string const&, bool)
Ogre::ResourceGroupManager::resourceExists(std::string const&, std::string const&)
Ogre::ResourceGroupManager::resourceExists(Ogre::ResourceGroupManager::ResourceGroup*, std::string const&)
Ogre::ResourceGroupManager::resourceExistsInAnyGroup(std::string const&)
Ogre::ResourceGroupManager::findGroupContainingResource(std::string const&)
Ogre::ResourceGroupManager::findResourceFileInfo(std::string const&, std::string const&, bool)
Ogre::ResourceGroupManager::resourceModifiedTime(std::string const&, std::string const&)
Ogre::ResourceGroupManager::listResourceLocations(std::string const&)
Ogre::ResourceGroupManager::findResourceLocation(std::string const&, std::string const&)
Ogre::ResourceGroupManager::resourceModifiedTime(Ogre::ResourceGroupManager::ResourceGroup*, std::string const&)
Ogre::ResourceGroupManager::createResource(std::string const&, std::string const&, bool, std::string const&)
Ogre::ResourceGroupManager::deleteResource(std::string const&, std::string const&, std::string const&)
Ogre::ResourceGroupManager::deleteMatchingResources(std::string const&, std::string const&, std::string const&)
Ogre::ResourceGroupManager::addResourceGroupListener(Ogre::ResourceGroupListener*)
Ogre::ResourceGroupManager::removeResourceGroupListener(Ogre::ResourceGroupListener*)
Ogre::ResourceGroupManager::setWorldResourceGroupName(std::string const&)
Ogre::ResourceGroupManager::getWorldResourceGroupName() const
Ogre::ResourceGroupManager::linkWorldGeometryToResourceGroup(std::string const&, std::string const&, Ogre::SceneManager*)
Ogre::ResourceGroupManager::unlinkWorldGeometryFromResourceGroup(std::string const&)
Ogre::ResourceGroupManager::isResourceGroupInGlobalPool(std::string const&)
Ogre::ResourceGroupManager::shutdownAll()
Ogre::ResourceGroupManager::_registerResourceManager(std::string const&, Ogre::ResourceManager*)
Ogre::ResourceGroupManager::_unregisterResourceManager(std::string const&)
Ogre::ResourceGroupManager::getResourceManagerIterator()
Ogre::ResourceGroupManager::_registerScriptLoader(Ogre::ScriptLoader*)
Ogre::ResourceGroupManager::_unregisterScriptLoader(Ogre::ScriptLoader*)
Ogre::ResourceGroupManager::_findScriptLoader(std::string const&)
Ogre::ResourceGroupManager::_getResourceManager(std::string const&)
Ogre::ResourceGroupManager::_notifyResourceCreated(Ogre::SharedPtr<Ogre::Resource>&)
Ogre::ResourceGroupManager::_notifyResourceRemoved(Ogre::SharedPtr<Ogre::Resource>&)
Ogre::ResourceGroupManager::_notifyResourceGroupChanged(std::string const&, Ogre::Resource*)
Ogre::ResourceGroupManager::_notifyAllResourcesRemoved(Ogre::ResourceManager*)
Ogre::ResourceGroupManager::_notifyWorldGeometryStageStarted(std::string const&)
Ogre::ResourceGroupManager::_notifyWorldGeometryStageEnded()
Ogre::ResourceGroupManager::getResourceGroups()
Ogre::ResourceGroupManager::getResourceDeclarationList(std::string const&)
Ogre::ResourceGroupManager::getResourceLocationList(std::string const&)
Ogre::ResourceGroupManager::setLoadingListener(Ogre::ResourceLoadingListener*)
Ogre::ResourceGroupManager::getLoadingListener()
Ogre::ResourceGroupManager::getSingleton()
Ogre::ResourceGroupManager::getSingletonPtr()
Ogre::ResourceGroupManager::ResourceDeclaration::~ResourceDeclaration()
Ogre::ResourceGroupManager::ResourceDeclaration::operator=(Ogre::ResourceGroupManager::ResourceDeclaration const&)
Ogre::ResourceGroupManager::ResourceDeclaration::ResourceDeclaration(Ogre::ResourceGroupManager::ResourceDeclaration const&)
Ogre::ResourceGroupManager::ResourceDeclaration::ResourceDeclaration()
Ogre::ResourceGroupManager::ResourceLocation::~ResourceLocation()
Ogre::ResourceGroupManager::ResourceLocation::operator=(Ogre::ResourceGroupManager::ResourceLocation const&)
Ogre::ResourceGroupManager::ResourceLocation::ResourceLocation(Ogre::ResourceGroupManager::ResourceLocation const&)
Ogre::ResourceGroupManager::ResourceLocation::ResourceLocation()
Ogre::ResourceGroupManager::ResourceGroup::~ResourceGroup()
Ogre::ResourceGroupManager::ResourceGroup::ResourceGroup()
Ogre::ResourceGroupManager::ResourceGroup::addToIndex(std::string const&, Ogre::Archive*)
Ogre::ResourceGroupManager::ResourceGroup::removeFromIndex(std::string const&, Ogre::Archive*)
Ogre::ResourceGroupManager::ResourceGroup::removeFromIndex(Ogre::Archive*)
*/
