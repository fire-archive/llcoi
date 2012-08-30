/******************************************************************************
 * resourcegroupmanager_bind.h - bindings for Ogre::ResourceGroupManager
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
#ifndef RESOURCEGROUPMANAGER_BIND_H
#define RESOURCEGROUPMANAGER_BIND_H

#include "ogre_interface.h"
#define ResourceGroupManagerHandle void*

// Resource management
DLL ResourceGroupManagerHandle create_resourcegroupmanager();
DLL void destroy_resourcegroupmanager(ResourceGroupManagerHandle handle);
DLL void resourcegroupmanager_setup_resources(const char* resources_cfg);
DLL void resourcegroupmanager_add_resource_location(ResourceGroupManagerHandle handle, const char* location, const char* type, const char* group);
DLL const char * resourcegroupmanager_DEFAULT_RESOURCE_GROUP_NAME();
DLL const char * resourcegroupmanager_INTERNAL_RESOURCE_GROUP_NAME();
DLL const char * resourcegroupmanager_AUTODETECT_RESOURCE_GROUP_NAME();
DLL size_t resourcegroupmanager_RESOURCE_SYSTEM_NUM_REFERENCE_COUNTS();
DLL void resourcegroupmanager_initialise_all_resourcegroups(ResourceGroupManagerHandle handle);
DLL ResourceGroupManagerHandle resourcegroupmanager_get_singleton();
DLL ResourceGroupManagerHandle resourcegroupmanager_get_singleton_ptr();

#endif
