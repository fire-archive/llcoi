/******************************************************************************
 * materialmanager_bind.h - bindings for Ogre::MaterialManager
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

typedef void* MaterialManagerHandle;

#include "ogre_interface.h"

//static String DEFAULT_SCHEME_NAME
DLL const char* materialmanager_get_default_scheme_name();
//MaterialManager()
DLL MaterialManagerHandle create_materialmanager();
//~MaterialManager()
DLL void destroy_materialmanager(MaterialManagerHandle handle);
//void initialise()
DLL void materialmanager_initialise(MaterialManagerHandle handle);
//TODO: void parseScript(DataStreamPtr& stream, const String& groupName)
//void setDefaultTextureFiltering(TextureFilterOptions fo)
DLL void materialmanager_set_default_texture_filtering(MaterialManagerHandle handle, texture_filter_options fo);
//void setDefaultTextureFiltering(FilterType ftype, FilterOptions opts)
DLL void materialmanager_set_default_texture_filtering_with_type(MaterialManagerHandle handle, filter_type ftype, filter_options opts);
//void setDefaultTextureFiltering(FilterOptions minFilter, FilterOptions magFilter, FilterOptions mipFilter)
DLL void materialmanager_set_default_texture_filtering_min(MaterialManagerHandle handle, filter_options min_filter, filter_options mag_filter, filter_options mip_filter);
//FilterOptions getDefaultTextureFiltering(FilterType ftype) const
DLL filter_options materialmanager_get_default_texture_filtering(const MaterialManagerHandle handle, filter_type ftype);
//void setDefaultAnisotropy(unsigned int maxAniso)
DLL void materialmanager_set_default_anisotropy(MaterialManagerHandle handle, unsigned int max_aniso);
//unsigned int getDefaultAnisotropy() const
DLL unsigned int materialmanager_get_default_anisotropy(const MaterialManagerHandle handle);
//MaterialPtr getDefaultSettings() const
DLL MaterialPtrHandle materialmanager_get_default_settings(const MaterialManagerHandle handle);
//unsigned short _getSchemeIndex(const String& name)
DLL unsigned short materialmanager__get_scheme_index(MaterialManagerHandle handle, const char* name);
//const String& _getSchemeName(unsigned short index)
DLL const char* materialmanager__get_scheme_name(MaterialManagerHandle handle, unsigned short index);
//unsigned short _getActiveSchemeIndex() const
DLL unsigned short materialmanager__get_active_scheme_index(const MaterialManagerHandle handle);
//const String& getActiveScheme() const
DLL const char* materialmanager_get_active_scheme(const MaterialManagerHandle handle);
//void setActiveScheme(const String& schemeName)
DLL void materialmanager_set_active_scheme(MaterialManagerHandle handle, const char* scheme_name);
//TODO: void addListener(Listener* l, const Ogre::String& schemeName = StringUtil::BLANK)
//TODO: void removeListener(Listener* l, const Ogre::String& schemeName = StringUtil::BLANK)
//TODO: Technique* _arbitrateMissingTechniqueForActiveScheme(    Material* mat, unsigned short lodIndex, const Renderable* rend)
//static MaterialManager& getSingleton()
DLL MaterialManagerHandle materialmanager_get_singleton();
//static MaterialManager* getSingletonPtr()
DLL MaterialManagerHandle materialmanager_get_singleton_ptr();
