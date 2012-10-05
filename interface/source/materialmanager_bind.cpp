/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "materialmanager_bind.h"
#include "binding_utils.h"
#include <OgreMaterialManager.h>

// For when we've wrapped Ogre::Technique...
class MaterialListener : public Ogre::MaterialManager::Listener
{
public:
    MaterialListener() {}
    virtual ~MaterialListener() {}

    Ogre::Technique* handleSchemeNotFound(unsigned short schemeIndex, const Ogre::String& schemeName, Ogre::Material* originalMaterial, unsigned short lodIndex,  const Ogre::Renderable* rend)
    {
    }
};

//static String DEFAULT_SCHEME_NAME
const char* materialmanager_get_default_scheme_name()
{
    return Ogre::MaterialManager::DEFAULT_SCHEME_NAME.c_str();
}

//MaterialManager()
MaterialManagerHandle create_materialmanager()
{
    Ogre::MaterialManager* mgr = new Ogre::MaterialManager;
    return static_cast<MaterialManagerHandle>(mgr);
}

//~MaterialManager()
void destroy_materialmanager(MaterialManagerHandle handle)
{
    Ogre::MaterialManager* mgr = static_cast<Ogre::MaterialManager*>(handle);
    delete mgr;
}

//void initialise()
void materialmanager_initialise(MaterialManagerHandle handle)
{
    Ogre::MaterialManager* mgr = static_cast<Ogre::MaterialManager*>(handle);
    mgr->initialise();
}

//TODO: void parseScript(DataStreamPtr& stream, const String& groupName)
//void setDefaultTextureFiltering(TextureFilterOptions fo)
void materialmanager_set_default_texture_filtering(MaterialManagerHandle handle, texture_filter_options fo)
{
    Ogre::MaterialManager* mgr = static_cast<Ogre::MaterialManager*>(handle); 
    mgr->setDefaultTextureFiltering(enum_converter(fo));
}

//void setDefaultTextureFiltering(FilterType ftype, FilterOptions opts)
void materialmanager_set_default_texture_filtering_with_type(MaterialManagerHandle handle, filter_type ftype, filter_options opts)
{
    Ogre::MaterialManager* mgr = static_cast<Ogre::MaterialManager*>(handle);
    mgr->setDefaultTextureFiltering(enum_converter(ftype), enum_converter(opts));
}

//void setDefaultTextureFiltering(FilterOptions minFilter, FilterOptions magFilter, FilterOptions mipFilter)
void materialmanager_set_default_texture_filtering_min(MaterialManagerHandle handle, filter_options min_filter, filter_options mag_filter, filter_options mip_filter)
{
    Ogre::MaterialManager* mgr = static_cast<Ogre::MaterialManager*>(handle);
    mgr->setDefaultTextureFiltering(enum_converter(min_filter), enum_converter(mag_filter), enum_converter(mip_filter));
}

//FilterOptions getDefaultTextureFiltering(FilterType ftype) const
filter_options materialmanager_get_default_texture_filtering(const MaterialManagerHandle handle, filter_type ftype)
{
    const Ogre::MaterialManager* mgr = static_cast<const Ogre::MaterialManager*>(handle);
    return enum_converter(
        mgr->getDefaultTextureFiltering(enum_converter(ftype))
    );
}

//void setDefaultAnisotropy(unsigned int maxAniso)
void materialmanager_set_default_anisotropy(MaterialManagerHandle handle, unsigned int max_aniso)
{
    Ogre::MaterialManager* mgr = static_cast<Ogre::MaterialManager*>(handle);
    mgr->setDefaultAnisotropy(max_aniso);
}

//unsigned int getDefaultAnisotropy() const
unsigned int materialmanager_get_default_anisotropy(const MaterialManagerHandle handle)
{
    const Ogre::MaterialManager* mgr = static_cast<const Ogre::MaterialManager*>(handle);
    return mgr->getDefaultAnisotropy();
}

//MaterialPtr getDefaultSettings() const
MaterialPtrHandle materialmanager_get_default_settings(const MaterialManagerHandle handle)
{
    const Ogre::MaterialManager* mgr = static_cast<const Ogre::MaterialManager*>(handle);
    Ogre::MaterialPtr ptr = mgr->getDefaultSettings();
    return static_cast<MaterialPtrHandle>(ptr.get());
}

//unsigned short _getSchemeIndex(const String& name)
unsigned short materialmanager__get_scheme_index(MaterialManagerHandle handle, const char* name)
{
    Ogre::MaterialManager* mgr = static_cast<Ogre::MaterialManager*>(handle);
    return mgr->_getSchemeIndex(Ogre::String(name));
}

//const String& _getSchemeName(unsigned short index)
const char* materialmanager__get_scheme_name(MaterialManagerHandle handle, unsigned short index)
{
    Ogre::MaterialManager* mgr = static_cast<Ogre::MaterialManager*>(handle);
    return mgr->_getSchemeName(index).c_str();
}

//unsigned short _getActiveSchemeIndex() const
unsigned short materialmanager__get_active_scheme_index(const MaterialManagerHandle handle)
{
    const Ogre::MaterialManager* mgr = static_cast<const Ogre::MaterialManager*>(handle);
    return mgr->_getActiveSchemeIndex();
}

//const String& getActiveScheme() const
const char* materialmanager_get_active_scheme(const MaterialManagerHandle handle)
{
    const Ogre::MaterialManager* mgr = static_cast<const Ogre::MaterialManager*>(handle);
    return mgr->getActiveScheme().c_str();
}

//void setActiveScheme(const String& schemeName)
void materialmanager_set_active_scheme(MaterialManagerHandle handle, const char* scheme_name)
{
    Ogre::MaterialManager* mgr = static_cast<Ogre::MaterialManager*>(handle);
    mgr->setActiveScheme(Ogre::String(scheme_name));
}

//TODO: void addListener(Listener* l, const Ogre::String& schemeName = StringUtil::BLANK)
//TODO: void removeListener(Listener* l, const Ogre::String& schemeName = StringUtil::BLANK)
//TODO: Technique* _arbitrateMissingTechniqueForActiveScheme(    Material* mat, unsigned short lodIndex, const Renderable* rend)
//static MaterialManager& getSingleton()
MaterialManagerHandle materialmanager_get_singleton()
{
    Ogre::MaterialManager& mgr = Ogre::MaterialManager::getSingleton();
    return static_cast<MaterialManagerHandle>(&mgr);
}

//static MaterialManager* getSingletonPtr()
MaterialManagerHandle materialmanager_get_singleton_ptr()
{
    Ogre::MaterialManager* mgr = Ogre::MaterialManager::getSingletonPtr();
    return static_cast<MaterialManagerHandle>(mgr);
}
