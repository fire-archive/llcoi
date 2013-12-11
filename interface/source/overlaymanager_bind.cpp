/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "overlaymanager_bind.h"
#include "binding_utils.h"
#include <Overlay/OgreOverlayManager.h>
#include <Overlay/OgreOverlayElement.h>

/*#define protected public
#include <iostream>
#include <boost/format.hpp> */

OverlayManagerHandle create_overlaymanager()
{
    Ogre::OverlayManager* ovm = new Ogre::OverlayManager;
    return static_cast<OverlayManagerHandle>(ovm);
}

void destroy_overlaymanager(OverlayManagerHandle handle)
{
    Ogre::OverlayManager* ovm = static_cast<Ogre::OverlayManager*>(handle);
    delete ovm;
}

coiReal overlaymanager_get_loading_order(OverlayManagerHandle handle)
{
    Ogre::OverlayManager* ovm = static_cast<Ogre::OverlayManager*>(handle);
    return ovm->getLoadingOrder();
}

OverlayHandle overlaymanager_create(OverlayManagerHandle handle, const char* name)
{
    Ogre::OverlayManager* ovm = static_cast<Ogre::OverlayManager*>(handle);
    return static_cast<OverlayHandle>(ovm->create(Ogre::String(name)));
}

OverlayHandle overlaymanager_get_by_name(OverlayManagerHandle handle, const char* name)
{
    Ogre::OverlayManager* ovm = static_cast<Ogre::OverlayManager*>(handle);
    return static_cast<OverlayHandle>(ovm->getByName(Ogre::String(name)));
}

void overlaymanager_destroy_by_name(OverlayManagerHandle handle, const char* name)
{
    Ogre::OverlayManager* ovm = static_cast<Ogre::OverlayManager*>(handle);
    ovm->destroy(Ogre::String(name));
}

void overlaymanager_destroy(OverlayManagerHandle handle, OverlayHandle overlay_handle)
{
    Ogre::OverlayManager* ovm = static_cast<Ogre::OverlayManager*>(handle);
    Ogre::Overlay* overlay = static_cast<Ogre::Overlay*>(overlay_handle);
    ovm->destroy(overlay);
}

void overlaymanager_destroy_all(OverlayManagerHandle handle)
{
    Ogre::OverlayManager* ovm = static_cast<Ogre::OverlayManager*>(handle);
    ovm->destroyAll();
}

/*
void overlaymanager_list_overlays(OverlayManagerHandle handle)
{
    using std::cerr;
    using boost::format;

    Ogre::OverlayManager* ovm = static_cast<Ogre::OverlayManager*>(handle);

    Ogre::OverlayManager::OverlayMapIterator map_iter = ovm->getOverlayIterator();
    Ogre::OverlayManager::OverlayMapIterator::iterator end = map_iter.end();

    for(Ogre::OverlayManager::OverlayMapIterator::iterator start = map_iter.begin(); start != end; ++start)
    {
        cerr << map_iter.current()->first << "\n";
    }

    Ogre::OverlayManager::ElementMap & elements = ovm->getElementMap(false);
    for(Ogre::OverlayManager::ElementMap::iterator e_iter = elements.begin(); e_iter != elements.end(); ++e_iter)
    {
        cerr << e_iter->first << "\n";
    }
}
*/

int overlaymanager_has_viewport_changed(OverlayManagerHandle handle)
{
    Ogre::OverlayManager* ovm = static_cast<Ogre::OverlayManager*>(handle);
    return ovm->hasViewportChanged();
}

int overlaymanager_get_viewport_height(OverlayManagerHandle handle)
{
    Ogre::OverlayManager* ovm = static_cast<Ogre::OverlayManager*>(handle);
    return ovm->getViewportHeight();
}

int overlaymanager_get_viewport_width(OverlayManagerHandle handle)
{
    Ogre::OverlayManager* ovm = static_cast<Ogre::OverlayManager*>(handle);
    return ovm->getViewportWidth();
}

coiReal overlaymanager_get_viewport_aspect_ratio(OverlayManagerHandle handle)
{
    Ogre::OverlayManager* ovm = static_cast<Ogre::OverlayManager*>(handle);
    return ovm->getViewportAspectRatio();
}

orientation_mode overlaymanager_get_viewport_orientation_mode(OverlayManagerHandle handle)
{
    Ogre::OverlayManager* ovm = static_cast<Ogre::OverlayManager*>(handle);
    Ogre::OrientationMode mode = ovm->getViewportOrientationMode();
    return ogre_orientation_mode_to_llcoi_orientation_mode(mode);
}

int overlaymanager_has_overlay_element(OverlayManagerHandle handle, const char* name, int is_template)
{
    Ogre::OverlayManager* ovm = static_cast<Ogre::OverlayManager*>(handle);
    return ovm->hasOverlayElement(Ogre::String(name), is_template);
}

OverlayElementHandle overlaymanager_create_overlayelement(OverlayManagerHandle handle, const char* type_name, const char* instance_name, int is_template)
{
    Ogre::OverlayManager* ovm = static_cast<Ogre::OverlayManager*>(handle);

    const Ogre::String typeName(type_name);
    const Ogre::String instanceName(instance_name);
    Ogre::OverlayElement* oe = ovm->createOverlayElement(typeName, instanceName, is_template);

    return static_cast<OverlayElementHandle>(oe);
}

OverlayElementHandle overlaymanager_get_overlayelement(OverlayManagerHandle handle, const char* name, int is_template)
{
    Ogre::OverlayManager* ovm = static_cast<Ogre::OverlayManager*>(handle);
    Ogre::OverlayElement* oe = ovm->getOverlayElement(Ogre::String(name), is_template);

    return static_cast<OverlayElementHandle>(oe);
}

void overlaymanager_destroy_overlay_element(OverlayManagerHandle handle, const char* name, int is_template)
{
    Ogre::OverlayManager* ovm = static_cast<Ogre::OverlayManager*>(handle);
    ovm->destroyOverlayElement(Ogre::String(name), is_template);
}

void overlaymanager_destroy_overlay_element_instance(OverlayManagerHandle handle, OverlayElementHandle instance, int is_template)
{
    Ogre::OverlayManager* ovm = static_cast<Ogre::OverlayManager*>(handle);
    Ogre::OverlayElement* oe  = static_cast<Ogre::OverlayElement*>(instance);
    ovm->destroyOverlayElement(oe, is_template);
}

void overlaymanager_destroy_all_overlay_elements(OverlayManagerHandle handle)
{
    Ogre::OverlayManager* ovm = static_cast<Ogre::OverlayManager*>(handle);
    ovm->destroyAllOverlayElements();
}

OverlayElementHandle overlaymanager_create_overlayelement_from_template(OverlayManagerHandle handle, const char* template_name, const char* type_name, const char* instance_name, int is_template)
{
    Ogre::OverlayManager* ovm = static_cast<Ogre::OverlayManager*>(handle);

    const Ogre::String templateName(template_name);
    const Ogre::String typeName(type_name);
    const Ogre::String instanceName(instance_name);

    Ogre::OverlayElement* oe = ovm->createOverlayElementFromTemplate(templateName, typeName, instanceName, is_template);

    return static_cast<OverlayElementHandle>(oe);
}

OverlayElementHandle overlaymanager_clone_overlayelement_from_template(OverlayManagerHandle handle, const char* template_name, const char* instance_name)
{
    Ogre::OverlayManager* ovm = static_cast<Ogre::OverlayManager*>(handle);

    const Ogre::String templateName(template_name);
    const Ogre::String instanceName(instance_name);

    Ogre::OverlayElement* oe = ovm->cloneOverlayElementFromTemplate(templateName, instanceName);

    return static_cast<OverlayElementHandle>(oe);
}

OverlayElementHandle overlaymanager_create_overlayelement_from_factory(OverlayManagerHandle handle, const char* type_name, const char* instance_name)
{
    Ogre::OverlayManager* ovm = static_cast<Ogre::OverlayManager*>(handle);

    const Ogre::String typeName(type_name);
    const Ogre::String instanceName(instance_name);

    Ogre::OverlayElement* oe = ovm->cloneOverlayElementFromTemplate(typeName, instanceName);

    return static_cast<OverlayElementHandle>(oe);
}

int overlaymanager_is_template(OverlayManagerHandle handle, const char* name)
{
    Ogre::OverlayManager* ovm = static_cast<Ogre::OverlayManager*>(handle);
    return ovm->isTemplate(Ogre::String(name));
}

OverlayManagerHandle overlaymanager_get_singleton_ptr()
{
    Ogre::OverlayManager* ovm = Ogre::OverlayManager::getSingletonPtr();
    return static_cast<OverlayManagerHandle>(
        ovm
    );
}
