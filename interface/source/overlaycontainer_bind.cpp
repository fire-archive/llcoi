/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "overlaycontainer_bind.h"
#include "binding_utils.h"
#include <Overlay/OgreOverlayContainer.h>

void destroy_overlaycontainer(OverlayContainerHandle handle)
{
    Ogre::OverlayContainer* oc = static_cast<Ogre::OverlayContainer*>(handle);
    delete oc;
}

void overlaycontainer_add_child(OverlayContainerHandle handle, OverlayElementHandle child_handle)
{
    Ogre::OverlayContainer* oc = static_cast<Ogre::OverlayContainer*>(handle);
    Ogre::OverlayElement* oe   = static_cast<Ogre::OverlayElement*>(child_handle);
    oc->addChild(oe);
}

void overlaycontainer_add_child_impl(OverlayContainerHandle handle, OverlayElementHandle child_handle)
{
    Ogre::OverlayContainer* oc = static_cast<Ogre::OverlayContainer*>(handle);
    Ogre::OverlayElement* oe   = static_cast<Ogre::OverlayElement*>(child_handle);
    oc->addChildImpl(oe);
}

void overlaycontainer_add_child_container_impl(OverlayContainerHandle handle, OverlayContainerHandle child_handle)
{
    Ogre::OverlayContainer* oc = static_cast<Ogre::OverlayContainer*>(handle);
    Ogre::OverlayContainer* child = static_cast<Ogre::OverlayContainer*>(child_handle);
    oc->addChildImpl(child);
}

void overlaycontainer_remove_child(OverlayContainerHandle handle, const char* name)
{
    Ogre::OverlayContainer* oc = static_cast<Ogre::OverlayContainer*>(handle);
    oc->removeChild(Ogre::String(name));
}

OverlayElementHandle overlaycontainer_get_child(OverlayContainerHandle handle, const char* name)
{
    Ogre::OverlayContainer* oc = static_cast<Ogre::OverlayContainer*>(handle);
    Ogre::OverlayElement* oe = oc->getChild(Ogre::String(name));
    return static_cast<OverlayElementHandle>(oe);
}

void overlaycontainer_initialise(OverlayContainerHandle handle)
{
    Ogre::OverlayContainer* oc = static_cast<Ogre::OverlayContainer*>(handle);
    oc->initialise();
}

void overlaycontainer__add_child(OverlayContainerHandle handle, OverlayElementHandle elem)
{
    Ogre::OverlayContainer* oc = static_cast<Ogre::OverlayContainer*>(handle);
    Ogre::OverlayElement* oe   = static_cast<Ogre::OverlayElement*>(elem);
    oc->_addChild(oe);
}

void overlaycontainer__remove_child(OverlayContainerHandle handle, OverlayElementHandle elem)
{
    Ogre::OverlayContainer* oc = static_cast<Ogre::OverlayContainer*>(handle);
    Ogre::OverlayElement* oe   = static_cast<Ogre::OverlayElement*>(elem);
    oc->_removeChild(oe);
}

void overlaycontainer__remove_child_by_name(OverlayContainerHandle handle, const char* name)
{
    Ogre::OverlayContainer* oc = static_cast<Ogre::OverlayContainer*>(handle);
    oc->_removeChild(Ogre::String(name));
}

void overlaycontainer__positions_out_of_date(OverlayContainerHandle handle)
{
    Ogre::OverlayContainer* oc = static_cast<Ogre::OverlayContainer*>(handle);
    oc->_positionsOutOfDate();
}

void overlaycontainer__update(OverlayContainerHandle handle)
{
    Ogre::OverlayContainer* oc = static_cast<Ogre::OverlayContainer*>(handle);
    oc->_update();
}

unsigned short overlaycontainer__notify_zorder(OverlayContainerHandle handle, unsigned short new_zorder)
{
    Ogre::OverlayContainer* oc = static_cast<Ogre::OverlayContainer*>(handle);
    return oc->_notifyZOrder(new_zorder);
}

void overlaycontainer__notify_viewport(OverlayContainerHandle handle)
{
    Ogre::OverlayContainer* oc = static_cast<Ogre::OverlayContainer*>(handle);
    oc->_notifyViewport();
}

void overlaycontainer__notify_world_transforms(OverlayContainerHandle handle, const coiMatrix4* xform)
{
    Ogre::OverlayContainer* oc = static_cast<Ogre::OverlayContainer*>(handle);
    Ogre::Matrix4 m;
    llcoi_matrix4_to_ogre_matrix4(*xform, m);
    oc->_notifyWorldTransforms(m);
}

void overlaycontainer__notify_parent(OverlayContainerHandle handle, OverlayContainerHandle parent_handle, OverlayHandle overlay_handle)
{
    Ogre::OverlayContainer* oc = static_cast<Ogre::OverlayContainer*>(handle);
    Ogre::OverlayContainer* parent = static_cast<Ogre::OverlayContainer*>(parent_handle);
    Ogre::Overlay* overlay = static_cast<Ogre::Overlay*>(overlay_handle);
    oc->_notifyParent(parent, overlay);
}

int overlaycontainer_is_container(OverlayContainerHandle handle)
{
    Ogre::OverlayContainer* oc = static_cast<Ogre::OverlayContainer*>(handle);
    return oc->isContainer();
}

int overlaycontainer_is_children_process_events(OverlayContainerHandle handle)
{
    Ogre::OverlayContainer* oc = static_cast<Ogre::OverlayContainer*>(handle);
    return oc->isChildrenProcessEvents();
}

void overlaycontainer_set_children_process_events(OverlayContainerHandle handle, int val)
{
    Ogre::OverlayContainer* oc = static_cast<Ogre::OverlayContainer*>(handle);
    oc->setChildrenProcessEvents(val);
}

OverlayElementHandle overlaycontainer_find_element_at(OverlayContainerHandle handle, coiReal x, coiReal y)
{
    Ogre::OverlayContainer* oc = static_cast<Ogre::OverlayContainer*>(handle);
    Ogre::OverlayElement* oe = oc->findElementAt(x, y);
    return static_cast<OverlayElementHandle>(oe);
}

void overlaycontainer_copy_from_template(OverlayContainerHandle handle, OverlayElementHandle template_overlay)
{
    Ogre::OverlayContainer* oc = static_cast<Ogre::OverlayContainer*>(handle);
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(template_overlay);
    oc->copyFromTemplate(oe);
}

OverlayElementHandle overlaycontainer_clone(OverlayContainerHandle handle, const char* instance_name)
{
    Ogre::OverlayContainer* oc = static_cast<Ogre::OverlayContainer*>(handle);
    Ogre::OverlayElement* oe = oc->clone(Ogre::String(instance_name));
    return static_cast<OverlayElementHandle>(oe);
}
