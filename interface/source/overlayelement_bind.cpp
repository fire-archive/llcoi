/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "overlayelement_bind.h"
#include "binding_utils.h"
#include <Overlay/OgreOverlayElement.h>
#include <Overlay/OgreOverlay.h>
#include <Overlay/OgreOverlayContainer.h>
#include <OgreCamera.h>

void destroy_overlayelement(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    delete oe;
}

void overlayelement_initialise(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    oe->initialise();
}

const char* overlayelement_get_name(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    return oe->getName().c_str();
}

void overlayelement_show(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    oe->show();
}

void overlayelement_hide(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    oe->hide();
}

int overlayelement_is_visible(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    return oe->isVisible();
}

int overlayelement_is_enabled(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    return oe->isEnabled();
}

void overlayelement_set_enabled(OverlayElementHandle handle, int b)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    oe->setEnabled(b);
}

void overlayelement_set_dimensions(OverlayElementHandle handle, coiReal width, coiReal height)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    oe->setDimensions(width, height);
}

void overlayelement_set_position(OverlayElementHandle handle, coiReal left, coiReal top)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    oe->setPosition(left, top);
}

void overlayelement_set_width(OverlayElementHandle handle, coiReal width)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    oe->setWidth(width);
}

coiReal overlayelement_get_width(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    return oe->getWidth();
}

void overlayelement_set_height(OverlayElementHandle handle, coiReal height)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    oe->setHeight(height);
}

coiReal overlayelement_get_height(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    return oe->getHeight();
}

void overlayelement_set_left(OverlayElementHandle handle, coiReal left)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    oe->setLeft(left);
}

coiReal overlayelement_get_left(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    return oe->getLeft();
}

void overlayelement_set_top(OverlayElementHandle handle, coiReal top)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    oe->setTop(top);
}

coiReal overlayelement_get_top(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    return oe->getTop();
}

coiReal overlayelement__get_left(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    return oe->_getLeft();
}

coiReal overlayelement__get_top(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    return oe->_getTop();
}

coiReal overlayelement__get_width(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    return oe->_getWidth();
}

coiReal overlayelement__get_height(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    return oe->_getHeight();
}

void overlayelement__set_left(OverlayElementHandle handle, coiReal left)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    oe->_setLeft(left);
}

void overlayelement__set_top(OverlayElementHandle handle, coiReal top)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    oe->_setTop(top);
}

void overlayelement__set_width(OverlayElementHandle handle, coiReal width)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    oe->_setWidth(width);
}

void overlayelement__set_height(OverlayElementHandle handle, coiReal height)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    oe->_setHeight(height);
}

void overlayelement__set_position(OverlayElementHandle handle, coiReal left, coiReal top)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    oe->_setPosition(left, top);
}

void overlayelement__set_dimensions(OverlayElementHandle handle, coiReal width, coiReal height)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    oe->_setDimensions(width, height);
}

const char* overlayelement_get_material_name(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    return oe->getMaterialName().c_str();
}

void overlayelement_set_material_name(OverlayElementHandle handle, const char* name)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    oe->setMaterialName(Ogre::String(name));
}

void overlayelement_get_world_transforms(OverlayElementHandle handle, coiMatrix4* xform)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    Ogre::Matrix4 getter;
    oe->getWorldTransforms(&getter);
    ogre_matrix4_to_llcoi_matrix4(getter, *xform);
}

void overlayelement__positions_out_of_date(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    oe->_positionsOutOfDate();
}

void overlayelement__update(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    oe->_update();
}

void overlayelement__update_from_parent(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    oe->_updateFromParent();
}

void overlayelement__notify_parent(OverlayElementHandle handle, OverlayContainerHandle parent_handle, OverlayHandle overlay_handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    Ogre::OverlayContainer* oc = static_cast<Ogre::OverlayContainer*>(parent_handle);
    Ogre::Overlay* overlay = static_cast<Ogre::Overlay*>(overlay_handle);

    oe->_notifyParent(oc, overlay);
}

coiReal overlayelement__get_derived_left(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    return oe->_getDerivedLeft();
}

coiReal overlayelement__get_derived_top(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    return oe->_getDerivedTop();
}

coiReal overlayelement__get_relative_width(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    return oe->_getRelativeWidth();
}

coiReal overlayelement__get_relative_height(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    return oe->_getRelativeHeight();
}

unsigned short overlayelement__notify_zorder(OverlayElementHandle handle, unsigned short new_zorder)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    return oe->_notifyZOrder(new_zorder);
}

void overlayelement__notify_world_transforms(OverlayElementHandle handle, const coiMatrix4* xform)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    Ogre::Matrix4 m;
    llcoi_matrix4_to_ogre_matrix4(*xform, m);
    oe->_notifyWorldTransforms(m);
}

void overlayelement__notify_viewport(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    oe->_notifyViewport();
}

const char* overlayelement_get_type_name(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    return oe->getTypeName().c_str();
}

void overlayelement_set_caption(OverlayElementHandle handle, const char* text)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    oe->setCaption(Ogre::DisplayString(text));
}

const char* overlayelement_get_caption(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    return oe->getCaption().asUTF8_c_str();
}

void overlayelement_set_colour(OverlayElementHandle handle, const coiColourValue* c)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    const Ogre::ColourValue colour(c->r, c->b, c->g, c->a);
    oe->setColour(colour);
}

void overlayelement_get_colour(OverlayElementHandle handle, coiColourValue* c)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    const Ogre::ColourValue& colour = oe->getColour();

    c->r = colour.r;
    c->b = colour.b;
    c->g = colour.g;
    c->a = colour.a;
}


void overlayelement_set_metrics_mode(OverlayElementHandle handle, gui_metrics_mode mode)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    Ogre::GuiMetricsMode gmm = llcoi_gui_metrics_mode_to_ogre_gui_metrics_mode(mode);
    oe->setMetricsMode(gmm);
}

gui_metrics_mode overlayelement_get_metrics_mode(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    Ogre::GuiMetricsMode GMM = oe->getMetricsMode();
    return ogre_gui_metrics_mode_to_llcoi_metrics_mode(GMM);
}


void overlayelement_set_horizontal_alignment(OverlayElementHandle handle, gui_horizontal_alignment gha)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    Ogre::GuiHorizontalAlignment GHA = llcoi_gui_horizontal_alignment_to_ogre_gui_horizontal_alignment(gha);
    oe->setHorizontalAlignment(GHA);
}

gui_horizontal_alignment overlayelement_get_horizontal_alignment(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    Ogre::GuiHorizontalAlignment GHA = oe->getHorizontalAlignment();
    return ogre_gui_horizontal_alignment_to_llcoi_gui_horizontal_alignment(GHA);
}

void overlayelement_set_vertical_alignment(OverlayElementHandle handle, gui_vertical_alignment gva)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    Ogre::GuiVerticalAlignment GVA = llcoi_gui_vertical_alignment_to_ogre_gui_vertical_alignment(gva);
    oe->setVerticalAlignment(GVA);
}

gui_vertical_alignment overlayelement_get_vertical_alignment(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    Ogre::GuiVerticalAlignment GVA = oe->getVerticalAlignment();
    return ogre_gui_vertical_alignment_to_llcoi_gui_vertical_alignment(GVA);
}


int overlayelement_contains(OverlayElementHandle handle, coiReal x, coiReal y)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    return oe->contains(x,y);
}

OverlayElementHandle overlayelement_find_element_at(OverlayElementHandle handle, coiReal x, coiReal y)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    Ogre::OverlayElement* found = oe->findElementAt(x, y);
    return static_cast<OverlayElementHandle>(found);
}

int overlayelement_is_container(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    return oe->isContainer();
}

int overlayelement_is_key_enabled(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    return oe->isKeyEnabled();
}

int overlayelement_is_cloneable(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    return oe->isCloneable();
}

void overlayelement_set_cloneable(OverlayElementHandle handle, int c)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    oe->setCloneable(c);
}


OverlayContainerHandle overlayelement_get_parent(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    Ogre::OverlayContainer* oc = oe->getParent();
    return static_cast<OverlayContainerHandle>(oc);
}

void overlayelement_set_parent(OverlayElementHandle handle, OverlayContainerHandle parent_handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    Ogre::OverlayContainer* oc = static_cast<Ogre::OverlayContainer*>(parent_handle);
    oe->_setParent(oc);
}

unsigned short overlayelement_get_zorder(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    return oe->getZOrder();
}

coiReal overlayelement_get_squared_view_depth(OverlayElementHandle handle, CameraHandle camera_handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    const Ogre::Camera* cam = static_cast<Ogre::Camera*>(camera_handle);
    return oe->getSquaredViewDepth(cam);
}

void overlayelement_copy_from_template(OverlayElementHandle handle, OverlayElementHandle template_handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    Ogre::OverlayElement* tpl = static_cast<Ogre::OverlayElement*>(template_handle);
    oe->copyFromTemplate(tpl);
}

OverlayElementHandle overlayelement_clone(OverlayElementHandle handle, const char* instance_name)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    Ogre::OverlayElement* cloned = oe->clone(Ogre::String(instance_name));
    return static_cast<OverlayElementHandle>(cloned);
}

OverlayElementHandle overlayelement_get_source_template(OverlayElementHandle handle)
{
    Ogre::OverlayElement* oe = static_cast<Ogre::OverlayElement*>(handle);
    const Ogre::OverlayElement* source = oe->getSourceTemplate();
    // TODO: preserve constness
    return static_cast<OverlayElementHandle>(const_cast<Ogre::OverlayElement*>(source));
}
