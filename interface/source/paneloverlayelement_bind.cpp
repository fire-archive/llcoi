/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "paneloverlayelement_bind.h"
#include "binding_utils.h"
#include <Overlay/OgrePanelOverlayElement.h>

//PanelOverlayElement(const String& name);
PanelOverlayElementHandle create_paneloverlayelement(const char* name)
{
    Ogre::PanelOverlayElement* poe = new Ogre::PanelOverlayElement(Ogre::String(name));
    return static_cast<PanelOverlayElementHandle>(poe);
}

//~PanelOverlayElement();
void destroy_paneloverlayelement(PanelOverlayElementHandle handle)
{
    Ogre::PanelOverlayElement* poe = static_cast<Ogre::PanelOverlayElement*>(handle);
    delete poe;
}

//void initialise(void);
void paneloverlayelement_initialise(PanelOverlayElementHandle handle)
{
    Ogre::PanelOverlayElement* poe = static_cast<Ogre::PanelOverlayElement*>(handle);
    poe->initialise();
}

//void setTiling(Real x, Real y, ushort layer = 0);
void paneloverlayelement_set_tiling(PanelOverlayElementHandle handle, coiReal x, coiReal y, unsigned short layer)
{
    Ogre::PanelOverlayElement* poe = static_cast<Ogre::PanelOverlayElement*>(handle);
    poe->setTiling(x,y,layer);
}

//Real getTileX(ushort layer = 0) const;
coiReal paneloverlayelement_get_tile_x(const PanelOverlayElementHandle handle, unsigned short layer)
{
    const Ogre::PanelOverlayElement* poe = static_cast<const Ogre::PanelOverlayElement*>(handle);
    return poe->getTileX(layer);
}

//Real getTileY(ushort layer = 0) const;
coiReal paneloverlayelement_get_tile_y(const PanelOverlayElementHandle handle, unsigned short layer)
{
    const Ogre::PanelOverlayElement* poe = static_cast<const Ogre::PanelOverlayElement*>(handle);
    return poe->getTileY(layer);
}

//void setUV(Real u1, Real v1, Real u2, Real v2);
void paneloverlayelement_set_uv(PanelOverlayElementHandle handle, coiReal u1, coiReal v1, coiReal u2, coiReal v2)
{
    Ogre::PanelOverlayElement* poe = static_cast<Ogre::PanelOverlayElement*>(handle);
    poe->setUV(u1, v1, u2, v2);
}

//void getUV(Real& u1, Real& v1, Real& u2, Real& v2) const;
void paneloverlayelement_get_uv(const PanelOverlayElementHandle handle, coiReal* u1, coiReal* v1, coiReal* u2, coiReal* v2)
{
    const Ogre::PanelOverlayElement* poe = static_cast<const Ogre::PanelOverlayElement*>(handle);
    poe->getUV(*u1, *v1, *u2, *v2);
}

//void setTransparent(bool isTransparent);
void paneloverlayelement_set_transparent(PanelOverlayElementHandle handle, int is_transparent)
{
    Ogre::PanelOverlayElement* poe = static_cast<Ogre::PanelOverlayElement*>(handle);
    poe->setTransparent(is_transparent);
}

//bool isTransparent(void) const;
int paneloverlayelement_is_transparent(const PanelOverlayElementHandle handle)
{
    const Ogre::PanelOverlayElement* poe = static_cast<const Ogre::PanelOverlayElement*>(handle);
    return poe->isTransparent();
}

//const String& getTypeName(void) const;
const char* paneloverlayelement_get_type_name(const PanelOverlayElementHandle handle)
{
    const Ogre::PanelOverlayElement* poe = static_cast<const Ogre::PanelOverlayElement*>(handle);
    return poe->getTypeName().c_str();
}

//void getRenderOperation(RenderOperation& op);
void paneloverlayelement_get_renderoperation(PanelOverlayElementHandle handle, RenderOperationHandle renderOp)
{
    Ogre::PanelOverlayElement* poe = static_cast<Ogre::PanelOverlayElement*>(handle);
    Ogre::RenderOperation* op = static_cast<Ogre::RenderOperation*>(renderOp);
    poe->getRenderOperation(*op);
}
//void setMaterialName(const String& matName);
void paneloverlayelement_set_material_name(PanelOverlayElementHandle handle, const char* mat_name)
{
    Ogre::PanelOverlayElement* poe = static_cast<Ogre::PanelOverlayElement*>(handle);
    poe->setMaterialName(Ogre::String(mat_name));
}

//TODO: void _updateRenderQueue(RenderQueue* queue);

