/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "overlayelementfactory_bind.h"

#include <OgreOverlayElementFactory.h>
#include <OgreOverlayElement.h>
#include <OgrePanelOverlayElement.h>

class PanelOverlayElementFactoryBind : public Ogre::PanelOverlayElementFactory
{
public:
    Ogre::OverlayElement* createOverlayElement(const Ogre::String& instanceName)
    {
    }
};

