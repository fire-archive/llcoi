/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "rendersystem_bind.h"

#include <OgreRoot.h>
#include <OgreRenderSystem.h>

void render_system_set_config_option(RenderSystemHandle render_system_handle, const char* option, const char* value)
{
    Ogre::RenderSystem* rs = reinterpret_cast<Ogre::RenderSystem*>(render_system_handle);
    rs->setConfigOption(option, value);
}

const char * render_system_get_name(RenderSystemHandle handle)
{
    Ogre::RenderSystem* rs = reinterpret_cast<Ogre::RenderSystem*>(handle);
    return rs->getName().c_str();
}

unsigned int render_system_list_size(RenderSystemListHandle list_handle)
{
    Ogre::RenderSystemList* rs_list = reinterpret_cast<Ogre::RenderSystemList*>(list_handle);
    return rs_list->size();
}

RenderSystemHandle render_system_list_get(RenderSystemListHandle list_handle, unsigned int index)
{
    Ogre::RenderSystemList* rs_list = reinterpret_cast<Ogre::RenderSystemList*>(list_handle);
    return rs_list->at(index);
}

void destroy_render_system_list(RenderSystemListHandle handle)
{
    Ogre::RenderSystemList* rs_list = reinterpret_cast<Ogre::RenderSystemList*>(handle);
    delete rs_list;
}
