/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "texturemanager_bind.h"

#include <OgreTextureManager.h>

DLL TextureManagerHandle texturemanager_singleton()
{
  return reinterpret_cast<TextureManagerHandle>(Ogre::TextureManager::getSingletonPtr());
}

DLL void texturemanager_set_default_num_mipmaps(TextureManagerHandle tm_hande, int number)
{
  Ogre::TextureManager* tm = reinterpret_cast<Ogre::TextureManager*>(tm_hande);
  tm->setDefaultNumMipmaps(static_cast<size_t>(number));
}
