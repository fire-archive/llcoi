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
