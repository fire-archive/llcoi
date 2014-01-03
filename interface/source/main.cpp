/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "main.h"

#include <OgreRoot.h>

void log_message(const char* message)
{
	Ogre::LogManager::getSingletonPtr()->logMessage(message);
}

void set_default_num_mipmaps(int number)
{
    Ogre::TextureManager::getSingleton().setDefaultNumMipmaps(number);
}

// It might be hacky, but probably less so than a nested struct of
// const char* and tons of realloc calls.
NameValuePairListHandle create_name_value_pair_list()
{
    Ogre::NameValuePairList* pair_list = new Ogre::NameValuePairList;
    return reinterpret_cast<NameValuePairListHandle>(pair_list);
}

void add_pair(NameValuePairListHandle params, const char* name, const char* value)
{
    Ogre::NameValuePairList* pair_list = reinterpret_cast<Ogre::NameValuePairList*>(params);
    std::string n = std::string str(name)
    std::string v = std::string str(value)
    pair_list->insert(std::make_pair(n, v));
}

void destroy_name_value_pair_list(NameValuePairListHandle params)
{
    Ogre::NameValuePairList *pair_list = reinterpret_cast<Ogre::NameValuePairList*>(params);
    pair_list->clear();
    delete pair_list;
}

#ifdef PLATFORM_WIN
BOOL APIENTRY DllMain(HANDLE /*hModule*/, DWORD /*ul_reason_for_call*/, LPVOID /*lpReserved*/)
{
#if defined( _MSC_VER ) && defined( _DEBUG )
	//_crtBreakAlloc = 1397;
	_CrtSetDbgFlag ( _CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF );
#endif
}
#endif
