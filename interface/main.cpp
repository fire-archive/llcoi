#include <OgreRoot.h>
#include <OgreRenderWindow.h>
#include <OgreWindowEventUtilities.h>

#include <cstdlib>
#include <cstring>
#include <string>

// Detect platform
#if defined( WINCE )
#   if !defined( PLATFORM_WIN_CE )
#       define PLATFORM_WIN_CE
#   endif
#elif defined( WIN32 ) || defined( _WINDOWS )
#   if !defined( PLATFORM_WIN )
#       define PLATFORM_WIN
#       define PATH_SEP "\\"
#   endif
#elif defined( __APPLE__ ) || defined( __APPLE_CC__ )
#   if !defined( PLATFORM_MAC )
#      define PLATFORM_MAC
#   endif
#else
#   if !defined( PLATFORM_LINUX )
#       define PLATFORM_LINUX
#       define PATH_SEP "/"
#   endif
#endif


#ifndef DLLEXP
#   ifdef PLATFORM_WIN
#       define DLLEXP extern "C" __declspec( dllexport )
#   else
#       if defined( __GNUC__ ) && __GNUC__ >= 4
#         define DLLEXP extern "C" __attribute__ ((visibility("default")))
#       else
#         define DLLEXP extern "C"
#       endif
#   endif
#endif


bool initialized;

const char *emptyCString = "";
std::string emptyString = emptyCString;
std::string strPool[4];  // String pool for avoiding memory allocations of temporary string objects


inline const std::string &safeStr( const char *str, int index )
{
    if( str != 0x0 ) return strPool[index] = str;
    else return emptyString;
}

DLLEXP int ogre3dInit()
{
    if( initialized )
    {   
        return 1;
    }
    initialized = true;

    new Ogre::Root();
    
    return 1;
}

DLLEXP int ogre3dRelease()
{
    if( initialized )
    {   
        delete Ogre::Root::getSingletonPtr();
        return 1;
    }
    return 1;
}

DLLEXP void load_ogre_plugin(const char * plugin);

static const char * plugin_folder = NULL;

DLLEXP void init_engine(const char * renderer_s, const char * plugin_folder_s, int auto_window)
{
    plugin_folder = plugin_folder_s;
    // suppress console logging
    Ogre::LogManager * log_man = new Ogre::LogManager();
    Ogre::Log * vge_log = log_man->createLog("vge.log", true, false);
    Ogre::Root * root = new Ogre::Root("", "", "");

    // default
    const char * renderer = "OpenGL Rendering Subsystem";
    const char * render_plugin = "RenderSystem_GL";

    if (strstr(renderer_s,"Direct") || strstr(renderer_s,"D3D")) {
        renderer = "Direct3D9 Rendering Subsystem";
        render_plugin = "RenderSystem_Direct3D9";
    } else if (!strstr(renderer_s,"GL"))
        Ogre::LogManager::getSingleton().logMessage(
            "Can't parse renderer string, using default (OpenGL)");

    load_ogre_plugin(render_plugin);

    root->getRenderSystemByName( renderer )->setConfigOption("Full Screen", "No");
    root->getRenderSystemByName( renderer )->setConfigOption("VSync", "No");
    root->getRenderSystemByName( renderer )->setConfigOption("Video Mode", "800 x 600 @ 32-bit");
    
    root->setRenderSystem( root->getRenderSystemByName( renderer ));
    
    Ogre::SceneManager * scene_manager = 
        root->createSceneManager(Ogre::ST_GENERIC, "scene-manager");

    if(auto_window) {
        root->initialise(true , "VGE");
    }else{
        root->initialise(false , "VGE");
    }
}

DLLEXP void load_ogre_plugin(const char * plugin)
{
    Ogre::Root::getSingleton().loadPlugin( Ogre::String(plugin_folder) + 
                           PATH_SEP + plugin );
}

DLLEXP Ogre::SceneManager * get_scene_manager()
{
    Ogre::Root * root = Ogre::Root::getSingletonPtr();
    return root->getSceneManager("scene-manager");
}

DLLEXP  Ogre::Camera * get_camera(const char * name)
{
    Ogre::Camera * cam = get_scene_manager()->getCamera(name);
    return cam;
}

DLLEXP  Ogre::Camera * create_camera(const char * name)
{
    Ogre::Camera * cam = get_scene_manager()->createCamera(name);
    return cam;
}

DLLEXP void camera_set_near_clip_distance(Ogre::Camera * camera, double d)
{
    camera->setNearClipDistance( d );
}

DLLEXP void camera_set_far_clip_distance(Ogre::Camera * camera, double d)
{
    camera->setFarClipDistance( d );
}

DLLEXP void camera_set_auto_aspect_ratio(Ogre::Camera * camera, bool on)
{
    camera->setAutoAspectRatio(on);
}

DLLEXP void camera_set_fovy(Ogre::Camera * cam, Ogre::Degree angle)
{
    cam->setFOVy(angle);
}

DLLEXP void camera_set_frustum_offset(Ogre::Camera * cam, const Ogre::Vector2& offset)
{
    cam->setFrustumOffset(offset);
}

DLLEXP void camera_set_focal_length(Ogre::Camera * cam, double fl)
{
    cam->setFocalLength(fl);
}

DLLEXP void add_viewport(const char * camera_name)
{
    Ogre::RenderWindow* window = Ogre::Root::getSingletonPtr()->getAutoCreatedWindow();
    Ogre::Viewport* vp = window->addViewport(get_camera(camera_name));
    vp->setBackgroundColour(Ogre::ColourValue(0,0,0));

    // Alter the camera aspect ratio to match the viewport
    get_camera(camera_name)->setAspectRatio(
        Ogre::Real(vp->getActualWidth()) / Ogre::Real(vp->getActualHeight()));
}

DLLEXP void add_resource_location(const char * location, const char * type, const char * group)
{
    Ogre::ResourceGroupManager::getSingleton().addResourceLocation(location, type, group);
}

DLLEXP void initialize_all_resourcegroups()
{
    Ogre::ResourceGroupManager::getSingleton().initialiseAllResourceGroups();    
}


DLLEXP int render_loop()
{
    while(true)
    {
        // Pump window messages for nice behaviour
        Ogre::WindowEventUtilities::messagePump();
 
        // Render a frame
        Ogre::Root::getSingletonPtr()->renderOneFrame();
 
        if(Ogre::Root::getSingletonPtr()->getAutoCreatedWindow()->isClosed())
        {
            return 0;
        }
    }
}

#ifdef PLATFORM_WIN
BOOL APIENTRY DllMain( HANDLE /*hModule*/, DWORD /*ul_reason_for_call*/, LPVOID /*lpReserved*/ )
{
    #if defined( _MSC_VER ) && defined( _DEBUG )
    //_crtBreakAlloc = 1397;
    _CrtSetDbgFlag ( _CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF );
    #endif
    
    /*switch( ul_reason_for_call )
    {
    case DLL_PROCESS_DETACH:
        _CrtDumpMemoryLeaks();
        break;
    }*/
    
    return TRUE;
}
#endif
