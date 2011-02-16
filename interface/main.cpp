#define EXPORTING
#include <ogre_interface.h>

#include <OgreRoot.h>
#include <OgreRenderWindow.h>
#include <OgreWindowEventUtilities.h>
#include <OgreCamera.h>

#include <cstdlib>
#include <cstring>
#include <string>

using namespace Ogre;

bool initialized;

const char *emptyCString = "";
std::string emptyString = emptyCString;
std::string strPool[4];  // String pool for avoiding memory allocations of temporary string objects


inline const std::string &safeStr( const char* str, int index )
{
    if( str != 0x0 ) return strPool[index] = str;
    else return emptyString;
}

DLL void release_engine()
{
    delete Ogre::Root::getSingletonPtr();
}

void load_ogre_plugin(const char* plugin);

static const char * plugin_folder = NULL;

DLL void default_engine_options(engine_options &options)
{
    options.renderer_s = "OpenGL";
#ifdef PLATFORM_WIN
    options.plugin_folder_s = ".";
#else
    options.plugin_folder_s = "/usr/local/lib/OGRE";
#endif    
    options.window_title = "Renderwindow";
    options.width = 800;
    options.height = 600;
    options.auto_window = 1;
    options.log_name = (char*) "Ogre.log";
}

DLL void init_engine(const engine_options options)
{
    plugin_folder = options.plugin_folder_s;
    // suppress console logging
    Ogre::LogManager * log_man = new Ogre::LogManager();
    Ogre::Log * vge_log = log_man->createLog(options.log_name, true, false);
    Ogre::Root * root = new Ogre::Root("", "", "");

    // default
    const char * renderer = "OpenGL Rendering Subsystem";
    const char * render_plugin = "RenderSystem_GL";

    if (strstr(options.renderer_s,"Direct") || strstr(options.renderer_s,"D3D")) {
        renderer = "Direct3D9 Rendering Subsystem";
        render_plugin = "RenderSystem_Direct3D9";
    } else if (!strstr(options.renderer_s,"GL"))
        Ogre::LogManager::getSingleton().logMessage(
            "Can't parse renderer string, using default (OpenGL)");

    load_ogre_plugin(render_plugin);
    Ogre::RenderSystem* rs = root->getRenderSystemByName( Ogre::String(renderer) );
    rs->setConfigOption("Full Screen", "No");
    rs->setConfigOption("VSync", "No");
    //rs->setConfigOption("Video Mode", "800 x 600 @ 32-bit");
    rs->setConfigOption("Video Mode", Ogre::StringConverter::toString(options.width) + " x " + 
                            Ogre::StringConverter::toString(options.height) + " @ 32-bit");
    
    root->setRenderSystem(rs);
    
    load_ogre_plugin("Plugin_OctreeSceneManager");
    
    Ogre::SceneManager * scene_manager = 
        root->createSceneManager(Ogre::ST_GENERIC, "scene-manager");

    if(options.auto_window) {
        root->initialise(true , options.window_title);
    }else{
        root->initialise(false , options.window_title);
    }
}

DLL void load_ogre_plugin(const char* plugin)
{
#if defined( WIN32 ) || defined( _WINDOWS )
Ogre::String pluginString(plugin);
#ifdef _DEBUG
    Ogre::Root::getSingleton().loadPlugin(pluginString + Ogre::String("_d"));
#else
    Ogre::Root::getSingleton().loadPlugin(plugin);
#endif
#else
    Ogre::Root::getSingleton().loadPlugin( Ogre::String(plugin_folder) + "/" + plugin );
#endif
}

DLL void* get_camera(const char* camera_name)
{
    Ogre::Camera* camera =  Ogre::Root::getSingletonPtr()->getSceneManager("scene-manager")->getCamera(camera_name);
    return static_cast<void*>(camera);
}

DLL void* create_camera(const char* camera_name)
{
    Ogre::Camera* camera = Ogre::Root::getSingletonPtr()->getSceneManager("scene-manager")->createCamera(camera_name);
    return static_cast<void*>(camera);
}

DLL void camera_set_near_clip_distance(void* camera_handle, float d)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(camera_handle);
    camera->setNearClipDistance( d );
}

DLL void camera_set_far_clip_distance(void* camera_handle, float d)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(camera_handle);
    camera->setFarClipDistance( d );
}

DLL void camera_set_auto_aspect_ratio(void* camera_handle, bool on)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(camera_handle);
    camera->setAutoAspectRatio(on);
}

DLL void camera_set_fovy(void* camera_handle, float angle)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(camera_handle);
    camera->setFOVy((Ogre::Radian)angle);
}

DLL void camera_set_frustum_offset(void* camera_handle, const int offset_x, const int offset_y)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(camera_handle);
    camera->setFrustumOffset(Ogre::Vector2(offset_x, offset_y));
}

DLL void camera_set_focal_length(void* camera_handle, float fl)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(camera_handle);
    camera->setFocalLength(fl);
}

DLL void camera_set_position(void* camera_handle, const float x, const float y, const float z)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(camera_handle);
    camera->setPosition(Ogre::Vector3(x, y, z));
}

DLL void camera_lookat(void* camera_handle, const float x, const float y, const float z)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(camera_handle);
    camera->lookAt(Ogre::Vector3(x, y, z));
}

DLL void add_viewport(void* camera_handle)
{
    Ogre::Camera* camera = static_cast<Ogre::Camera*>(camera_handle);
    Ogre::RenderWindow* window = Ogre::Root::getSingletonPtr()->getAutoCreatedWindow();
    Ogre::Viewport* vp = window->addViewport(camera);
    vp->setBackgroundColour(Ogre::ColourValue(0,0,0));

    // Alter the camera aspect ratio to match the viewport
    camera->setAspectRatio(
        Ogre::Real(vp->getActualWidth()) / Ogre::Real(vp->getActualHeight()));
}

DLL void add_resource_location(const char* location, const char* type, const char* group)
{
    Ogre::ResourceGroupManager::getSingleton().addResourceLocation(location, type, group);
}

DLL void initialise_all_resourcegroups()
{
    Ogre::ResourceGroupManager::getSingleton().initialiseAllResourceGroups();    
}

DLL void* create_entity(const char* entity_name, const char* mesh_file)
{
    Ogre::Entity* entity = Ogre::Root::getSingletonPtr()->getSceneManager("scene-manager")->createEntity(entity_name, mesh_file);
    return static_cast<void*>(entity);
}

DLL void* create_child_scenenode(const char* node_name)
{
    Ogre::SceneNode* scenenode = Ogre::Root::getSingletonPtr()->getSceneManager("scene-manager")->getRootSceneNode()->createChildSceneNode(node_name);
    return static_cast<void*>(scenenode);
}

DLL void attach_entity_to_scenenode(void* entity_handle, void* scenenode_handle)
{
    Ogre::SceneNode* node = static_cast<Ogre::SceneNode*>(scenenode_handle);
    Ogre::MovableObject* object = reinterpret_cast<Ogre::MovableObject*>(entity_handle);
    node->attachObject(object);
}

DLL void set_ambient_light_rgba(const float r, const float g, const float b, const float a)
{
    Ogre::Root::getSingletonPtr()->getSceneManager("scene-manager")->setAmbientLight(Ogre::ColourValue(r, g, b, a));
}

DLL void set_ambient_light_rgb(const float r, const float g, const float b)
{
    Ogre::Root::getSingletonPtr()->getSceneManager("scene-manager")->setAmbientLight(Ogre::ColourValue(r, g, b));
}

DLL void* create_light(const char* light_name)
{
    Ogre::Light* light = Ogre::Root::getSingletonPtr()->getSceneManager("scene-manager")->createLight(light_name);
    return static_cast<void*>(light);
}

DLL void light_set_position(void* light_handle, const float x, const float y, const float z)
{
    Ogre::Light* light = static_cast<Ogre::Light*>(light_handle);
    light->setPosition(Ogre::Vector3(x, y, z));
}

DLL void set_default_num_mipmaps(int number)
{
    Ogre::TextureManager::getSingleton().setDefaultNumMipmaps(number);
}


bool do_render = 1;

DLL void render_loop()
{
    while(do_render)
    {
        // Pump window messages for nice behaviour
        Ogre::WindowEventUtilities::messagePump();
 
        // Render a frame
        Ogre::Root::getSingletonPtr()->renderOneFrame();
 
        if(Ogre::Root::getSingletonPtr()->getAutoCreatedWindow()->isClosed())
        {
            do_render = 0;
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
