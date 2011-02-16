#include <ogre_interface.h>

#include <OgreRoot.h>
#include <OgreRenderWindow.h>
#include <OgreWindowEventUtilities.h>
#include <OgreCamera.h>

#include <cstdlib>
#include <cstring>
#include <string>

bool initialized;

void release_engine()
{
    delete Ogre::Root::getSingletonPtr();
}

void load_ogre_plugin(const char* plugin);

static const char * plugin_folder = NULL;

static Ogre::RenderWindow* activeRenderWindow;

void default_engine_options(engine_options* options)
{
    options->renderer_s = "OpenGL";
#ifdef PLATFORM_WIN
    options->plugin_folder_s = ".";
#else
    options->plugin_folder_s = "/usr/local/lib/OGRE";
#endif
    options->window_title = "Renderwindow";
    options->width = 800;
    options->height = 600;
    options->auto_window = 1;
    options->log_name = "Ogre.log";
}

void init_engine(const engine_options options)
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

    if (options.auto_window) {
        activeRenderWindow = root->initialise(true , options.window_title);
    } else {
        root->initialise(false , options.window_title);
    }
}

RenderWindowHandle root_initialise(int auto_create_window, const char* render_window_title)
{
    Ogre::RenderWindow* window = Ogre::Root::getSingletonPtr()->initialise(auto_create_window, render_window_title);
	if(auto_create_window)
		activeRenderWindow = window;
    return reinterpret_cast<RenderWindowHandle>(window);
}

DLL int root_is_initialised()
{
    if(Ogre::Root::getSingletonPtr()->isInitialised())
        return 1;
    return 0;
}

RootHandle create_root(const char* pluginFileName, const char* configFileName, const char* logFileName)
{
    Ogre::Root* root = new Ogre::Root(Ogre::String(pluginFileName), Ogre::String(configFileName), Ogre::String(logFileName));
    return reinterpret_cast<RootHandle>(root);
}

void save_config()
{
    Ogre::Root::getSingletonPtr()->saveConfig();
}

int restore_config()
{
    if(Ogre::Root::getSingletonPtr()->restoreConfig())
        return 1;
    return 0;
}

int show_config_dialog()
{
    if(Ogre::Root::getSingletonPtr()->showConfigDialog())
        return 1;
    return 0;
}

void add_render_system(RenderSystemHandle render_system)
{
    Ogre::RenderSystem* rs = reinterpret_cast<Ogre::RenderSystem*>(render_system);
    Ogre::Root::getSingletonPtr()->addRenderSystem(rs);
}

void set_render_system(RenderSystemHandle render_system)
{
    Ogre::RenderSystem* rs = reinterpret_cast<Ogre::RenderSystem*>(render_system);
    Ogre::Root::getSingletonPtr()->setRenderSystem(rs);
}

RenderSystemHandle get_render_system_by_name(const char* render_system_name)
{
    Ogre::RenderSystem* rs = Ogre::Root::getSingletonPtr()->getRenderSystemByName(render_system_name);
    return reinterpret_cast<RenderSystemHandle>(rs);
}

RenderSystemHandle get_render_system()
{
    return reinterpret_cast<RenderSystemHandle>(Ogre::Root::getSingletonPtr()->getRenderSystem());
}

void render_system_set_config_option(RenderSystemHandle render_system_handle, const char* option, const char* value)
{
    Ogre::RenderSystem* rs = reinterpret_cast<Ogre::RenderSystem*>(render_system_handle);
    rs->setConfigOption(option, value);
}

SceneManagerHandle create_scene_manager(const char* type_name, ...)
{
    const char* instance_name = "default";
    Ogre::SceneManager* sm = Ogre::Root::getSingletonPtr()->createSceneManager(Ogre::String(type_name), Ogre::String(instance_name));
    return reinterpret_cast<SceneManagerHandle>(sm);
}

SceneManagerHandle get_scene_manager()
{
    Ogre::SceneManager* sm = Ogre::Root::getSingletonPtr()->getSceneManager("default");
    return reinterpret_cast<SceneManagerHandle>(sm);
}

SceneManagerHandle get_scene_manager_by_name(const char* scene_manager_instance_name)
{
    Ogre::SceneManager* sm = Ogre::Root::getSingletonPtr()->getSceneManager(scene_manager_instance_name);
    return reinterpret_cast<SceneManagerHandle>(sm);
}


void load_ogre_plugin(const char* plugin)
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

CameraHandle get_camera(const char* camera_name)
{
    Ogre::Camera* camera =  Ogre::Root::getSingletonPtr()->getSceneManager("default")->getCamera(camera_name);
    return reinterpret_cast<CameraHandle>(camera);
}

CameraHandle create_camera(const char* camera_name)
{
    Ogre::Camera* camera = Ogre::Root::getSingletonPtr()->getSceneManager("default")->createCamera(camera_name);
    return reinterpret_cast<CameraHandle>(camera);
}

void camera_set_near_clip_distance(CameraHandle camera_handle, float d)
{
    Ogre::Camera* camera = reinterpret_cast<Ogre::Camera*>(camera_handle);
    camera->setNearClipDistance( d );
}

void camera_set_far_clip_distance(CameraHandle camera_handle, float d)
{
    Ogre::Camera* camera = reinterpret_cast<Ogre::Camera*>(camera_handle);
    camera->setFarClipDistance( d );
}

void camera_set_auto_aspect_ratio(CameraHandle camera_handle, bool on)
{
    Ogre::Camera* camera = reinterpret_cast<Ogre::Camera*>(camera_handle);
    camera->setAutoAspectRatio(on);
}

void camera_set_fovy(CameraHandle camera_handle, float angle)
{
    Ogre::Camera* camera = reinterpret_cast<Ogre::Camera*>(camera_handle);
    camera->setFOVy((Ogre::Radian)angle);
}

void camera_set_frustum_offset(CameraHandle camera_handle, const int offset_x, const int offset_y)
{
    Ogre::Camera* camera = reinterpret_cast<Ogre::Camera*>(camera_handle);
    camera->setFrustumOffset(Ogre::Vector2(offset_x, offset_y));
}

void camera_set_focal_length(CameraHandle camera_handle, float fl)
{
    Ogre::Camera* camera = reinterpret_cast<Ogre::Camera*>(camera_handle);
    camera->setFocalLength(fl);
}

void camera_set_position(CameraHandle camera_handle, const float x, const float y, const float z)
{
    Ogre::Camera* camera = reinterpret_cast<Ogre::Camera*>(camera_handle);
    camera->setPosition(Ogre::Vector3(x, y, z));
}

void camera_lookat(CameraHandle camera_handle, const float x, const float y, const float z)
{
    Ogre::Camera* camera = reinterpret_cast<Ogre::Camera*>(camera_handle);
    camera->lookAt(Ogre::Vector3(x, y, z));
}

void add_viewport(CameraHandle camera_handle)
{
    Ogre::Camera* camera = reinterpret_cast<Ogre::Camera*>(camera_handle);
    Ogre::RenderWindow* window = activeRenderWindow;
    Ogre::Viewport* vp = window->addViewport(camera);
    vp->setBackgroundColour(Ogre::ColourValue(0,0,0));

    // Alter the camera aspect ratio to match the viewport
    camera->setAspectRatio(
        Ogre::Real(vp->getActualWidth()) / Ogre::Real(vp->getActualHeight()));
}

void add_resource_location(const char* location, const char* type, const char* group)
{
    Ogre::ResourceGroupManager::getSingleton().addResourceLocation(location, type, group);
}

void initialise_all_resourcegroups()
{
    Ogre::ResourceGroupManager::getSingleton().initialiseAllResourceGroups();
}

EntityHandle create_entity(const char* entity_name, const char* mesh_file)
{
    Ogre::Entity* entity = Ogre::Root::getSingletonPtr()->getSceneManager("default")->createEntity(entity_name, mesh_file);
    return reinterpret_cast<EntityHandle>(entity);
}

SceneNodeHandle create_child_scenenode(const char* node_name)
{
    Ogre::SceneNode* scenenode = Ogre::Root::getSingletonPtr()->getSceneManager("default")->getRootSceneNode()->createChildSceneNode(node_name);
    return reinterpret_cast<SceneNodeHandle>(scenenode);
}

void attach_entity_to_scenenode(EntityHandle entity_handle, SceneNodeHandle scenenode_handle)
{
    Ogre::SceneNode* node = reinterpret_cast<Ogre::SceneNode*>(scenenode_handle);
    Ogre::MovableObject* object = reinterpret_cast<Ogre::MovableObject*>(entity_handle);
    node->attachObject(object);
}

void set_ambient_light_rgba(const float r, const float g, const float b, const float a)
{
    Ogre::Root::getSingletonPtr()->getSceneManager("default")->setAmbientLight(Ogre::ColourValue(r, g, b, a));
}

void set_ambient_light_rgb(const float r, const float g, const float b)
{
    Ogre::Root::getSingletonPtr()->getSceneManager("default")->setAmbientLight(Ogre::ColourValue(r, g, b));
}

LightHandle create_light(const char* light_name)
{
    Ogre::Light* light = Ogre::Root::getSingletonPtr()->getSceneManager("default")->createLight(light_name);
    return reinterpret_cast<LightHandle>(light);
}

void light_set_position(LightHandle light_handle, const float x, const float y, const float z)
{
    Ogre::Light* light = reinterpret_cast<Ogre::Light*>(light_handle);
    light->setPosition(Ogre::Vector3(x, y, z));
}

void set_default_num_mipmaps(int number)
{
    Ogre::TextureManager::getSingleton().setDefaultNumMipmaps(number);
}

int render_one_frame()
{
    if(Ogre::Root::getSingletonPtr()->renderOneFrame())
        return 1;
    return 0;
}

int render_one_frame_custom(float time_since_last_frame)
{
    if(Ogre::Root::getSingletonPtr()->renderOneFrame(time_since_last_frame))
        return 1;
    return 0;
}

static bool do_render = 1;

void render_loop()
{
    while (do_render)
    {
        // Pump window messages for nice behaviour
        Ogre::WindowEventUtilities::messagePump();

        // Render a frame
        Ogre::Root::getSingletonPtr()->renderOneFrame();

        if (Ogre::Root::getSingletonPtr()->getAutoCreatedWindow()->isClosed())
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
