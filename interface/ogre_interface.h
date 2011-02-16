#pragma once

// Detect platform
#if defined( WINCE )
#   if !defined( PLATFORM_WIN_CE )
#       define PLATFORM_WIN_CE
#   endif
#elif defined( WIN32 ) || defined( _WINDOWS )
#   if !defined( PLATFORM_WIN )
#       define PLATFORM_WIN
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

#define BUILD_DYNAMIC
#if defined(BUILD_DYNAMIC)
#	if defined( WIN32 ) || defined( _WINDOWS )
#		ifndef OgreInterface_EXPORTS
#			define DLL __declspec(dllimport)
#		else
#			define DLL extern "C" __declspec(dllexport)
#		endif
#	else
#		if defined( __GNUC__ ) && __GNUC__ >= 4
#			define DLL __attribute__ ((visibility("default")))
#		else
#			define DLL
#		endif
#	endif
#else
#	define DLL
#endif

#define CameraHandle void*
#define EntityHandle void*
#define SceneNodeHandle void*
#define LightHandle void*

typedef struct
{
    float w;
    float x;
    float y;
    float z;
} coiQuaternion;

typedef struct
{
    float x;
    float y;
    float z;
} coiVector3;

typedef struct
{
    const char* renderer_s;
    const char* plugin_folder_s;
    const char* window_title;
    const char* log_name;
    int width, height, auto_window;
} engine_options;

DLL void release_engine();

DLL void default_engine_options(engine_options* options);

DLL void init_engine(const engine_options options);

DLL void load_ogre_plugin(const char * plugin);

DLL CameraHandle create_camera(const char* name);

DLL CameraHandle get_camera(const char* camera_name);

DLL void camera_set_near_clip_distance(CameraHandle camera_handle, float d);

DLL void camera_set_far_clip_distance(CameraHandle camera_handle, float d);

DLL void camera_set_auto_aspect_ratio(CameraHandle camera_handle, int on);

DLL void camera_set_fovy(CameraHandle camera_handle, float angle);

DLL void camera_set_frustum_offset(CameraHandle camera_handle, const int offset_x, const int offset_y);

DLL void camera_set_focal_length(CameraHandle camera_handle, float fl);

DLL void camera_set_position(CameraHandle camera_handle, const float x, const float y, const float z);

DLL void camera_lookat(CameraHandle camera_handle, const float x, const float y, const float z);

DLL void add_viewport(CameraHandle camera_handle);

DLL void render_loop();

DLL void add_resource_location(const char* location, const char* type, const char* group);

DLL void initialise_all_resourcegroups();

DLL EntityHandle create_entity(const char* entity_name, const char* mesh_file);

DLL SceneNodeHandle create_child_scenenode(const char* node_name);

DLL void attach_entity_to_scenenode(EntityHandle entity_handle, SceneNodeHandle scenenode_handle);

DLL void set_ambient_light_rgba(const float r, const float g, const float b, const float a);

DLL void set_ambient_light_rgb(const float r, const float g, const float b);

DLL LightHandle create_light(const char* light_name);

DLL void light_set_position(LightHandle light_handle, const float x, const float y, const float z);

DLL void set_default_num_mipmaps(int number);
