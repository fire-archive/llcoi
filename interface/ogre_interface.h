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

#if defined(BUILD_DYNAMIC)
#	if defined( WIN32 ) || defined( _WINDOWS )
#		ifdef EXPORTS
#			define DLL __declspec(dllexport)
#		else
#			define DLL __declspec(dllimport)
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

#define COI_DECLARE_HANDLE(name) typedef struct name##__ { int unused; } *name
//#define COI_DECLARE_HANDLE(name) typedef void* name

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

#define coiCamera void*
#define coiEntity void*

typedef struct
{
    const char* renderer_s;
    const char* plugin_folder_s;
    const char* window_title;
    const char* log_name;
    int width, height, auto_window;
} engine_options;

COI_DECLARE_HANDLE(coiSceneNodeHandle);
COI_DECLARE_HANDLE(coiLightHandle);

#ifdef _cplusplus
extern "C" {
#endif

extern DLL void release_engine();

extern DLL void default_engine_options(engine_options* options);

extern DLL void init_engine(const engine_options options);

extern DLL void load_ogre_plugin(const char * plugin);

extern DLL coiCamera create_camera_ex(const char* name);

extern DLL coiCamera create_camera(const char* name);

extern DLL coiCamera get_camera(const char* camera_name);

extern DLL void camera_set_near_clip_distance(coiCamera camera_handle, float d);

extern DLL void camera_set_far_clip_distance(coiCamera camera_handle, float d);

extern DLL void camera_set_auto_aspect_ratio(coiCamera camera_handle, int on);

extern DLL void camera_set_fovy(coiCamera camera_handle, float angle);

extern DLL void camera_set_frustum_offset(coiCamera camera_handle, const int offset_x, const int offset_y);

extern DLL void camera_set_focal_length(coiCamera camera_handle, float fl);

extern DLL void camera_set_position(coiCamera camera_handle, const float x, const float y, const float z);

extern DLL void camera_lookat(coiCamera camera_handle, const float x, const float y, const float z);

extern DLL void add_viewport(coiCamera camera_handle);

extern DLL void render_loop();

extern DLL void add_resource_location(const char* location, const char* type, const char* group);

extern DLL void initialise_all_resourcegroups();

extern DLL coiEntity create_entity(const char* entity_name, const char* mesh_file);

extern DLL coiSceneNodeHandle create_child_scenenode(const char* node_name);

extern DLL void attach_entity_to_scenenode(coiEntity entity_handle, coiSceneNodeHandle scenenode_handle);

extern DLL void set_ambient_light_rgba(const float r, const float g, const float b, const float a);

extern DLL void set_ambient_light_rgb(const float r, const float g, const float b);

extern DLL coiLightHandle create_light(const char* light_name);

extern DLL void light_set_position(coiLightHandle light_handle, const float x, const float y, const float z);

extern DLL void set_default_num_mipmaps(int number);

#ifdef _cplusplus
}
#endif
