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
#		ifndef EXPORTING
#			define DLL __declspec(dllimport)
#		else
#			define DLL __declspec(dllexport)
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

#ifdef _cplusplus
extern "C" {
#endif

extern DLL void release_engine();

extern DLL void default_engine_options(engine_options* options);

extern DLL void init_engine(const engine_options options);

extern DLL void load_ogre_plugin(const char * plugin);

extern DLL void* create_camera(const char* name);

extern DLL void* get_camera(const char* camera_name);

extern DLL void camera_set_near_clip_distance(void* camera_handle, float d);

extern DLL void camera_set_far_clip_distance(void* camera_handle, float d);

extern DLL void camera_set_auto_aspect_ratio(void* camera_handle, int on);

extern DLL void camera_set_fovy(void* camera_handle, float angle);

extern DLL void camera_set_frustum_offset(void* camera_handle, const int offset_x, const int offset_y);

extern DLL void camera_set_focal_length(void* camera_handle, float fl);

extern DLL void camera_set_position(void* camera_handle, const float x, const float y, const float z);

extern DLL void camera_lookat(void* camera_handle, const float x, const float y, const float z);

extern DLL void add_viewport(void* camera_handle);

extern DLL void render_loop();

extern DLL void add_resource_location(const char* location, const char* type, const char* group);

extern DLL void initialise_all_resourcegroups();

extern DLL void* create_entity(const char* entity_name, const char* mesh_file);

extern DLL void* create_child_scenenode(const char* node_name);

extern DLL void attach_entity_to_scenenode(void* entity_handle, void* scenenode_handle);

extern DLL void set_ambient_light_rgba(const float r, const float g, const float b, const float a);

extern DLL void set_ambient_light_rgb(const float r, const float g, const float b);

extern DLL void* create_light(const char* light_name);

extern DLL void light_set_position(void* light_handle, const float x, const float y, const float z);

extern DLL void set_default_num_mipmaps(int number);

#ifdef _cplusplus
}
#endif
