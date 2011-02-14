#pragma once

#ifndef DLL
#   if defined( WIN32 ) || defined( _WINDOWS )
#       define DLL __declspec( dllimport )
#   else
#  if defined( __GNUC__ ) && __GNUC__ >= 4
#   define DLL __attribute__ ((visibility("default")))
#  else
#       define DLL
#  endif
#   endif
#endif

#ifdef _cplusplus
extern "C" {
#endif

typedef struct
{
    char* renderer_s;
    char* plugin_folder_s;
    char* window_title;
    char* log_name;
    int width, height, auto_window;
} engine_options;

DLL void release_engine();

DLL void default_engine_options(engine_options* options);

DLL void init_engine(const engine_options options);

DLL void load_ogre_plugin(const char * plugin);

DLL void create_camera(const char* name);

DLL void camera_set_near_clip_distance(const char* camera_name, double d);

DLL void camera_set_far_clip_distance(const char* camera_name, double d);

DLL void camera_set_auto_aspect_ratio(const char* camera_name, int on);

DLL void camera_set_fovy(const char* camera_name, float angle);

DLL void camera_set_frustum_offset(const char* camera_name, const int offset_x, const int offset_y);

DLL void camera_set_focal_length(const char* camera_name, double fl);

DLL void camera_set_position(const char* camera_name, const float x, const float y, const float z);

DLL void camera_lookat(const char* camera_name, const float x, const float y, const float z);

DLL void add_viewport(const char* camera_name);

DLL void render_loop();

DLL void add_resource_location(const char* location, const char* type, const char* group);

DLL void initialise_all_resourcegroups();

DLL void create_entity(const char* entity_name, const char* mesh_file);

DLL void create_child_scenenode(const char* node_name);

DLL void attach_entity_to_scenenode(const char* object_name, const char* node_name);

DLL void set_ambient_light_rgba(const int r, const int g, const int b, const int a);

DLL void set_ambient_light_rgb(const int r, const int g, const int b);

DLL void create_light(const char* light_name);

DLL void light_set_position(const char* light_name, const float x, const float y, const float z);

DLL void textureManager_setDefaultNumMipmaps(int number);

#ifdef _cplusplus
}
#endif
