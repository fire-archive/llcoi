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

#define COI_DECLARE_HANDLE(name) typedef struct name##__ { int unused; } *name

#ifdef COI_USE_DOUBLE_PRECISION
typedef double coiReal;
#else
typedef float coiReal;
#endif

typedef struct
{
    coiReal w;
    coiReal x;
    coiReal y;
    coiReal z;
} coiQuaternion;

typedef struct
{
    coiReal x;
    coiReal y;
    coiReal z;
} coiVector3;

#ifdef _cplusplus
extern "C" {
#endif

typedef struct
{
    const char* renderer_s;
    const char* plugin_folder_s;
    const char* window_title;
    const char* log_name;
    int width, height, auto_window;
} engine_options;

COI_DECLARE_HANDLE(coiCameraHandle);
COI_DECLARE_HANDLE(coiSceneNodeHandle);
COI_DECLARE_HANDLE(coiEntityHandle);
COI_DECLARE_HANDLE(coiLightHandle);

DLL void release_engine();

DLL void default_engine_options(engine_options* options);

DLL void init_engine(const engine_options options);

DLL void load_ogre_plugin(const char * plugin);

DLL coiCameraHandle create_camera(const char* name);

DLL coiCameraHandle get_camera(const char* camera_name);

DLL void camera_set_near_clip_distance(coiCameraHandle camera_handle, coiReal d);

DLL void camera_set_far_clip_distance(coiCameraHandle camera_handle, coiReal d);

DLL void camera_set_auto_aspect_ratio(coiCameraHandle camera_handle, int on);

DLL void camera_set_fovy(coiCameraHandle camera_handle, coiReal angle);

DLL void camera_set_frustum_offset(coiCameraHandle camera_handle, const int offset_x, const int offset_y);

DLL void camera_set_focal_length(coiCameraHandle camera_handle, coiReal fl);

DLL void camera_set_position(coiCameraHandle camera_handle, const coiReal x, const coiReal y, const coiReal z);

DLL void camera_lookat(coiCameraHandle camera_handle, const coiReal x, const coiReal y, const coiReal z);

DLL void add_viewport(coiCameraHandle camera_handle);

DLL void render_loop();

DLL void add_resource_location(const char* location, const char* type, const char* group);

DLL void initialise_all_resourcegroups();

DLL coiEntityHandle create_entity(const char* entity_name, const char* mesh_file);

DLL coiSceneNodeHandle create_child_scenenode(const char* node_name);

DLL void attach_entity_to_scenenode(coiEntityHandle entity_handle, coiSceneNodeHandle scenenode_handle);

DLL void set_ambient_light_rgba(const float r, const float g, const float b, const float a);

DLL void set_ambient_light_rgb(const float r, const float g, const float b);

DLL coiLightHandle create_light(const char* light_name);

DLL void light_set_position(coiLightHandle light_handle, const coiReal x, const coiReal y, const coiReal z);

DLL void set_default_num_mipmaps(int number);

#ifdef _cplusplus
}
#endif
