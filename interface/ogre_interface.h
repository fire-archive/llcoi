#pragma once

#ifndef DLL
#   if defined( WIN32 ) || defined( _WINDOWS )
#       define DLL extern "C" __declspec( dllimport )
#   else
#  if defined( __GNUC__ ) && __GNUC__ >= 4
#   define DLL extern "C" __attribute__ ((visibility("default")))
#  else
#       define DLL extern "C"
#  endif
#   endif
#endif

typedef struct engine_options
{
    char* renderer_s;
    char* plugin_folder_s;
    char* window_title;
    char* log_name;
    int width, height, auto_window;
} engine_options;

#ifdef _cplusplus

DLL void release_engine();

DLL void default_engine_options(struct engine_options &options);

DLL void init_engine(const struct engine_options options);

DLL void load_ogre_plugin(const char * plugin);

DLL Ogre::SceneManager * get_scene_manager();

DLL  Ogre::Camera * get_camera(const char * name);

DLL  Ogre::Camera * create_camera(const char * name);

DLL void camera_set_near_clip_distance(Ogre::Camera * camera, double d);

DLL void camera_set_far_clip_distance(Ogre::Camera * camera, double d);

DLL void camera_set_auto_aspect_ratio(Ogre::Camera * camera, bool on);

DLL void camera_set_fovy(Ogre::Camera * cam, Ogre::Degree angle);

DLL void camera_set_frustum_offset(Ogre::Camera * cam, const Ogre::Vector2& offset);

DLL void camera_set_focal_length(Ogre::Camera * cam, double fl);

DLL void add_viewport(const char * camera_name);

DLL void render_loop();

DLL void add_resource_location(const char * location, const char * type, const char * group);

DLL void initialize_all_resourcegroups();

#endif
