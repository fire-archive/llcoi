/******************************************************************************
 * ogre_interface.h - main include file for C clients
 ******************************************************************************
 * This file is part of
 *     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 *                          
 * Low Level C Ogre Interface (llcoi)
 *
 * See http://code.google.com/p/llcoi/ for more information.
 *
 * Copyright (c) 2011, Llcoi Team
 * 
 * License: MIT
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 ******************************************************************************/
#pragma once
#ifndef LLCOI_OGRE_INTERFACE
#define LLCOI_OGRE_INTERFACE

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
#   endif
#endif

#if defined(LLCOI_BUILD_DYNAMIC)
#   if defined( WIN32 ) || defined( _WINDOWS )
#       ifndef llcoi_EXPORTS
#           define DLL __declspec(dllimport)
#       else
#           define DLL extern "C" __declspec(dllexport)
#       endif
#   else
#       ifndef llcoi_EXPORTS
#           define DLL
#       else
#           if defined( __GNUC__ ) && __GNUC__ >= 4
#               define DLL extern "C" __attribute__ ((visibility("default")))
#           else
#               define DLL extern "C"
#           endif
#       endif
#   endif
#else
#   if defined( LLCOI_BUILD_STATIC )
#       if defined( __GNUC__ ) && __GNUC__ >= 4
#           define DLL extern "C" __attribute__ ((visibility("default")))
#       else
#           define DLL extern "C"
#       endif
#   else
#       define DLL
#   endif
#endif

//defines

#define coiReal float

#define EVENT_FRAME_STARTED 1
#define EVENT_FRAME_RENDERING_QUEUED 2
#define EVENT_FRAME_ENDED 4


#define COI_DECLARE_HANDLE(name) struct name##__ { int unused; }; typedef struct name##__ *name

// COI_DECLARE_HANDLE(CameraHandle);
// COI_DECLARE_HANDLE(EntityHandle);
// COI_DECLARE_HANDLE(SceneNodeHandle);
// COI_DECLARE_HANDLE(LightHandle);
// COI_DECLARE_HANDLE(RenderWindowHandle);
// COI_DECLARE_HANDLE(RootHandle);
// COI_DECLARE_HANDLE(RenderSystemHandle);
// COI_DECLARE_HANDLE(SceneManagerHandle);
// COI_DECLARE_HANDLE(ViewportHandle);

#define CameraHandle void*
#define EntityHandle void*
#define SceneNodeHandle void*
#define LightHandle void*
#define RenderWindowHandle void*
#define RootHandle void*
#define RenderSystemHandle void*
#define RenderSystemListHandle void*
#define SceneManagerHandle void*
#define ViewportHandle void*
#define LogManagerHandle void*
#define LogHandle void*
#define LogListenerHandle void*
#define NameValuePairListHandle void*
#define FrameListenerHandle void*

// listener typedefs
typedef int(*FrameListenerEvent)(float,float,int);
typedef void(*WindowListenerEvent)(RenderWindowHandle);

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
    coiReal time_since_last_event;
    coiReal time_since_last_frame;
} FrameEvent;

typedef struct
{
    const char* renderer_s;
    const char* plugin_folder_s;
    const char* window_title;
    const char* log_name;
    int width, height, auto_window;
} engine_options;


typedef enum 
{
    LL_LOW = 1,
    LL_NORMAL = 2,
    LL_BOREME = 3
} logging_level;

typedef enum 
{
    LML_TRIVIAL = 1,
    LML_NORMAL = 2,
    LML_CRITICAL = 3
} log_message_level;


typedef void(*LogListenerEvent)(const char* message, log_message_level lml, int maskDebug, const char* log_name, int skip_message);

// Same as LogListenerEvent but allows the client to
// send additional data via a void pointer. We assume the
// client knows what they're doing if they use this. (:
typedef void(*LogListenerCtx)(const char* message, log_message_level lml, int maskDebug, const char* log_name, int skip_message, void* userdata);


// Again, variation of FrameListenerEvent
typedef int(*FrameListenerCtx)(const FrameEvent* event, int evt_type, void* userdata);

// Root functions
DLL void release_engine();

DLL void default_engine_options(engine_options* options);

DLL void init_engine(const engine_options options);

DLL RootHandle create_root(const char* pluginFileName, const char* configFileName, const char* logFileName);

DLL RenderWindowHandle root_initialise(int auto_create_window, const char* render_window_title);

DLL RenderWindowHandle create_render_window(const char* name, const int width, const int height, const int full_screen);

DLL RenderWindowHandle create_render_window_gl_context(const char* name, const int width, const int height, const int full_screen);

DLL RenderWindowHandle create_render_window_hwnd(const char* name, const int width, const int height, const int full_screen, unsigned long hwnd);

DLL unsigned int render_window_get_hwnd(RenderWindowHandle window_handle);

DLL void render_window_set_visible(RenderWindowHandle window_handle, int visible);

DLL void render_window_update(RenderWindowHandle window_handle, int swap_buffers);

DLL void current_window_update(int swap_buffers);

DLL void render_window_resize(unsigned int width, unsigned int height);

DLL void render_window_moved_or_resized();

DLL int render_window_closed();

DLL int root_is_initialised();

DLL void save_config();

DLL int restore_config();

DLL int show_config_dialog();

DLL void load_ogre_plugin(const char * plugin);

DLL SceneManagerHandle create_scene_manager(const char* type_name, const char* instance_name);

DLL SceneManagerHandle get_scene_manager();

DLL SceneManagerHandle get_scene_manager_by_name(const char* scene_manager_instance_name);

DLL int render_one_frame();

DLL int render_one_frame_ex(float time_since_last_frame);

DLL void render_loop();

DLL void pump_messages();

DLL void log_message(const char* message);

DLL RenderWindowHandle root_create_render_window(const char* name, unsigned int width, unsigned int height, int fullscreen, NameValuePairListHandle params);

DLL RenderSystemListHandle root_get_available_renderers();



// RenderSystem functions
DLL void set_render_system(RenderSystemHandle render_system);

DLL void add_render_system(RenderSystemHandle render_system);

DLL RenderSystemHandle get_render_system();

DLL RenderSystemHandle get_render_system_by_name(const char* render_system_name);

DLL void render_system_set_config_option(RenderSystemHandle render_system_handle, const char* option, const char* value);

DLL unsigned int render_system_list_size(RenderSystemListHandle list_handle);

DLL RenderSystemHandle render_system_list_get(RenderSystemListHandle list_handle, unsigned int index);

DLL void destroy_render_system_list(RenderSystemListHandle handle);


// SceneManager functions
DLL void set_default_num_mipmaps(int number);

DLL void set_ambient_light_rgba(const float r, const float g, const float b, const float a);

DLL void set_ambient_light_rgb(const float r, const float g, const float b);

DLL ViewportHandle add_viewport(CameraHandle camera_handle);

DLL void scene_manager_log_name();

// Scene nodes
DLL SceneNodeHandle create_child_scenenode(const char* node_name);

DLL void attach_entity_to_scenenode(EntityHandle entity_handle, SceneNodeHandle scenenode_handle);

DLL void scenenode_update(SceneNodeHandle scenenode_handle, int update_children, int parent_has_changed);

DLL void scenenode_update_bounds(SceneNodeHandle scenenode_handle);

DLL EntityHandle scenenode_get_attached_entity_int(SceneNodeHandle scenenode_handle, int entity_index);

DLL EntityHandle scenenode_get_attached_entity(SceneNodeHandle scenenode_handle, const char* entity_name);

DLL int scenenode_num_attached_objects(SceneNodeHandle scenenode_handle);

DLL void scenenode_detach_entity_int(SceneNodeHandle scenenode_handle, int entity_index);

DLL void scenenode_detach_entity(SceneNodeHandle scenenode_handle, EntityHandle entity_handle);

DLL void scenenode_detach_entity_string(SceneNodeHandle scenenode_handle, const char* entity_name);

DLL void scenenode_detach_all_objects(SceneNodeHandle scenenode_handle);

DLL int scenenode_is_in_scenegraph(SceneNodeHandle scenenode_handle);

DLL void scenenode_notify_rootnode(SceneNodeHandle scenenode_handle);

DLL void scenenode_show_boundingbox(SceneNodeHandle scenenode_handle, int show_boundingbox);

DLL void scenenode_hide_boundingbox(SceneNodeHandle scenenode_handle, int hide_boundingbox);

DLL int scenenode_get_show_boundingbox(SceneNodeHandle scenenode_handle);

DLL SceneNodeHandle scenenode_get_parent_scenenode(SceneNodeHandle scenenode_handle);

DLL void scenenode_set_visible(SceneNodeHandle scenenode_handle, int visible);

DLL void scenenode_set_visible_ex(SceneNodeHandle scenenode_handle, int visible, int cascade);

DLL void scenenode_flip_visibility(SceneNodeHandle scenenode_handle);

DLL void scenenode_flip_visibility_ex(SceneNodeHandle scenenode_handle, int cascade);

DLL void scenenode_set_debug_display_enabled(SceneNodeHandle scenenode_handle, int enabled);

DLL void scenenode_set_debug_display_enabled_ex(SceneNodeHandle scenenode_handle, int enabled, int cascade);

DLL SceneManagerHandle scenenode_get_creator(SceneNodeHandle scenenode_handle);

DLL void scenenode_set_direction(SceneNodeHandle scenenode_handle, float x, float y, float z);

DLL void scenenode_set_orientation(SceneNodeHandle scenenode_handle, float w, float x, float y, float z);

DLL void scenenode_set_position(SceneNodeHandle scenenode_handle, float x, float y, float z);

DLL void scenenode_yaw(SceneNodeHandle scenenode_handle, coiReal radians);

DLL void scenenode_set_scale(SceneNodeHandle scenenode_handle, float x, float y, float z);

DLL void scenenode_scale(SceneNodeHandle scenenode_handle, float x, float y, float z);

DLL void scenenode_translate(SceneNodeHandle scenenode_handle, float x, float y, float z);

DLL void scenenode_roll(SceneNodeHandle scenenode_handle, coiReal radians);

DLL void scenenode_pitch(SceneNodeHandle scenenode_handle, coiReal radians);

// Viewports
DLL void viewport_set_background_colour(ViewportHandle viewport_handle, float r, float g, float b);

DLL float viewport_get_width(ViewportHandle viewport_handle);

DLL float viewport_get_height(ViewportHandle viewport_handle);

// Resource management
DLL void setup_resources(const char* resources_cfg);

DLL void add_resource_location(const char* location, const char* type, const char* group);

DLL void initialise_all_resourcegroups();


// Camera
DLL CameraHandle create_camera(const char* camera_name);

DLL CameraHandle get_camera(const char* camera_name);

DLL void camera_set_near_clip_distance(CameraHandle camera_handle, float d);

DLL void camera_set_far_clip_distance(CameraHandle camera_handle, float d);

DLL void camera_set_aspect_ratio(CameraHandle camera_handle, float w, float h);

DLL void camera_set_auto_aspect_ratio(CameraHandle camera_handle, int on);

DLL void camera_set_fovy(CameraHandle camera_handle, float angle);

DLL void camera_set_frustum_offset(CameraHandle camera_handle, const int offset_x, const int offset_y);

DLL void camera_set_focal_length(CameraHandle camera_handle, float fl);

DLL void camera_set_position(CameraHandle camera_handle, const float x, const float y, const float z);

DLL void camera_lookat(CameraHandle camera_handle, const float x, const float y, const float z);


// Entity
DLL EntityHandle create_entity(const char* entity_name, const char* mesh_file);


// Light
DLL LightHandle create_light(const char* light_name);

DLL void light_set_position(LightHandle light_handle, const float x, const float y, const float z);


// FrameListener
DLL void add_frame_listener(FrameListenerEvent frame_event,int frame_event_type);

DLL void remove_frame_listener(FrameListenerEvent frame_event);

DLL FrameListenerHandle add_frame_listener_ctx(FrameListenerCtx callback, void* userdata);

DLL void remove_frame_listener_ctx(FrameListenerHandle handle);


// WindowListener
DLL void add_window_listener(RenderWindowHandle window_handle, WindowListenerEvent window_event);

DLL void remove_window_listener(RenderWindowHandle window_handle);

// LogManager
DLL LogManagerHandle create_log_manager();

// LogManager::getSingletonPtr
DLL LogManagerHandle get_log_manager();

//LogManager::getLog
DLL LogHandle logmanager_get_log(const char* name);

//LogManager::getDefaultLog
DLL LogHandle logmanager_get_default_log();

//LogManager::setDefaultLog
DLL LogHandle logmanager_set_default_log(LogHandle log_handle);

//LogManager::createLog
DLL LogHandle logmanager_create_log(const char* name, int default_log, int debugger_output, int suppress_file_output);

// n.b., Allows for finer grained control over the log messages at the cost of
// having to supply all these variables. If you don't need this control,
// use log_message above.
//LogManager::logMessage
DLL void logmanager_log_message(const char* message, log_message_level lml, int maskDebug, const char* log_name, int skip_message);

//LogManager::setLogDetail
DLL void logmanager_set_log_detail(logging_level lvl);

//LogManager::destroyLog
DLL void logmanager_destroy_log(const char* name);

//LogManager::destroyLog
DLL void logmanager_destroy_log_by_handle(LogHandle log_handle);

//Log::addListener
DLL LogListenerHandle add_log_listener(LogListenerEvent log_event, LogHandle log_handle);

//Log::addListener
DLL LogListenerHandle add_log_listener_ctx(LogListenerCtx log_event, LogHandle log_handle, void* userdata);

//Log::removeListener
DLL void remove_log_listener(LogListenerHandle handle, LogHandle log_handle);

//Log::addListener
DLL void remove_log_listener_ctx(LogListenerHandle handle, LogHandle log_handle);

//Log::logMessage
DLL void log_log_message(LogHandle handle, const char *message, log_message_level lml, int maskDebug);


// NameValuePairList 
DLL NameValuePairListHandle create_name_value_pair_list();
DLL void add_pair(NameValuePairListHandle params, const char* name, const char* value);
DLL void destroy_name_value_pair_list(NameValuePairListHandle params);


// RenderWindow
DLL ViewportHandle render_window_add_viewport(RenderWindowHandle window_handle, CameraHandle camera_handle, .../*int zorder, float left, float top, float width, float height*/);
DLL int render_window_is_closed(RenderWindowHandle handle);

// Vector3

//Vector3::operator !=
DLL int vector3_notequals_vector3(coiVector3 lhs, coiVector3 rhs);

//Vector3::operator ==
DLL int vector3_equals_vector3(coiVector3 lhs, coiVector3 rhs);

//Vector3::operator +
DLL coiVector3 vector3_add_vector3(coiVector3 lhs, coiVector3 rhs);

//Vector3::operator +=
DLL void vector3_update_add_vector3(coiVector3 lhs, coiVector3 rhs);

//Vector3::operator -
DLL coiVector3 vector3_subtract_vector3(coiVector3 lhs, coiVector3 rhs);

//Vector3::operator -=
DLL void vector3_update_subtract_vector3(coiVector3 lhs, coiVector3 rhs);

//Vector3::operator - 
DLL coiVector3 vector3_negate(coiVector3 v3);

// Vector3::operator/ 
DLL coiVector3 vector3_divide_vector3(coiVector3 lhs, coiVector3 rhs);

// Vector3::operator*
DLL coiVector3 vector3_multiply_vector3(coiVector3 lhs, coiVector3 rhs);


// Vector3::isNaN
DLL int vector3_is_nan(coiVector3 v3);

//Vector3::primaryAxis
DLL coiVector3 vector3_primary_axis(coiVector3);

// Vector3::ZERO
DLL coiVector3 vector3_ZERO();
DLL coiVector3 vector3_UNIT_X();
DLL coiVector3 vector3_UNIT_Y();
DLL coiVector3 vector3_UNIT_Z();
DLL coiVector3 vector3_NEGATIVE_UNIT_X();
DLL coiVector3 vector3_NEGATIVE_UNIT_Y();
DLL coiVector3 vector3_NEGATIVE_UNIT_Z();
DLL coiVector3 vector3_UNIT_SCALE();

#endif
