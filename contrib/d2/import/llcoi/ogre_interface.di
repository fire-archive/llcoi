/******************************************************************************
 * ogre_interface.di - main interface file for D clients
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

module llcoi.ogre_interface;

extern(C):

alias float coiReal;

const int EVENT_FRAME_STARTED = 1;
const int EVENT_FRAME_RENDERING_QUEUED = 2;
const int EVENT_FRAME_ENDED = 4;

// From OgrePlatform.h
alias uint uint32;
alias ushort uint16;
alias ubyte uint8;
alias int int32;
alias short int16;
alias byte int8;

// OgreSceneManager.h
alias ushort SceneTypeMask;

// OgreColourValue.h
alias uint32 RGBA;
alias uint32 ARGB;
alias uint32 ABGR;
alias uint32 BGRA;


alias void* CameraHandle;
alias void* EntityHandle;
alias void* SceneNodeHandle;
alias void* LightHandle;
alias void* RenderWindowHandle;
alias void* RootHandle;
alias void* RenderSystemHandle;
alias void* RenderSystemListHandle;
alias void* SceneManagerHandle;
alias void* ViewportHandle;
alias void* LogManagerHandle;
alias void* LogHandle;
alias void* LogListenerHandle;
alias void* NameValuePairListHandle;
alias void* FrameListenerHandle;
alias void* PlaneHandle;
alias void* MeshHandle;
alias void* TimerHandle;
alias void* WindowListenerHandle;


// listener typedefs
alias int function(float,float,int) FrameListenerEvent;
alias void function(RenderWindowHandle) WindowListenerEvent;
alias void function(const char* message, int lml, int maskDebug, const char* log_name, int skip_message) LogListenerEvent;
alias void function(const char* message, int lml, int maskDebug, const char* log_name, int skip_message, void* userdata) LogListenerCtx;
alias int function(const ref FrameEvent evt, int frame_type, void* userdata) FrameListenerCtx;

struct coiQuaternion
{
    float w;
    float x;
    float y;
    float z;
}

struct coiVector3
{
    float x;
    float y;
    float z;
}

struct ViewPoint
{
    coiVector3 position;
    coiQuaternion orientation;
};

struct FrameEvent
{
    coiReal timeSinceLastEvent;
    coiReal timeSinceLastFrame;
}


struct ColourValue
{
    float r;
    float g;
    float b;
    float a;
}

struct engine_options
{
    const char* renderer_s;
    const char* plugin_folder_s;
    const char* window_title;
    const char* log_name;
    int width, height, auto_window;
};


struct FrameStats
{
    float lastFPS;
    float avgFPS;
    float bestFPS;
    float worstFPS;
    ulong bestFrameTime;
    ulong worstFrameTime;
    size_t triangleCount;
    size_t batchCount;
};


enum LoggingLevel
{
    LL_LOW = 1,
    LL_NORMAL = 2,
    LL_BOREME = 3
};

enum LogMessageLevel
{
    LML_TRIVIAL = 1,
    LML_NORMAL = 2,
    LML_CRITICAL = 3
};

enum stat_flags
{
    SF_NONE           = 0,
    SF_FPS            = 1,
    SF_AVG_FPS        = 2,
    SF_BEST_FPS       = 4,
    SF_WORST_FPS      = 8,
    SF_TRIANGLE_COUNT = 16,
    SF_ALL            = 0xFFFF
};

enum frame_buffer
{
    FB_FRONT,
    FB_BACK,
    FB_AUTO
};

enum scene_type
{
    ST_GENERIC = 1,
    ST_EXTERIOR_CLOSE = 2,
    ST_EXTERIOR_FAR = 4,
    ST_EXTERIOR_REAL_FAR = 8,
    ST_INTERIOR = 16
};

enum hardware_buffer_usage
{
    HBU_STATIC = 1,
    HBU_DYNAMIC = 2,
    HBU_WRITE_ONLY = 4,
    HBU_DISCARDABLE = 8,
    HBU_STATIC_WRITE_ONLY = 5, 
    HBU_DYNAMIC_WRITE_ONLY = 6,
    HBU_DYNAMIC_WRITE_ONLY_DISCARDABLE = 14
}

enum light_types
{
    /// Point light sources give off light equally in all directions, so require only position not direction
    LT_POINT = 0,
    /// Directional lights simulate parallel light beams from a distant source, hence have direction but no position
    LT_DIRECTIONAL = 1,
    /// Spotlights simulate a cone of light from a source so require position and direction, plus extra values for falloff
    LT_SPOTLIGHT = 2
};

enum transform_space
{
    TS_LOCAL,
    TS_PARENT,
    TS_WORLD
};



// Root functions
void release_engine();

void default_engine_options(engine_options* options);

void init_engine(const engine_options options);

RootHandle create_root(const char* pluginFileName, const char* configFileName, const char* logFileName);

RenderWindowHandle root_initialise(int auto_create_window, const char* render_window_title);

TimerHandle root_get_timer();

RenderWindowHandle create_render_window(const char* name, const int width, const int height, const int full_screen);

RenderWindowHandle create_render_window_gl_context(const char* name, const int width, const int height, const int full_screen);

RenderWindowHandle create_render_window_hwnd(const char* name, const int width, const int height, const int full_screen, ulong hwnd);

uint render_window_get_hwnd(RenderWindowHandle window_handle);

void render_window_set_visible(RenderWindowHandle window_handle, int visible);

void render_window_update(RenderWindowHandle window_handle, int swap_buffers);

void current_window_update(int swap_buffers);

void render_window_resize(uint width, uint height);

void render_window_moved_or_resized();

int render_window_closed();

int root_is_initialised();

void save_config();

int restore_config();

int show_config_dialog();

void load_ogre_plugin(const char * plugin);

// Doesn't use OgreManager. Can still throw if type_name doesn't exist.
SceneManagerHandle root_create_scene_manager(const char* type_name, const char* instance_name);

// Doesn't use OgreManager. If a specific scene manager is not found,
// the default implementation is always returned.
SceneManagerHandle root_create_scene_manager_by_mask(SceneTypeMask type_mask, const char* instance_name);

// Does use OgreManager.
SceneManagerHandle create_scene_manager(const char* type_name, const char* instance_name);

SceneManagerHandle get_scene_manager();

SceneManagerHandle get_scene_manager_by_name(const char* scene_manager_instance_name);


int render_one_frame();

int render_one_frame_ex(float time_since_last_frame);

void render_loop();

void pump_messages();

void log_message(const char* message);

RenderWindowHandle root_create_render_window(const char* name, uint width, uint height, int fullscreen, NameValuePairListHandle params);

RenderSystemListHandle root_get_available_renderers();

// Ogre::SceneManager calls

EntityHandle scenemanager_create_entity(SceneManagerHandle handle, const char* name, const char* mesh_name, const char* group_name);

SceneNodeHandle scenemanager_get_root_scene_node(SceneManagerHandle handle);

LightHandle scenemanager_create_light(SceneManagerHandle handle, const char* name);

void scenemanager_set_sky_box(SceneManagerHandle handle, int enable, const char* material_name, float distance,
                              int draw_first, const coiQuaternion* orientation,
                              const char* group_name);

void scenemanager_set_sky_dome(SceneManagerHandle handle, int enable, const char* material_name, float curvature,
                               float tiling, float distance, int draw_first, const coiQuaternion* orientation,
                               int xsegments, int ysegments, int ysegments_keep, const char* group_name);

// RenderSystem functions
void add_render_system(RenderSystemHandle render_system);

void set_render_system(RenderSystemHandle render_system);

RenderSystemHandle get_render_system();

RenderSystemHandle get_render_system_by_name(const char* render_system_name);

const(char*) render_system_get_name(RenderSystemHandle handle);

void render_system_set_config_option(RenderSystemHandle render_system_handle, const char* option, const char* value);

uint render_system_list_size(RenderSystemListHandle list_handle);

RenderSystemHandle render_system_list_get(RenderSystemListHandle list_handle, uint at);

void destroy_render_system_list(RenderSystemListHandle handle);


// SceneManager functions
void set_default_num_mipmaps(int number);

void set_ambient_light_rgba(const float r, const float g, const float b, const float a);

void set_ambient_light_rgb(const float r, const float g, const float b);

ViewportHandle add_viewport(CameraHandle camera_handle);

void scene_manager_log_name();

// Scene nodes
SceneNodeHandle create_child_scenenode(const char* node_name);

void attach_entity_to_scenenode(EntityHandle entity_handle, SceneNodeHandle scenenode_handle);

void scenenode_update(SceneNodeHandle scenenode_handle, int update_children, int parent_has_changed);

void scenenode_update_bounds(SceneNodeHandle scenenode_handle);

EntityHandle scenenode_get_attached_entity_int(SceneNodeHandle scenenode_handle, int entity_index);

EntityHandle scenenode_get_attached_entity(SceneNodeHandle scenenode_handle, const char* entity_name);

int scenenode_num_attached_objects(SceneNodeHandle scenenode_handle);

void scenenode_detach_entity_int(SceneNodeHandle scenenode_handle, int entity_index);

void scenenode_detach_entity(SceneNodeHandle scenenode_handle, EntityHandle entity_handle);

void scenenode_detach_entity_string(SceneNodeHandle scenenode_handle, const char* entity_name);

void scenenode_detach_all_objects(SceneNodeHandle scenenode_handle);

int scenenode_is_in_scenegraph(SceneNodeHandle scenenode_handle);

void scenenode_notify_rootnode(SceneNodeHandle scenenode_handle);

void scenenode_show_boundingbox(SceneNodeHandle scenenode_handle, int show_boundingbox);

void scenenode_hide_boundingbox(SceneNodeHandle scenenode_handle, int hide_boundingbox);

int scenenode_get_show_boundingbox(SceneNodeHandle scenenode_handle);

SceneNodeHandle scenenode_get_parent_scenenode(SceneNodeHandle scenenode_handle);

void scenenode_set_visible(SceneNodeHandle scenenode_handle, int visible);

void scenenode_set_visible_ex(SceneNodeHandle scenenode_handle, int visible, int cascade);

void scenenode_flip_visibility(SceneNodeHandle scenenode_handle);

void scenenode_flip_visibility_ex(SceneNodeHandle scenenode_handle, int cascade);

void scenenode_set_debug_display_enabled(SceneNodeHandle scenenode_handle, int enabled);

void scenenode_set_debug_display_enabled_ex(SceneNodeHandle scenenode_handle, int enabled, int cascade);

SceneManagerHandle scenenode_get_creator(SceneNodeHandle scenenode_handle);

void scenenode_set_direction(SceneNodeHandle scenenode_handle, float x, float y, float z);

void scenenode_set_orientation(SceneNodeHandle scenenode_handle, float w, float x, float y, float z);

void scenenode_set_position(SceneNodeHandle scenenode_handle, float x, float y, float z);

void scenenode_set_derived_position(SceneNodeHandle handle, const coiVector3* pos);

void scenenode_get_derived_position(SceneNodeHandle handle, coiVector3* pos);

void scenenode_yaw_degree(SceneNodeHandle handle, coiReal angle);

void scenenode_yaw(SceneNodeHandle scenenode_handle, coiReal radians);

void scenenode_set_scale(SceneNodeHandle scenenode_handle, float x, float y, float z);

void scenenode_scale(SceneNodeHandle scenenode_handle, float x, float y, float z);

void scenenode_translate(SceneNodeHandle scenenode_handle, float x, float y, float z);

void scenenode_roll(SceneNodeHandle scenenode_handle, coiReal radians);

void scenenode_pitch(SceneNodeHandle scenenode_handle, coiReal radians);

SceneNodeHandle scenenode_create_child_scenenode(SceneNodeHandle handle, const char* name, const ref coiVector3 translate, const ref coiQuaternion rotate);

// Viewports
void viewport_set_background_colour(ViewportHandle viewport_handle, float r, float g, float b, float a);

void viewport_set_background_colour_cv(ViewportHandle viewport_handle, ref ColourValue cv);

void viewport_set_auto_updated(ViewportHandle handle, int autoupdate);

int viewport_is_auto_updated(ViewportHandle handle);

float viewport_get_top(ViewportHandle handle);

float viewport_get_left(ViewportHandle handle);

float viewport_get_width(ViewportHandle viewport_handle);

float viewport_get_height(ViewportHandle viewport_handle);

int viewport_get_actual_top(ViewportHandle handle);

int viewport_get_actual_left(ViewportHandle handle);

int viewport_get_actual_width(ViewportHandle handle);

int viewport_get_actual_height(ViewportHandle handle);

// Resource management
void setup_resources(const char* resources_cfg);

void add_resource_location(const char* location, const char* type, const char* group);

void initialise_all_resourcegroups();

const(char*) resourcegroupmanager_DEFAULT_RESOURCE_GROUP_NAME();

const(char*) resourcegroupmanager_INTERNAL_RESOURCE_GROUP_NAME();

const(char*)  resourcegroupmanager_AUTODETECT_RESOURCE_GROUP_NAME();

size_t resourcegroupmanager_RESOURCE_SYSTEM_NUM_REFERENCE_COUNTS();

// Camera
CameraHandle create_camera(const char* camera_name);

CameraHandle get_camera(const char* camera_name);

void camera_move(CameraHandle handle, const float x, const float y, const float z);

void camera_move_relative(CameraHandle handle, const float x, const float y, const float z);

void camera_set_direction(CameraHandle handle, const float x, const float y, const float z);

void camera_get_direction(CameraHandle handle, coiVector3* v3);

void camera_set_near_clip_distance(CameraHandle camera_handle, float d);

void camera_set_far_clip_distance(CameraHandle camera_handle, float d);

void camera_set_aspect_ratio(CameraHandle camera_handle, float w, float h);

void camera_set_aspect_ratio_ex(CameraHandle handle, float ratio);

float camera_get_aspect_ratio(CameraHandle handle);

void camera_set_auto_aspect_ratio(CameraHandle camera_handle, int on);

void camera_set_fovy(CameraHandle camera_handle, float angle);

void camera_set_frustum_offset(CameraHandle camera_handle, const int offset_x, const int offset_y);

void camera_set_focal_length(CameraHandle camera_handle, float fl);

void camera_set_position(CameraHandle camera_handle, const float x, const float y, const float z);

void camera_get_position(CameraHandle handle, ref coiVector3 result);

void camera_lookat(CameraHandle camera_handle, const float x, const float y, const float z);


// Entity
EntityHandle create_entity(const char* entity_name, const char* mesh_file);

void entity_set_cast_shadows(EntityHandle handle, int enabled);

int entity_get_cast_shadows(EntityHandle handle);

int entity_get_receives_shadows(EntityHandle handle);

void entity_set_material_name(EntityHandle handle, const char* material_name, const char* group_name);


// Light
LightHandle create_light(const char* light_name);

void light_set_position(LightHandle light_handle, const float x, const float y, const float z);

void destroy_light(LightHandle handle);

void light_set_type(LightHandle handle, light_types type);

void light_set_diffuse_colour(LightHandle handle, const ref ColourValue colour);

void light_set_specular_colour(LightHandle handle, const ref ColourValue colour);

// FrameListener
void add_frame_listener(FrameListenerEvent frame_event,const int frame_event_type);

void remove_frame_listener(FrameListenerEvent frame_event);

FrameListenerHandle add_frame_listener_ctx(FrameListenerCtx callback, void* userdata);

void remove_frame_listener_ctx(FrameListenerHandle handle);


// WindowListener
void add_window_listener(RenderWindowHandle window_handle, WindowListenerEvent window_event);

void remove_window_listener(RenderWindowHandle window_handle);

WindowListenerHandle add_window_listener_ctx(RenderWindowHandle window_handle, WindowListenerEvent window_event, void* userdata);
   
void remove_window_listener_ctx(RenderWindowHandle window_handle, WindowListenerHandle listener_handle);

// LogManager
LogManagerHandle create_log_manager();

// LogManager::getSingletonPtr
LogManagerHandle get_log_manager();

//LogManager::getLog
LogHandle logmanager_get_log(const char* name);

//LogManager::getDefaultLog
LogHandle logmanager_get_default_log();

//LogManager::setDefaultLog
LogHandle logmanager_set_default_log(LogHandle log_handle);

//LogManager::createLog
LogHandle logmanager_create_log(const char* name, int default_log, int debugger_output, int suppress_file_output);

// n.b., Allows for finer grained control over the log messages at the cost of
// having to supply all these variables. If you don't need this control,
// use log_message above.
//LogManager::logMessage
void logmanager_log_message(const char* message, LogMessageLevel lml, int maskDebug, const char* log_name, int skip_message);

//LogManager::setLogDetail
void logmanager_set_log_detail(LoggingLevel lvl);

//LogManager::destroyLog
void logmanager_destroy_log(const char* name);

//LogManager::destroyLog overload
void logmanager_destroy_log_by_handle(LogHandle log_handle);

//Log::addListener
LogListenerHandle add_log_listener(LogListenerEvent log_event, LogHandle log_handle);

//Log::addListener
LogListenerHandle add_log_listener_ctx(LogListenerCtx log_event, LogHandle log_handle, void* userdata);

//Log::removeListener
void remove_log_listener(LogListenerHandle llh, LogHandle log_handle);

//Log::removeListener
void remove_log_listener_ctx(LogListenerHandle llh, LogHandle log_handle);


// NameValuePairList 
NameValuePairListHandle create_name_value_pair_list();
void add_pair(NameValuePairListHandle params, const char* name, const char* value);
void destroy_name_value_pair_list(NameValuePairListHandle params);

// RenderWindow
ViewportHandle render_window_add_viewport(RenderWindowHandle window_handle, CameraHandle camera_handle, int zorder, float left, float top, float width, float height);

int render_window_is_closed(RenderWindowHandle handle);

void render_window_set_active(RenderWindowHandle handle, int state);

void render_window_swap_buffers(RenderWindowHandle handle, int wait_for_vsync);

void render_window_get_custom_attribute(RenderWindowHandle handle, const char* attribute, void* pdata);

uint render_window_get_width(RenderWindowHandle handle);

uint render_window_get_height(RenderWindowHandle handle);

// ColourValue
void colourvalue_zero(ref ColourValue c);
void colourvalue_black(ref ColourValue c);
void colourvalue_white(ref ColourValue c);
void colourvalue_red(ref ColourValue c);
void colourvalue_green(ref ColourValue c);
void colourvalue_blue(ref ColourValue c);

// Vector3
//Vector3::operator !=
int vector3_notequals_vector3(coiVector3 lhs, coiVector3 rhs);

//Vector3::operator ==
int vector3_equals_vector3(coiVector3 lhs, coiVector3 rhs);

//Vector3::operator +
coiVector3 vector3_add_vector3(coiVector3 lhs, coiVector3 rhs);

//Vector3::operator +=
void vector3_update_add_vector3(coiVector3 lhs, coiVector3 rhs);

//Vector3::operator -
coiVector3 vector3_subtract_vector3(coiVector3 lhs, coiVector3 rhs);

//Vector3::operator -=
void vector3_update_subtract_vector3(coiVector3 lhs, coiVector3 rhs);

//Vector3::operator - 
coiVector3 vector3_negate(coiVector3 v3);

// Vector3::operator/ 
coiVector3 vector3_divide_vector3(coiVector3 lhs, coiVector3 rhs);

// Vector3::operator*
coiVector3 vector3_multiply_vector3(coiVector3 lhs, coiVector3 rhs);

// Vector3::isNaN
int vector3_is_nan(coiVector3 v3);

//Vector3::primaryAxis
coiVector3 vector3_primary_axis(coiVector3);

// Vector3::ZERO
coiVector3 vector3_ZERO();
coiVector3 vector3_UNIT_X();
coiVector3 vector3_UNIT_Y();
coiVector3 vector3_UNIT_Z();
coiVector3 vector3_NEGATIVE_UNIT_X();
coiVector3 vector3_NEGATIVE_UNIT_Y();
coiVector3 vector3_NEGATIVE_UNIT_Z();
coiVector3 vector3_UNIT_SCALE();

// Plane

PlaneHandle plane_create_plane();
PlaneHandle plane_create_plane_normal(float x, float y, float z, float distance);
void plane_destroy_plane(PlaneHandle handle);
void plane_get_normal(PlaneHandle handle, coiVector3* normal);
void plane_set_normal(PlaneHandle handle, const coiVector3* normal);
coiReal plane_get_d(PlaneHandle handle);
void plane_set_d(PlaneHandle handle, coiReal d);

// MeshManager

MeshHandle meshmanager_create_plane(const char* name, const char* group_name,
                                    PlaneHandle plane, float width,
                                    float height, int xsegments, int ysegments,
                                    int normals, ushort num_tex_coord_sets,
                                    float utile, float vtile, ref coiVector3 up_vector,
                                    hardware_buffer_usage vertex_buffer_usage,
                                    hardware_buffer_usage index_buffer_usage,
                                    int vertex_shadow_buffer, int index_shadow_buffer);

// Ogre::Timer
int timer_set_option(TimerHandle handle, const char* key, void* value);

ulong timer_get_milliseconds(TimerHandle handle);

ulong timer_get_microseconds(TimerHandle handle);

ulong timer_get_milliseconds_cpu(TimerHandle handle);

ulong timer_get_microseconds_cpu(TimerHandle handle);

void timer_reset(TimerHandle handle);

