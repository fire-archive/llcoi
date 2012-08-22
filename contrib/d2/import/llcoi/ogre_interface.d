/******************************************************************************
 * ogre_interface.d - main interface file for D clients
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
private import core.stdc.config : c_long, c_ulong;

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
alias void* NodeHandle;
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
alias void* PlaneListHandle;
alias void* PlaneBoundedVolumeHandle;
alias void* MeshHandle;
alias void* TimerHandle;
alias void* WindowListenerHandle;
alias void* AxisAlignedBoxHandle;
alias void* RayHandle;
alias void* SphereHandle;
alias void* SceneQueryHandle;
alias void* RaySceneQueryHandle;
alias void* RaySceneQueryResultHandle;
alias void* SceneQueryListenerHandle;
alias void* RaySceneQueryListenerHandle;
alias void* SceneQueryResultHandle;
alias void* MovableObjectHandle;
alias void* RenderOperationHandle;
alias void* OverlayHandle;
alias void* OverlayManagerHandle;
alias void* OverlayElementHandle;
alias void* OverlayContainerHandle;



// listener typedefs
alias int function(float,float,int) FrameListenerEvent;
alias void function(RenderWindowHandle) WindowListenerEvent;
alias void function(const char* message, int lml, int maskDebug, const char* log_name, int skip_message) LogListenerEvent;
alias void function(const char* message, int lml, int maskDebug, const char* log_name, int skip_message, void* userdata) LogListenerCtx;
alias int function(const ref FrameEvent evt, int frame_type, void* userdata) FrameListenerCtx;

alias int function(const ref world_fragment frag, void* userdata) SceneQueryFragmentResult;
alias int function(MovableObjectHandle handle, void* userdata) SceneQueryObjectResult;
alias int function(const ref world_fragment frag, coiReal distance, void* userdata) RaySceneQueryFragmentResult;
alias int function(MovableObjectHandle handle, coiReal distance, void* userdata) RaySceneQueryObjectResult;

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

struct coiMatrix3
{
    coiReal m[3][3];
}

struct coiMatrix4
{
    coiReal m[4][4];
}

struct ViewPoint
{
    coiVector3 position;
    coiQuaternion orientation;
}

struct FrameEvent
{
    coiReal timeSinceLastEvent;
    coiReal timeSinceLastFrame;
}

struct ray_pair
{
    int intersects;
    coiReal distance;
}


struct coiColourValue
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
    c_ulong bestFrameTime;
    c_ulong worstFrameTime;
    size_t triangleCount;
    size_t batchCount;
};

struct world_fragment
{
    world_fragment_type fragment_type;
    coiVector3 single_intersection;
    PlaneListHandle planes;
    void* geometry;
    RenderOperationHandle render_op;
}

struct rayscenequery_result_entry
{
    coiReal distance;
    MovableObjectHandle movable;
    world_fragment* fragment;
}


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

enum orientation_mode
{
    OR_DEGREE_0       = 0,
    OR_DEGREE_90      = 1,
    OR_DEGREE_180     = 2,
    OR_DEGREE_270     = 3,

    OR_PORTRAIT       = OR_DEGREE_0,
    OR_LANDSCAPERIGHT = OR_DEGREE_90,
    OR_LANDSCAPELEFT  = OR_DEGREE_270
}

enum projection_type
{
    PT_ORTHOGRAPHIC,
    PT_PERSPECTIVE
}

enum frustum_plane
{
    FRUSTUM_PLANE_NEAR   = 0,
    FRUSTUM_PLANE_FAR    = 1,
    FRUSTUM_PLANE_LEFT   = 2,
    FRUSTUM_PLANE_RIGHT  = 3,
    FRUSTUM_PLANE_TOP    = 4,
    FRUSTUM_PLANE_BOTTOM = 5
}

enum stat_flags
{
    SF_NONE           = 0,
    SF_FPS            = 1,
    SF_AVG_FPS        = 2,
    SF_BEST_FPS       = 4,
    SF_WORST_FPS      = 8,
    SF_TRIANGLE_COUNT = 16,
    SF_ALL            = 0xFFFF
}

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

enum Extent
{
    EXTENT_NULL,
    EXTENT_FINITE,
    EXTENT_INFINITE
};

enum CornerEnum
{
    FAR_LEFT_BOTTOM = 0,
    FAR_LEFT_TOP = 1,
    FAR_RIGHT_TOP = 2,
    FAR_RIGHT_BOTTOM = 3,
    NEAR_RIGHT_BOTTOM = 7,
    NEAR_LEFT_BOTTOM = 6,
    NEAR_LEFT_TOP = 5,
    NEAR_RIGHT_TOP = 4
};

enum plane_side
{
    NO_SIDE,
    POSITIVE_SIDE,
    NEGATIVE_SIDE,
    BOTH_SIDE
};

enum world_fragment_type
{
    /// Return no world geometry hits at all
    WFT_NONE,
    /// Return pointers to convex plane-bounded regions
    WFT_PLANE_BOUNDED_REGION,
    /// Return a single intersection point (typically RaySceneQuery only)
    WFT_SINGLE_INTERSECTION,
    /// Custom geometry as defined by the SceneManager
    WFT_CUSTOM_GEOMETRY,
    /// General RenderOperation structure
    WFT_RENDER_OPERATION
};

enum gui_metrics_mode
{
    GMM_RELATIVE,
    GMM_PIXELS,
    GMM_RELATIVE_ASPECT_ADJUSTED
};

enum gui_horizontal_alignment
{
    GHA_LEFT,
    GHA_CENTER,
    GHA_RIGHT
};

enum gui_vertical_alignment
{
    GVA_TOP,
    GVA_CENTER,
    GVA_BOTTOM
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

RenderWindowHandle create_render_window_hwnd(const char* name, const int width, const int height, const int full_screen, c_ulong hwnd);

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

void load_ogre_plugin(const char* plugin);

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


const(char*) scenemanager_get_name(SceneManagerHandle handle);

//void SceneManager::destroyQuery(Ogre::SceneQuery* query);
void scenemanager_destroy_scenequery(SceneManagerHandle handle, SceneQueryHandle query);
// Ogre::SceneManager::createRayQuery(Ogre::Ray const&, unsigned long)
RaySceneQueryHandle scenemanager_create_rayquery(SceneQueryHandle handle, RayHandle ray_handle, c_ulong mask);


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

// Ogre::Node
//Ogre::Node::getName() const
const(char*) node_get_name(NodeHandle handle);
//Ogre::Node::getParent() const
//XXX: May be NULL if this is the root node.
NodeHandle node_get_parent(NodeHandle handle);
//Ogre::Node::getOrientation() const
void node_get_orientation(NodeHandle handle, ref coiQuaternion result);
//Ogre::Node::setOrientation(Ogre::Quaternion const&)
void node_set_orientation(NodeHandle handle, const ref coiQuaternion orientation);
//Ogre::Node::setScale(Ogre::Vector3 const&)
void node_set_scale(NodeHandle handle, const ref coiVector3 in_scale);
//Ogre::Node::setScale(Ogre::Vector3 const&)
void node_set_scale_xyz(NodeHandle handle, const float x, const float y, const float z);
//Ogre::Node::getScale() const
void node_get_scale(NodeHandle handle, ref coiVector3 result);
//Ogre::Node::setInheritOrientation(bool)
void node_set_inherit_orientation(NodeHandle handle, int inherit);
//Ogre::Node::getInheritOrientation() const
int node_get_inherit_orientation(NodeHandle handle);
//Ogre::Node::resetOrientation()
void node_reset_orientation(NodeHandle handle);
//Ogre::Node::setInheritScale(bool)
void node_set_inherit_scale(NodeHandle handle, int inherit);
//Ogre::Node::getInheritScale() const
int node_get_inherit_scale(NodeHandle handle);
//Ogre::Node::scale(Ogre::Vector3 const&)
void node_scale(NodeHandle handle, const ref coiVector3 scale);
//Ogre::Node::scale(Ogre::Vector3 const&)
void node_scale_xyz(NodeHandle handle, const float x, const float y, const float z);
//Ogre::Node::translate(Ogre::Vector3 const&, Ogre::Node::TransformSpace)
void node_translate(NodeHandle handle, const ref coiVector3 d, transform_space relative_to);
//Ogre::Node::translate(Ogre::Vector3 const&, Ogre::Node::TransformSpace)
void node_translate_xyz(NodeHandle handle, const float x, const float y, const float z, transform_space relative_to);
//Ogre::Node::translate(Ogre::Matrix3 const&, float, float, float, Ogre::Node::TransformSpace)
//Ogre::Node::translate(Ogre::Matrix3 const&, Ogre::Vector3 const&, Ogre::Node::TransformSpace)
void node_translate_m(NodeHandle handle, const ref coiMatrix3 axes, const ref coiVector3 move, transform_space relative_to);
//Ogre::Node::roll(Ogre::Radian const&, Ogre::Node::TransformSpace)
void node_roll(NodeHandle handle, const coiReal angle, transform_space relative_to);
//Ogre::Node::pitch(Ogre::Radian const&, Ogre::Node::TransformSpace)
void node_pitch(NodeHandle handle, const coiReal angle, transform_space relative_to);
// Ogre::Node::yaw(Ogre::Radian const&, Ogre::Node::TransformSpace)
void node_yaw(NodeHandle handle, const coiReal angle, transform_space relative_to);
//Ogre::Node::rotate(Ogre::Vector3 const&, Ogre::Radian const&, Ogre::Node::TransformSpace)
void node_rotate(NodeHandle handle, const ref coiVector3 axis, const coiReal angle, transform_space relative_to);
//Ogre::Node::rotate(Ogre::Quaternion const&, Ogre::Node::TransformSpace)
void node_rotate_q(NodeHandle handle, const ref coiQuaternion q, transform_space relative_to);
//Ogre::Node::getLocalAxes() const
void node_get_local_axes(NodeHandle handle, ref coiMatrix3 result);
//Ogre::Node::createChild(Ogre::Vector3 const&, Ogre::Quaternion const&)
NodeHandle node_create_child(NodeHandle handle, const ref coiVector3 translate, const ref coiQuaternion rotate);
//Ogre::Node::createChild(std::string const&, Ogre::Vector3 const&, Ogre::Quaternion const&)
NodeHandle node_create_named_child(NodeHandle handle, const char* name, const ref coiVector3 translate, const ref coiQuaternion rotate);
//Ogre::Node::addChild(Ogre::Node*)
void node_add_child(NodeHandle handle, NodeHandle child);
//Ogre::Node::numChildren() const
ushort node_num_children(NodeHandle handle);
//Ogre::Node::getChild(unsigned short) const
NodeHandle node_get_child_by_index(NodeHandle handle, ushort index);
//Ogre::Node::getChild(std::string const&) const
NodeHandle node_get_child_by_name(NodeHandle handle, const char* name);


// Ogre::SceneNode
SceneNodeHandle create_child_scenenode(const char* node_name);

void attach_entity_to_scenenode(EntityHandle entity_handle, SceneNodeHandle scenenode_handle);

void scenenode_update(SceneNodeHandle scenenode_handle, int update_children, int parent_has_changed);

void scenenode_update_bounds(SceneNodeHandle scenenode_handle);

EntityHandle scenenode_get_attached_entity_int(SceneNodeHandle scenenode_handle, int entity_index);

EntityHandle scenenode_get_attached_entity(SceneNodeHandle scenenode_handle, const char* entity_name);

ushort scenenode_num_attached_objects(SceneNodeHandle scenenode_handle);

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

void scenenode_set_direction(SceneNodeHandle scenenode_handle, float x, float y, float z, transform_space relative_to);

void scenenode_set_orientation(SceneNodeHandle scenenode_handle, float w, float x, float y, float z);

void scenenode_set_position(SceneNodeHandle scenenode_handle, float x, float y, float z);

void scenenode_get_position(SceneNodeHandle handle, ref coiVector3 pos);

void scenenode_set_derived_position(SceneNodeHandle handle, const ref coiVector3 pos);

void scenenode_get_derived_position(SceneNodeHandle handle, ref coiVector3 pos);
//void setFixedYawAxis( bool useFixed, const Vector3& fixedAxis = Vector3::UNIT_Y );
void scenenode_set_fixed_yaw_axis(SceneNodeHandle handle, int use_fixed, const ref coiVector3 fixed_axis);

void scenenode_yaw_degree(SceneNodeHandle handle, coiReal angle, transform_space relative_to);

void scenenode_yaw(SceneNodeHandle scenenode_handle, coiReal radians, transform_space relative_to);

void scenenode_set_scale(SceneNodeHandle scenenode_handle, float x, float y, float z);

void scenenode_scale(SceneNodeHandle scenenode_handle, float x, float y, float z);

void scenenode_translate(SceneNodeHandle scenenode_handle, float x, float y, float z, transform_space relative_to);

void scenenode_roll(SceneNodeHandle scenenode_handle, coiReal radians, transform_space relative_to);

void scenenode_pitch(SceneNodeHandle scenenode_handle, coiReal radians, transform_space relative_to);

SceneNodeHandle scenenode_create_child_scenenode(SceneNodeHandle handle, const char* name, const ref coiVector3 translate, const ref coiQuaternion rotate);

// Viewports
void viewport_set_background_colour(ViewportHandle viewport_handle, float r, float g, float b, float a);

void viewport_set_background_colour_cv(ViewportHandle viewport_handle, ref coiColourValue cv);

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

//Ogre::Viewport::setDimensions(float, float, float, float)
void viewport_set_dimensions(ViewportHandle handle, coiReal left, coiReal top, coiReal width, coiReal height);
//Ogre::Viewport::getActualDimensions(int&, int&, int&, int&) const
void viewport_get_actual_dimensions(ViewportHandle handle, ref int left, ref int top, ref int width, ref int height);
//Ogre::Viewport::getBackgroundColour() const
void viewport_get_background_colour(ViewportHandle handle, ref coiColourValue cv);


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

void camera_get_direction(CameraHandle handle, ref coiVector3 v3);

void camera_get_up(CameraHandle handle, ref coiVector3 up);

void camera_get_right(CameraHandle handle, ref coiVector3 right);

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

void camera_roll(CameraHandle handle, coiReal angle);

void camera_yaw(CameraHandle handle, coiReal angle);

void camera_pitch(CameraHandle handle, coiReal angle);

void camera_rotate(CameraHandle handle, const ref coiVector3 axis, coiReal angle);

void camera_rotate_q(CameraHandle handle, const ref coiQuaternion q);

//Ogre::Camera::setFixedYawAxis(bool, Ogre::Vector3 const&)
void camera_set_fixed_yaw_axis(CameraHandle handle, int on, const ref coiVector3 fixed_axis);
//Ogre::Camera::getOrientation() const
void camera_get_orientation(CameraHandle handle, ref coiQuaternion orientation);
//Ogre::Camera::setOrientation(Ogre::Quaternion const&)
void camera_set_orientation(CameraHandle handle, const ref coiQuaternion orientation);
//Ogre::Camera::getDerivedOrientation() const
void camera_get_derived_orientation(CameraHandle handle, ref coiQuaternion orientation);
//Ogre::Camera::getDerivedPosition() const
void camera_get_derived_position(CameraHandle handle, ref coiVector3 position);
//Ogre::Camera::getDerivedDirection() const
void camera_get_derived_direction(CameraHandle handle, ref coiVector3 direction);
//Ogre::Camera::getDerivedUp() const
void camera_get_derived_up(CameraHandle handle, ref coiVector3 up);
//Ogre::Camera::getDerivedRight() const
void camera_get_derived_right(CameraHandle handle, ref coiVector3 right);
//Ogre::Camera::setAutoTracking(bool, Ogre::SceneNode*, Ogre::Vector3 const&)
void camera_set_autotracking(CameraHandle handle, int on, SceneNodeHandle sn_handle, const ref coiVector3 offset);
//Ogre::Camera::setLodBias(float)
void camera_set_lod_bias(CameraHandle handle, coiReal factor);
//Ogre::Camera::getLodBias() const
coiReal camera_get_lod_bias(CameraHandle handle);
//Ogre::Camera::getCameraToViewportRay(float, float, Ogre::Ray*) const
void camera_get_camera_to_viewport_ray(CameraHandle handle, coiReal screenx, coiReal screeny, RayHandle ray);
//Ogre::Camera::setWindow(float, float, float, float)
void camera_set_window(CameraHandle handle, coiReal left, coiReal top, coiReal right, coiReal bottom);

SceneManagerHandle camera_get_scenemanager(CameraHandle handle);



// Entity
EntityHandle create_entity(const char* entity_name, const char* mesh_file);

void entity_set_cast_shadows(EntityHandle handle, int enabled);

int entity_get_cast_shadows(EntityHandle handle);

int entity_get_receives_shadows(EntityHandle handle);

void entity_set_material_name(EntityHandle handle, const char* material_name, const char* group_name);
//Ogre::Entity::getBoundingBox() const
AxisAlignedBoxHandle entity_get_bounding_box(EntityHandle handle);
//Ogre::Entity::getBoundingRadius() const
coiReal entity_get_bounding_radius(EntityHandle handle);

// Light
LightHandle create_light(const char* light_name);

void destroy_light(LightHandle handle);

void light_set_position(LightHandle light_handle, const float x, const float y, const float z);

//Ogre::Light::getPosition() const
void light_get_position(LightHandle handle, ref coiVector3 pos);
//Ogre::Light::getPosition() const
void light_get_position_xyz(LightHandle handle, ref float x, ref float y, ref float z);
//Ogre::Light::getPosition() const
void light_get_position_xyz(LightHandle handle, float* x, float* y, float* z);
//Ogre::Light::setDirection(float, float, float)
void light_set_direction_xyz(LightHandle handle, const float x, const float y, const float z);
//Ogre::Light::setDirection(Ogre::Vector3 const&)
void light_set_direction(LightHandle handle, const ref coiVector3 direction);
//Ogre::Light::getDirection() const
void light_get_direction(LightHandle handle, ref coiVector3 direction);
//Ogre::Light::setSpotlightRange(Ogre::Radian const&, Ogre::Radian const&, float)
void light_set_spotlight_range(LightHandle handle, const coiReal inner_angle, const coiReal outer_angle, coiReal fall_off);


void light_set_type(LightHandle handle, light_types type);

void light_set_diffuse_colour(LightHandle handle, const ref coiColourValue colour);

void light_set_specular_colour(LightHandle handle, const ref coiColourValue colour);



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

void renderwindow_get_statistics(RenderWindowHandle handle, ref FrameStats stats);

void renderwindow_get_statistics_ex(RenderWindowHandle handle, ref float lastFPS, ref float avgFPS, ref float bestFPS, ref float worstFPS);

// ColourValue
void colourvalue_zero(ref coiColourValue c);
void colourvalue_black(ref coiColourValue c);
void colourvalue_white(ref coiColourValue c);
void colourvalue_red(ref coiColourValue c);
void colourvalue_green(ref coiColourValue c);
void colourvalue_blue(ref coiColourValue c);

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
void plane_get_normal(PlaneHandle handle, ref coiVector3 normal);
void plane_set_normal(PlaneHandle handle, const ref coiVector3 normal);
coiReal plane_get_d(PlaneHandle handle);
void plane_set_d(PlaneHandle handle, coiReal d);

// PlaneList (typedef vector<Plane>::type PlaneList)
PlaneListHandle create_planelist();
void destroy_planelist(PlaneListHandle handle);



// PlaneBoundedVolume
PlaneBoundedVolumeHandle create_planeboundedvolume(plane_side the_outside);
void destroy_planeboundedvolume(PlaneBoundedVolumeHandle handle);
// bool intersects(const AxisAlignedBox&) const
int planeboundedvolume_intersects_axisalignedbox(PlaneBoundedVolumeHandle handle, AxisAlignedBoxHandle query);
// bool intersects(const Sphere&) const
int planeboundedvolume_intersects_sphere(PlaneBoundedVolumeHandle handle, SphereHandle query);
// std::pair<bool, Real> intersects(const Ray&) const
void planeboundedvolume_intersects_ray(PlaneBoundedVolumeHandle handle, RayHandle query, ref ray_pair result);

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

c_ulong timer_get_milliseconds(TimerHandle handle);

c_ulong timer_get_microseconds(TimerHandle handle);

c_ulong timer_get_milliseconds_cpu(TimerHandle handle);

c_ulong timer_get_microseconds_cpu(TimerHandle handle);

void timer_reset(TimerHandle handle);

// Ogre::AxisAlignedBox
AxisAlignedBoxHandle create_axis_aligned_box();
AxisAlignedBoxHandle create_axis_aligned_box_ex(Extent e);
AxisAlignedBoxHandle create_axis_aligned_box_v3(const ref coiVector3 min, const ref coiVector3 max);
void destroy_axis_aligned_box(AxisAlignedBoxHandle handle);
void axisalignedbox_get_size(AxisAlignedBoxHandle handle, ref coiVector3 size);
void axisalignedbox_get_minimum(AxisAlignedBoxHandle handle, ref coiVector3 minimum);
void axisalignedbox_get_maximum(AxisAlignedBoxHandle handle, ref coiVector3 maximum);
void axisalignedbox_set_minimum_x(AxisAlignedBoxHandle handle, coiReal x);
void axisalignedbox_set_minimum_y(AxisAlignedBoxHandle handle, coiReal y);
void axisalignedbox_set_minimum_z(AxisAlignedBoxHandle handle, coiReal z);
void axisalignedbox_set_minimum(AxisAlignedBoxHandle handle, const ref coiVector3 min);
void axisalignedbox_set_maximum(AxisAlignedBoxHandle handle, const ref coiVector3 max);
void axisalignedbox_set_maximum_x(AxisAlignedBoxHandle handle, coiReal x);
void axisalignedbox_set_maximum_y(AxisAlignedBoxHandle handle, coiReal y);
void axisalignedbox_set_maximum_z(AxisAlignedBoxHandle handle, coiReal z);
void axisalignedbox_set_extents(AxisAlignedBoxHandle handle, const ref coiVector3 min, const ref coiVector3 max);
void axisalignedbox_get_corner(AxisAlignedBoxHandle handle, CornerEnum e, ref coiVector3 corner);

//Ogre::Ray
RayHandle create_ray(const ref coiVector3 origin, const ref coiVector3 direction);
void destroy_ray(RayHandle handle);
//Ray::setOrigin
void ray_set_origin(RayHandle handle, const ref coiVector3 origin);
//Ray::getOrigin
void ray_get_origin(RayHandle handle, ref coiVector3 origin);
//Ray::setDirection
void ray_set_direction(RayHandle handle, const ref coiVector3 direction);
//Ray::getDirection
void ray_get_direction(RayHandle handle, ref coiVector3 direction);
//Ray::getPoint
void ray_get_point(RayHandle handle, coiReal units, ref coiVector3 point);
//Ray::intersects(Plane)
void ray_intersects_plane(RayHandle handle, PlaneHandle plane_handle, ref ray_pair result);
//Ray::intersects(AxisAlignedBox)
void ray_intersects_axisalignedbox(RayHandle handle, AxisAlignedBoxHandle query_handle, ref ray_pair result);
//Ray::intersects(Sphere)
void ray_intersects_sphere(RayHandle handle, SphereHandle query_handle, ref ray_pair result);


// Ogre::Sphere
SphereHandle create_sphere(const ref coiVector3 center, coiReal radius);
void destroy_sphere(SphereHandle handle);
//void setRadius(Real)
void sphere_set_radius(SphereHandle handle, coiReal radius);
//Real getRadius(void) const
coiReal sphere_get_radius(SphereHandle handle);
//void setCenter(Vector3)
void sphere_set_center(SphereHandle handle, const ref coiVector3 center);
//Real getCenter(void) const
void sphere_get_center(SphereHandle handle, ref coiVector3 center);
// bool intersects(const Sphere&) const
int sphere_intersects_sphere(SphereHandle handle, SphereHandle query);
// bool intersects(const AxisAlignedBox&) const
int sphere_intersects_axisalignedbox(SphereHandle handle, AxisAlignedBoxHandle query);
// bool intersects(const Plane&) const
int sphere_intersects_plane(SphereHandle handle, PlaneHandle query);
// bool intersects(const Vector3&) const
int sphere_intersects_vector3(SphereHandle handle, const ref coiVector3 query);
// void merge(const Sphere&)
void sphere_merge(SphereHandle handle, SphereHandle other_sphere);

// Ogre::SceneQuery
// SceneQuery::setQueryMask(uint32 mask)
void scenequery_set_query_mask(SceneQueryHandle handle, uint32 mask);
//uint32 SceneQuery::getQueryMask(void) const
uint32 scenequery_get_query_mask(SceneQueryHandle handle);

//void SceneQuery::setWorldFragmentType(enum WorldFragmentType wft);
void scenequery_set_world_fragment_type(SceneQueryHandle handle, world_fragment_type wft);
//WorldFragmentType SceneQuery::getWorldFragmentType(void) const;
world_fragment_type scenequery_get_world_fragment_type(SceneQueryHandle handle);


// SceneQueryListener
SceneQueryListenerHandle create_scenequerylistener(SceneQueryFragmentResult fragment_callback, SceneQueryObjectResult object_callback, void* userdata);
void destroy_scenequerylistener(SceneQueryListenerHandle handle);

size_t scenequeryresult_movables_count(SceneQueryResultHandle handle);
MovableObjectHandle scenequeryresult_movables_at(SceneQueryResultHandle handle, int index);

size_t scenequeryresult_worldfragments_count(SceneQueryResultHandle handle, int index);
void scenequeryresult_worldfragments_at(SceneQueryResultHandle handle, int index, ref world_fragment result);

RaySceneQueryListenerHandle create_rayscenequerylistener(RaySceneQueryFragmentResult fragment_callback, RaySceneQueryObjectResult object_callback, void* userdata);
void destroy_rayscenequerylistener(RaySceneQueryListenerHandle handle);

//setRay
void rayscenequery_set_ray(RaySceneQueryHandle handle, RayHandle ray_handle);
//getRay
RayHandle rayscenequery_get_ray(RaySceneQueryHandle handle);


//void setSortByDistance(bool sort, ushort maxresults = 0);
void rayscenequery_set_sort_by_distance(RaySceneQueryHandle handle, int on, ushort maxresults);
//bool getSortByDistance(void) const;
int rayscenequery_get_sort_by_distance(RaySceneQueryHandle handle);
//ushort getMaxResults(void) const;
ushort rayscenequery_get_max_results(RaySceneQueryHandle handle);


// typedef vector<RaySceneQueryResultEntry>::type RaySceneQueryResult;
size_t rayscenequeryresult_count(RaySceneQueryResultHandle handle);
void rayscenequeryresult_at(RaySceneQueryResultHandle handle, int index, ref rayscenequery_result_entry result);

// Ogre::Overlay
//const String& getName(void) const;
const(char[]) overlay_get_name(OverlayHandle handle);
//void setZOrder(ushort zorder);
void overlay_set_zorder(OverlayHandle handle, ushort zorder);
//ushort getZOrder(void) const;
ushort overlay_get_zorder(OverlayHandle handle);
//bool isVisible(void) const;
int overlay_is_visible(OverlayHandle handle);
//bool isInitialised(void) const;
int overlay_is_initialised(OverlayHandle handle);
//void show(void);
void overlay_show(OverlayHandle handle);
//void hide(void);
void overlay_hide(OverlayHandle handle);
//void add2D(OverlayContainer* cont);
void overlay_add_2d(OverlayHandle handle, OverlayContainerHandle cont);
//void remove2D(OverlayContainer* cont);
void overlay_remove_2d(OverlayHandle handle, OverlayContainerHandle cont);
//void add3D(SceneNode* node);
void overlay_add_3d(OverlayHandle handle, SceneNodeHandle node_handle);
//void remove3D(SceneNode* node);
void overlay_remove_3d(OverlayHandle handle, SceneNodeHandle node_handle);
// void clear();
void overlay_clear(OverlayHandle handle);
//void setScroll(Real x, Real y);
void overlay_set_scroll(OverlayHandle handle, coiReal x, coiReal y);
//Real getScrollX(void) const;
coiReal overlay_get_scroll_x(OverlayHandle handle);
//Real getScrollY(void) const;
coiReal overlay_get_scroll_y(OverlayHandle handle);
//void scroll(Real xoff, Real yoff);
void overlay_scroll(OverlayHandle handle, coiReal x, coiReal y);
//void setRotate(const Radian& angle);
void overlay_set_rotate(OverlayHandle handle, coiReal angle);
//const Radian &getRotate(void) const;
coiReal overlay_get_rotate(OverlayHandle handle);
//void rotate(const Radian& angle);
void overlay_rotate(OverlayHandle handle, coiReal angle);
//void setScale(Real x, Real y);
void overlay_set_scale(OverlayHandle handle, coiReal x, coiReal y);
//Real getScaleX(void) const;
coiReal overlay_get_scale_x(OverlayHandle handle);
//Real getScaleY(void) const;
coiReal overlay_get_scale_y(OverlayHandle handle);
//void _getWorldTransforms(Matrix4* xform) const;
void overlay_get_world_transforms(OverlayHandle handle, ref coiMatrix4 xform);
//const String& getOrigin(void) const;
const(char[]) overlay_get_origin(OverlayHandle handle);
//void _notifyOrigin(const String& origin);
void overlay_notify_origin(OverlayHandle handle, const(char*) origin);

//Ogre::OverlayManager
//OverlayManager();
OverlayManagerHandle create_overlaymanager();
//~OverlayManager();
void destroy_overlaymanager(OverlayManagerHandle handle);
//Real getLoadingOrder(void) const;
coiReal overlaymanager_get_loading_order(OverlayManagerHandle handle);
//Overlay* create(const String& name);
OverlayHandle overlaymanager_create(OverlayManagerHandle handle, const char* name);
//Overlay* getByName(const String& name);
OverlayHandle overlaymanager_get_by_name(OverlayManagerHandle handle, const char* name);
//void destroy(const String& name);
void overlaymanager_destroy_by_name(OverlayManagerHandle handle, const char* name);
//void destroy(Overlay* overlay);
void overlaymanager_destroy(OverlayManagerHandle handle, OverlayHandle overlay_handle);
//void destroyAll(void);
void overlaymanager_destroy_all(OverlayManagerHandle handle);
//bool hasViewportChanged(void) const;
int overlaymanager_has_viewport_changed(OverlayManagerHandle handle);
//int getViewportHeight(void) const;
int overlaymanager_get_viewport_height(OverlayManagerHandle handle);
//int getViewportWidth(void) const;
int overlaymanager_get_viewport_width(OverlayManagerHandle handle);
//Real getViewportAspectRatio(void) const;
coiReal overlaymanager_get_viewport_aspect_ratio(OverlayManagerHandle handle);
//OrientationMode getViewportOrientationMode(void) const;
orientation_mode overlaymanager_get_viewport_orientation_mode(OverlayManagerHandle handle);
//OverlayElement* createOverlayElement(const String& typeName, const String& instanceName, bool isTemplate = false);
OverlayElementHandle overlaymanager_create_overlayelement(OverlayManagerHandle handle, const char* type_name, const char* instance_name, int is_template);
//OverlayElement* getOverlayElement(const String& name, bool isTemplate = false);
OverlayElementHandle overlaymanager_get_overlayelement(OverlayManagerHandle handle, const char* name, int is_template);
//bool hasOverlayElement(const String& name, bool isTemplate = false);
int overlaymanager_has_overlay_element(OverlayManagerHandle handle, const char* name, int is_template);
//void destroyOverlayElement(const String& instanceName, bool isTemplate = false);
void overlaymanager_destroy_overlay_element(OverlayManagerHandle handle, const char* name, int is_template);
//void destroyOverlayElement(OverlayElement* pInstance, bool isTemplate = false);
void overlaymanager_destroy_overlay_element_instance(OverlayManagerHandle handle, OverlayElementHandle instance, int is_template);
//void destroyAllOverlayElements(bool isTemplate = false);
void overlaymanager_destroy_all_overlay_elements(OverlayManagerHandle handle);
//OverlayElement* createOverlayElementFromTemplate(const String& templateName, const String& typeName, const String& instanceName, bool isTemplate = false);
OverlayElementHandle overlaymanager_create_overlayelement_from_template(OverlayManagerHandle handle, const char* template_name, const char* type_name, const char* instance_name, int is_template);
//OverlayElement* cloneOverlayElementFromTemplate(const String& templateName, const String& instanceName);
OverlayElementHandle overlaymanager_clone_overlayelement_from_template(OverlayManagerHandle handle, const char* template_name, const char* instance_name);
//OverlayElement* createOverlayElementFromFactory(const String& typeName, const String& instanceName);
OverlayElementHandle overlaymanager_create_overlayelement_from_factory(OverlayManagerHandle handle, const char* type_name, const char* instance_name);
//bool isTemplate (String strName) const
int overlaymanager_is_template(OverlayManagerHandle handle, const char* name);
//static OverlayManager* getSingletonPtr(void);
OverlayManagerHandle overlaymanager_get_singleton_ptr();


// Ogre::OverlayElement

//~OverlayElement
void destroy_overlayelement(OverlayElementHandle handle);
//void initialise(void)
void overlayelement_initialise(OverlayElementHandle handle);
//const String& getName(void) const;
const(char[]) overlayelement_get_name(OverlayElementHandle handle);
//void show(void);
void overlayelement_show(OverlayElementHandle handle);
//void hide(void);
void overlayelement_hide(OverlayElementHandle handle);
//bool isVisible(void) const;
int overlayelement_is_visible(OverlayElementHandle handle);
//bool isEnabled() const;
int overlayelement_is_enabled(OverlayElementHandle handle);
//void setEnabled(bool b);
void overlayelement_set_enabled(OverlayElementHandle handle, int b);
//void setDimensions(Real width, Real height);
void overlayelement_set_dimensions(OverlayElementHandle handle, coiReal width, coiReal height);
//void setPosition(Real left, Real top);
void overlayelement_set_position(OverlayElementHandle handle, coiReal left, coiReal top);
//void setWidth(Real width);
void overlayelement_set_width(OverlayElementHandle handle, coiReal width);
//Real getWidth(void) const;
coiReal overlayelement_get_width(OverlayElementHandle handle);
//void setHeight(Real height);
void overlayelement_set_height(OverlayElementHandle handle, coiReal height);
//Real getHeight(void) const;
coiReal overlayelement_get_height(OverlayElementHandle handle);
//void setLeft(Real left);
void overlayelement_set_left(OverlayElementHandle handle, coiReal left);
//Real getLeft(void) const;
coiReal overlayelement_get_left(OverlayElementHandle handle);
//void setTop(Real Top);
void overlayelement_set_top(OverlayElementHandle handle, coiReal top);
//Real getTop(void) const;
coiReal overlayelement_get_top(OverlayElementHandle handle);
//Real _getLeft(void) const;
coiReal overlayelement__get_left(OverlayElementHandle handle);
//Real _getTop(void) const;
coiReal overlayelement__get_top(OverlayElementHandle handle);
//Real _getWidth(void) const;
coiReal overlayelement__get_width(OverlayElementHandle handle);
//Real _getHeight(void) const;
coiReal overlayelement__get_height(OverlayElementHandle handle);
//void _setLeft(Real left);
void overlayelement__set_left(OverlayElementHandle handle, coiReal left);
//void _setTop(Real top);
void overlayelement__set_top(OverlayElementHandle handle, coiReal top);
//void _setWidth(Real width);
void overlayelement__set_width(OverlayElementHandle handle, coiReal width);
//void _setHeight(Real height);
void overlayelement__set_height(OverlayElementHandle handle, coiReal height);
//void _setPosition(Real left, Real top);
void overlayelement__set_position(OverlayElementHandle handle, coiReal left, coiReal top);
//void _setDimensions(Real width, Real height);
void overlayelement__set_dimensions(OverlayElementHandle handle, coiReal width, coiReal height);
//const String& getMaterialName(void) const;
const(char[]) overlayelement_get_material_name(OverlayElementHandle handle);
//void setMaterialName(const String& matName);
void overlayelement_set_material_name(OverlayElementHandle handle, const char* name);
//void getWorldTransforms(Matrix4* xform) const;
void overlayelement_get_world_transforms(OverlayElementHandle handle, ref coiMatrix4 xform);
//void _positionsOutOfDate(void);
void overlayelement__positions_out_of_date(OverlayElementHandle handle);
//void _update(void);
void overlayelement__update(OverlayElementHandle handle);
//void _updateFromParent(void);
void overlayelement__update_from_parent(OverlayElementHandle handle);
//void _notifyParent(OverlayContainer* parent, Overlay* overlay);
void overlayelement__notify_parent(OverlayElementHandle handle, OverlayContainerHandle parent_handle, OverlayHandle overlay_handle);
//Real _getDerivedLeft(void);
coiReal overlayelement__get_derived_left(OverlayElementHandle handle);
//Real _getDerivedTop(void);
coiReal overlayelement__get_derived_top(OverlayElementHandle handle);
//Real _getRelativeWidth(void);
coiReal overlayelement__get_relative_width(OverlayElementHandle handle);
//Real _getRelativeHeight(void);
coiReal overlayelement__get_relative_height(OverlayElementHandle handle);
//ushort _notifyZOrder(ushort newZOrder);
ushort overlayelement__notify_zorder(OverlayElementHandle handle, ushort new_zorder);
//void _notifyWorldTransforms(const Matrix4& xform);
void overlayelement__notify_world_transforms(OverlayElementHandle handle, const ref coiMatrix4 xform);
//void _notifyViewport();
void overlayelement__notify_viewport(OverlayElementHandle handle);
//const String& getTypeName(void) const;
const(char[]) overlayelement_get_type_name(OverlayElementHandle handle);
//void setCaption(const DisplayString& text);
void overlayelement_set_caption(OverlayElementHandle handle, const char* text);
//const DisplayString& getCaption(void) const;
const(char[]) overlayelement_get_caption(OverlayElementHandle handle);
//void setColour(const ColourValue& col);
void overlayelement_set_colour(OverlayElementHandle handle, const ref coiColourValue col);
//const ColourValue& getColour(void) const;
void overlayelement_get_colour(OverlayElementHandle handle, ref coiColourValue col);
//void setMetricsMode(GuiMetricsMode gmm);
void overlayelement_set_metrics_mode(OverlayElementHandle handle, gui_metrics_mode gmm);
//GuiMetricsMode getMetricsMode(void) const;
gui_metrics_mode overlayelement_get_metrics_mode(OverlayElementHandle handle);
//void setHorizontalAlignment(GuiHorizontalAlignment gha);
void overlayelement_set_horizontal_alignment(OverlayElementHandle handle, gui_horizontal_alignment gha);
//GuiHorizontalAlignment getHorizontalAlignment(void) const;
gui_horizontal_alignment overlayelement_get_horizontal_alignment(OverlayElementHandle handle);
//void setVerticalAlignment(GuiVerticalAlignment gva);
void overlayelement_set_vertical_alignment(OverlayElementHandle handle, gui_vertical_alignment gva);
//GuiVerticalAlignment getVerticalAlignment(void) const;
gui_vertical_alignment overlayelement_get_vertical_alignment(OverlayElementHandle handle);
//bool contains(Real x, Real y) const;
int overlayelement_contains(OverlayElementHandle handle, coiReal x, coiReal y);
//OverlayElement* findElementAt(Real x, Real y);
OverlayElementHandle overlayelement_find_element_at(OverlayElementHandle handle, coiReal x, coiReal y);
//bool isContainer() const;
int overlayelement_is_container(OverlayElementHandle handle);
//bool isKeyEnabled() const;
int overlayelement_is_key_enabled(OverlayElementHandle handle);
//bool isCloneable() const
int overlayelement_is_cloneable(OverlayElementHandle handle);
//void setCloneable(bool c);
void overlayelement_set_cloneable(OverlayElementHandle handle, int c);
//OverlayContainer* getParent();
OverlayContainerHandle overlayelement_get_parent(OverlayElementHandle handle);
//void _setParent(OverlayContainer* parent);
void overlayelement_set_parent(OverlayElementHandle handle, OverlayContainerHandle parent_handle);
//ushort getZOrder() const;
ushort overlayelement_get_zorder(OverlayElementHandle handle);
//Real getSquaredViewDepth(const Camera* cam) const;
coiReal overlayelement_get_squared_view_depth(OverlayElementHandle handle, CameraHandle camera_handle);
//void copyFromTemplate(OverlayElement* templateOverlay);
void overlayelement_copy_from_template(OverlayElementHandle handle, OverlayElementHandle template_handle);
//OverlayElement* clone(const String& instanceName);
OverlayElementHandle overlayelement_clone(OverlayElementHandle handle, const char* instance_name);
//const OverlayElement* getSourceTemplate () const;
const(OverlayElementHandle) overlayelement_get_source_template(OverlayElementHandle handle);


// Ogre::OverlayContainer
void destroy_overlaycontainer(OverlayContainerHandle handle);
//void addChild(OverlayElement* elem);
void overlaycontainer_add_child(OverlayContainerHandle handle, OverlayElementHandle child_handle);
//void addChildImpl(OverlayElement* elem);
void overlaycontainer_add_child_impl(OverlayContainerHandle handle, OverlayElementHandle child_handle);
//void addChildImpl(OverlayContainer* cont);
void overlaycontainer_add_child_container_impl(OverlayContainerHandle handle, OverlayContainerHandle child_handle);
//void removeChild(const String& name);
void overlaycontainer_remove_child(OverlayContainerHandle handle, const char* name);
//OverlayElement* getChild(const String& name);
OverlayElementHandle overlaycontainer_get_child(OverlayContainerHandle handle, const char* name);
//void initialise(void);
void overlaycontainer_initialise(OverlayContainerHandle handle);
//void _addChild(OverlayElement* elem);
void overlaycontainer__add_child(OverlayContainerHandle handle, OverlayElementHandle elem);
//void _removeChild(OverlayElement* elem);
void overlaycontainer__remove_child(OverlayContainerHandle handle, OverlayElementHandle elem);
//void _removeChild(const String& name);
void overlaycontainer__remove_child_by_name(OverlayContainerHandle handle, const char* name);
//void _positionsOutOfDate(void);
void overlaycontainer__positions_out_of_date(OverlayContainerHandle handle);
//void _update(void);
void overlaycontainer__update(OverlayContainerHandle handle);
//ushort _notifyZOrder(ushort newZOrder);
ushort overlaycontainer__notify_zorder(OverlayContainerHandle handle, ushort new_zorder);
//void _notifyViewport();
void overlaycontainer__notify_viewport(OverlayContainerHandle handle);
//void _notifyWorldTransforms(const Matrix4& xform);
void overlaycontainer__notify_world_transforms(OverlayContainerHandle handle, const ref coiMatrix4 xform);
//void _notifyParent(OverlayContainer* parent, Overlay* overlay);
void overlaycontainer__notify_parent(OverlayContainerHandle handle, OverlayContainerHandle parent_handle, OverlayHandle overlay_handle);
//bool isContainer() const;
int overlaycontainer_is_container(OverlayContainerHandle handle);
//bool isChildrenProcessEvents() const;
int overlaycontainer_is_children_process_events(OverlayContainerHandle handle);
//void setChildrenProcessEvents(bool val);
void overlaycontainer_set_children_process_events(OverlayContainerHandle handle, int val);
//OverlayElement* findElementAt(Real x, Real y);
OverlayElementHandle overlaycontainer_find_element_at(OverlayContainerHandle handle, coiReal x, coiReal y);
//void copyFromTemplate(OverlayElement* templateOverlay);
void overlaycontainer_copy_from_template(OverlayContainerHandle handle, OverlayElementHandle template_overlay);
//virtual OverlayElement* clone(const String& instanceName);
OverlayElementHandle overlaycontainer_clone(OverlayContainerHandle handle, const char* instance_name);
