//FIXME: Not finished yet

import llcoi.ogre_interface;
import llcoi.ois_interface;

import std.math;
import std.c.stdio;
import std.stdio : writefln, writeln;

pragma(lib, "llcoi");

CameraHandle myCamera;
KeyboardInputHandle keyboard;
MouseInputHandle mouse;
float tiny_timer=0;

// Looks like it's a Bad Idea [tm] to place Ogre's singleton's in thread local storage :D
__gshared LogManagerHandle log_manager;
__gshared LogHandle log;

extern(C) void log_listener_test(const char *message, log_message_level lml, int maskDebug, const char *log_name, int skip_message)
{
    // Uncomment this to spew all of the logging, as well as information about the logging.
    // Or leave it commented, and it acts as a NullLogListener. (:

    //printf("received message \"%s\", level %d, maskDebug %d, log_name \"%s\", skip_message %d\n", message, lml, log_name, skip_message);
}

extern(C) void window_event_listener_test(RenderWindowHandle window_handle)
{
    log_message("I was called when the window closed!");
}


extern(C) int frame_listener_test(float evt_time,float frame_time,int event_type)
{
    tiny_timer+=frame_time;
    camera_set_position(myCamera,cos(tiny_timer)*100,50,sin(tiny_timer)*100);
    camera_lookat(myCamera,0,0,0);
    return 1;
}

int main()
{
    EntityHandle entity;
    SceneNodeHandle node;
    LightHandle light;
__gshared    RenderSystemHandle rendersystem;
    __gshared RenderWindowHandle window;
__gshared    ViewportHandle viewport;



    int keep_going = 1;

    // Make sure a log manager is created before Ogre::Root.
    log_manager = create_log_manager();

    log = logmanager_create_log("", // filename
                                1,  // default log?
                                0,  // output to debugger
                                0); // supressFileOutput

    // Add a listener callback to the log manager
    add_log_listener(&log_listener_test, log);

    // Same as root = new Ogre::Root("", "", "");
    auto root = create_root("", "", "");

    load_ogre_plugin("RenderSystem_GL");
    load_ogre_plugin("Plugin_OctreeSceneManager");

    // Get a list of available renderers. Note that we'll need to
    // free the memory held by that list when we're done with it.
    auto rslist = root_get_available_renderers();

    // Make sure the list has at least one renderer
    if (render_system_list_size(rslist) == 0)
    {
        writeln("No RenderSystems found!");
        return 1;
    }

    // Choose the first available render system
    set_render_system(render_system_list_get(rslist, 0));

    root_initialise(0,  // Autocreate window?
                    ""  // name of autocreated window, if any.
    );

    // Clean up our render system list.
    destroy_render_system_list(rslist);

    // Create an Ogre::NameValuePairList. Again, we'll have to free it
    // when we're done.
    auto params = create_name_value_pair_list();

    // Just like params["FSAA"] = "0";
    add_pair(params, "FSAA", "0");

    // Ditto
    add_pair(params, "vsync", "true");

    window = root_create_render_window("Example Window", // title
                                       800,              // height
                                       600,              // width
                                       0,                // fullscreen
                                       params);

    
    setup_resources("resources.cfg");

    set_default_num_mipmaps(5);

    initialise_all_resourcegroups();

    auto scene_manager = create_scene_manager("OctreeSceneManager", "The SceneManager");

    myCamera = create_camera("mycam");

    camera_set_position(myCamera, 0, 0, 80);

    camera_lookat(myCamera, 0, 0, -300);

    camera_set_near_clip_distance(myCamera, 5);

    viewport = render_window_add_viewport(window, myCamera);

    viewport_set_background_colour(viewport, 0, 0, 0);

    camera_set_aspect_ratio(myCamera, 800, 600);

    entity = create_entity("OgreHead", "ogrehead.mesh");

    node = create_child_scenenode("headNode");

    attach_entity_to_scenenode(entity, node);

    set_ambient_light_rgb(0.5f, 0.5f, 0.5f);

    light = create_light("mainLight");

    light_set_position(light, 20, 80, 50);

    add_frame_listener(&frame_listener_test, EVENT_FRAME_RENDERING_QUEUED|EVENT_FRAME_STARTED);

    add_window_listener(window, &window_event_listener_test);

    create_input_system(render_window_get_hwnd(window));
    
    keyboard = create_keyboard_object(0);
    mouse = create_mouse_object(0);
    
    while(keep_going)
    {
        keyboard_capture(keyboard);
        mouse_capture(mouse);

        if(keyboard_is_key_down(keyboard, KeyCode.KC_ESCAPE))
            keep_going = 0;
        
        // Pump window messages for nice behaviour
        pump_messages();

        // Render a frame
        render_one_frame();

        if (render_window_is_closed(window))
            keep_going = 0;
    }

    destroy_name_value_pair_list(params);
    remove_window_listener(window);
    destroy_keyboard_object(keyboard);
    destroy_mouse_object(mouse);
    destroy_input_system();
    release_engine();

    return 0;
}
