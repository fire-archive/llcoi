import llcoi.ogre_interface;
import llcoi.ois_interface;

import std.math;
import std.c.stdio;

pragma(lib, "llcoi");

CameraHandle myCamera;
KeyboardInputHandle keyboard;
MouseInputHandle mouse;
float tiny_timer=0;

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
    RenderSystemHandle rendersystem;
    RenderWindowHandle renderwindow;
    ViewportHandle viewport;

/*
version(linux)
{
    Display *disp;
    Window win;
    uint scrn;
}
*/

    int keep_going = 1;

    create_root("plugins.cfg", "ogre.cfg", "ogre.log");

    if (!(restore_config() || show_config_dialog()))
    {
        return 1;
    }

	setup_resources("resources.cfg");

    renderwindow = root_initialise(1, "Ogre Renderwindow");

    set_default_num_mipmaps(5);

    initialise_all_resourcegroups();

    create_scene_manager("OctreeSceneManager", "The SceneManager");

    myCamera = create_camera("mycam");

    camera_set_position(myCamera, 0, 0, 80);

    camera_lookat(myCamera, 0, 0, -300);

    camera_set_near_clip_distance(myCamera, 5);

    viewport = add_viewport(myCamera);

    viewport_set_background_colour(viewport, 0, 0, 0);

    camera_set_aspect_ratio(myCamera, 800, 600);

    entity = create_entity("OgreHead", "ogrehead.mesh");

    node = create_child_scenenode("headNode");

    attach_entity_to_scenenode(entity, node);

    set_ambient_light_rgb(0.5f, 0.5f, 0.5f);

    light = create_light("mainLight");

    light_set_position(light, 20, 80, 50);

    add_frame_listener(&frame_listener_test,EVENT_FRAME_RENDERING_QUEUED|EVENT_FRAME_STARTED);

	add_window_listener(renderwindow, &window_event_listener_test);

    create_input_system(render_window_get_hwnd(renderwindow));
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

        if (render_window_closed())
        {
            keep_going = 0;
        }
        
    }

/*
version(linux)
{
    disp = XOpenDisplay( NULL );
    scrn = DefaultScreen(disp);
    sprintf(openinput_window_params, "c:%u s:%u w:%u", cast(uint)disp, cast(uint)scrn, windowHnd);
}
*/

	remove_window_listener(renderwindow);

    destroy_keyboard_object(keyboard);
    destroy_mouse_object(mouse);
    destroy_input_system();
    release_engine();

    return 0;
}
