#include <ogre_interface.h>

#define LLCOI_TEST_USE_OPENINPUT 1

#if defined(LLCOI_TEST_USE_OPENINPUT)
#   include <openinput.h>
#endif

#if defined(LLCOI_TEST_USE_ALLEGRO)
#   include <allegro.h>
#endif

#include <math.h>
#include <stdio.h>

#if defined( WIN32 ) || defined( _WINDOWS )
#   define WIN32_LEAN_AND_MEAN
#   include "windows.h"
#else
#if defined(LLCOI_TEST_USE_OPENINPUT)
#include <X11/Xlib.h>
#endif
#endif

CameraHandle myCamera;
float tiny_timer=0;

void window_event_listener_test(RenderWindowHandle window_handle)
{
	log_message("I was called when the window closed!");
}

int frame_listener_test(float evt_time,float frame_time,int event_type)
{
    tiny_timer+=frame_time;
    camera_set_position(myCamera,cos(tiny_timer)*100,50,sin(tiny_timer)*100);
    camera_lookat(myCamera,0,0,0);
    return 1;
}

#if defined(LLCOI_TEST_USE_OPENINPUT) && defined(PLATFORM_WIN)
INT WINAPI WinMain( HINSTANCE hInst, HINSTANCE hPrevInst, LPSTR strCmdLine, INT nCmdShow)
#else
int main(int argc, char *argv[])
#endif
{
    /* C90 requires all vars to be declared at top of function */

    EntityHandle entity;
    SceneNodeHandle node;
    LightHandle light;
    RenderSystemHandle rendersystem;
    RenderWindowHandle renderwindow;
    ViewportHandle viewport;
    
#if defined(LLCOI_TEST_USE_OPENINPUT)
    // Openinput
    oi_event evt;
    char openinput_window_params[100];
    unsigned int windowHnd = 0;

#if defined(PLATFORM_LINUX)
    Display *disp;
    Window win;
    unsigned int scrn;
#endif
#endif //LLCOI_TEST_USE_OPENINPUT
    
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

    add_frame_listener(frame_listener_test,EVENT_FRAME_RENDERING_QUEUED|EVENT_FRAME_STARTED);

	add_window_listener(renderwindow, window_event_listener_test);

#if defined(LLCOI_TEST_USE_OPENINPUT)
    windowHnd = render_window_get_hwnd(renderwindow);

#if defined(PLATFORM_LINUX)
    disp = XOpenDisplay( NULL );
    scrn = DefaultScreen(disp);
    sprintf(openinput_window_params, "c:%u s:%u w:%u", (unsigned int)disp, (unsigned int)scrn, windowHnd);
#else
    sprintf(openinput_window_params, "c:%u s:%u w:%u", 0, 0, windowHnd);
#endif
    
    oi_init(openinput_window_params, 0);
    
    //render_loop();
    while (keep_going)
    {
        // ask oi to wait for events
        oi_events_poll(&evt);
        switch(evt.type)
        {
            case OI_QUIT:
                // Quit
                keep_going = 0;
                break;

            case OI_KEYDOWN:
                // Keyboard button down
                //WTF?? Better way to check this, please..
                if(evt.key.keysym.sym == OIK_ESC)
                    keep_going = 0;
                break;

            default:
                break;
        }
        // Pump window messages for nice behaviour
        pump_messages();
        // Render a frame
        render_one_frame();

        if (render_window_closed())
        {
            keep_going = 0;
        }
    }

    oi_close();
#endif //LLCOI_TEST_USE_OPENINPUT
    
	//remove_window_listener(renderwindow);

    release_engine();

    return 0;
}
