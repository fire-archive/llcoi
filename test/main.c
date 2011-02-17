#include <ogre_interface.h>

#include <allegro.h>
#include <allegro_opengl.h>

#if defined( WIN32 ) || defined( _WINDOWS )
#   define WIN32_LEAN_AND_MEAN
#   include "windows.h"
#endif

#if defined( WIN32 ) || defined( _WINDOWS )
INT WINAPI WinMain( HINSTANCE hInst, HINSTANCE hPrevInst, LPSTR strCmdLine, INT nCmdShow)
#else
int main(int argc, char *argv[])
#endif
{
	/* C90 requires all vars to be declared at top of function */
	CameraHandle myCamera;
	CameraHandle anotherHandle;
	EntityHandle entity;
	SceneNodeHandle node;
	LightHandle light;
    RenderSystemHandle rendersystem;
    RenderWindowHandle renderwindow;
    
   ALLEGRO_DISPLAY *display;

   if (!al_init()) {
      return 1;
   }
   al_install_keyboard();
   al_install_mouse();

   al_set_new_display_flags(ALLEGRO_OPENGL | ALLEGRO_RESIZABLE);
   display = al_create_display(800, 600);
   if (!display) {
      return 1;
   }
   al_set_window_title(display, "My window");
   
    create_root("plugins.cfg", "ogre.cfg", "ogre.log");
   
/*    load_ogre_plugin("RenderSystem_GL");

    rendersystem = get_render_system_by_name("OpenGL Rendering Subsystem");
    
    render_system_set_config_option(rendersystem, "Full Screen", "No");
    render_system_set_config_option(rendersystem, "VSync", "No");
    render_system_set_config_option(rendersystem, "Video Mode", "800 x 600 @ 32-bit");

    set_render_system(rendersystem);
    
    load_ogre_plugin("Plugin_OctreeSceneManager");*/
    
    if(!(restore_config() || show_config_dialog()))
    {
        return 1;
    }

    setup_resources("resources.cfg");
    
    renderwindow = create_render_window_ex("The RenderWindow", al_get_display_width(display), al_get_display_height(display), 0, "currentGLContext", "True");
    
    render_window_set_visible(renderwindow, 1);
    
    set_default_num_mipmaps(5);
    
    initialise_all_resourcegroups();
    
    create_scene_manager("OctreeSceneManager", "The SceneManager");

    myCamera = create_camera("mycam");

    camera_set_position(myCamera, 0, 0, 80);

    camera_lookat(myCamera, 0, 0, -300);

    camera_set_near_clip_distance(myCamera, 5);

    anotherHandle = get_camera("mycam");

    add_viewport(anotherHandle);

    entity = create_entity("OgreHead", "ogrehead.mesh");

    node = create_child_scenenode("headNode");

    attach_entity_to_scenenode(entity, node);

    set_ambient_light_rgb(0.5f, 0.5f, 0.5f);

    light = create_light("mainLight");

    light_set_position(light, 20, 80, 50);

    //render_loop();

    //release_engine();

    al_uninstall_system();

   return 0;
}
