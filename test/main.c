#include <ogre_interface.h>

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
	// C90 requires all vars to be declared at top of function
	CameraHandle myCamera;
	CameraHandle anotherHandle;
	EntityHandle entity;
	SceneNodeHandle node;
	LightHandle light;
    RenderSystemHandle rendersystem;
    
// 	engine_options options;
//     default_engine_options(&options);
// 	options.renderer_s = "OpenGL";
//     options.window_title = "Renderwindow from C - better version";
// 
//     init_engine(options);

    create_root("", "", "ogre.log");
    
    load_ogre_plugin("RenderSystem_GL");

    rendersystem = get_render_system_by_name("OpenGL Rendering Subsystem");
    
    render_system_set_config_option(rendersystem, "Full Screen", "No");
    render_system_set_config_option(rendersystem, "VSync", "No");
    render_system_set_config_option(rendersystem, "Video Mode", "800 x 600 @ 32-bit");

    set_render_system(rendersystem);
    
    load_ogre_plugin("Plugin_OctreeSceneManager");
    
    root_initialise(1, "The Ogre Window");
    
	create_scene_manager("OctreeSceneManager");
    
    add_resource_location("../media/materials/scripts", "FileSystem", "General");
    add_resource_location("../media/materials/textures", "FileSystem", "General");
    add_resource_location("../media/models", "FileSystem", "General");

	myCamera = create_camera("mycam");

    camera_set_position(myCamera, 0, 0, 80);

    camera_lookat(myCamera, 0, 0, -300);

    camera_set_near_clip_distance(myCamera, 5);

    anotherHandle = get_camera("mycam");

    add_viewport(anotherHandle);

    set_default_num_mipmaps(5);

    initialise_all_resourcegroups();

    entity = create_entity("OgreHead", "ogrehead.mesh");

    node = create_child_scenenode("headNode");

    attach_entity_to_scenenode(entity, node);

    set_ambient_light_rgb(0.5f, 0.5f, 0.5f);

    light = create_light("mainLight");

    light_set_position(light, 20, 80, 50);

    render_loop();

    release_engine();

    return 0;
}
