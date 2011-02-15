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
    engine_options options;
    default_engine_options(&options);
	options.renderer_s = "OpenGL";
    options.window_title = "Renderwindow from C";

    init_engine(options);

    add_resource_location("../media/materials/scripts", "FileSystem", "General");
    add_resource_location("../media/materials/textures", "FileSystem", "General");
    add_resource_location("../media/models", "FileSystem", "General");

    coiCameraHandle myCamera = create_camera("mycam");
    
    camera_set_position(myCamera, 0, 0, 80);

    camera_lookat(myCamera, 0, 0, -300);

    camera_set_near_clip_distance(myCamera, 5);

    coiCameraHandle anotherHandle = get_camera("mycam");
    
    add_viewport(anotherHandle);

    set_default_num_mipmaps(5);

    initialise_all_resourcegroups();

    coiEntityHandle entity = create_entity("OgreHead", "ogrehead.mesh");

    coiSceneNodeHandle node = create_child_scenenode("headNode");

    attach_entity_to_scenenode(entity, node);
   
    set_ambient_light_rgb(0.5f, 0.5f, 0.5f);

    coiLightHandle light = create_light("mainLight");

    light_set_position(light, 20, 80, 50);

    render_loop();

    release_engine();

    return 0;
}
