#include <ogre_interface.h>

#if defined( WIN32 ) || defined( _WINDOWS )
#   define WIN32_LEAN_AND_MEAN
#   include "windows.h"
#endif

#if defined( WIN32 ) || defined( _WINDOWS )
INT WINAPI WinMain( HINSTANCE hInst, HINSTANCE, LPSTR strCmdLine, INT )
#else
int main(int argc, char *argv[])
#endif
{
    engine_options options;
    default_engine_options(&options);
    options.window_title = "Renderwindow from C";

    init_engine(options);
    
    add_resource_location("../media/materials/scripts", "FileSystem", "General");
    add_resource_location("../media/materials/textures", "FileSystem", "General");
    add_resource_location("../media/models", "FileSystem", "General");
    
    create_camera("mycam");
    
    camera_set_position("mycam", 0, 0, 80);
    
    camera_lookat("mycam", 0, 0, -300);
    
    camera_set_near_clip_distance("mycam", 5);

    add_viewport("mycam");

    textureManager_setDefaultNumMipmaps(5);
    
    initialise_all_resourcegroups();
    
    create_entity("OgreHead", "ogrehead.mesh");
    
    create_child_scenenode("headNode");
    
    attach_entity_to_scenenode("OgreHead", "headNode");
    
    set_ambient_light_rgb(0.5f, 0.5f, 0.5f);
    
    create_light("mainLight");
    
    light_set_position("mainLight", 20, 80, 50);
    
    
    render_loop();

    release_engine();

    return 0;
}