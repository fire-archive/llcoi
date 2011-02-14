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
    
    create_camera("mycam");
    
    camera_set_near_clip_distance("mycam", 10.0);
    
    add_viewport("mycam");

    render_loop();

    release_engine();

    return 0;
}
