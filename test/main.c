#include <ogre_interface.h>

int main(void){

    engine_options options;
    default_engine_options(&options);

    options.window_title = "Renderwindow from C";

    init_engine(options);
    
    create_camera("mycam");
    
    camera_set_near_clip_distance( get_camera("mycam"), 10.0);
    
    add_viewport("mycam");

    render_loop();

    release_engine();

    return 0;
}
