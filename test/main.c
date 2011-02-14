#include "../interface/main.h"

int main(void){

    init_engine("OpenGL", "/usr/local/lib/OGRE", 1);

    create_camera("mycam");
    camera_set_near_clip_distance(get_camera("mycam"), 10.0);
    add_viewport("mycam");

    while(render_loop())
        render_loop();

    ogre3dRelease();

    return 1;
}
