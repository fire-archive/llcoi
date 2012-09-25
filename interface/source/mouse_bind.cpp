#include "mouse_bind.h"

#include <OISMouse.h>

void mouse_set_buffered(MouseInputHandle mouse_handle, int buffered)
{
    OIS::Mouse* mouse = reinterpret_cast<OIS::Mouse*>(mouse_handle);
    mouse->setBuffered((bool)buffered);
}

void mouse_capture(MouseInputHandle mouse_handle)
{
    OIS::Mouse* mouse = reinterpret_cast<OIS::Mouse*>(mouse_handle);
    mouse->capture();
}

MouseState mouse_get_state(MouseInputHandle mouse_handle)
{
    OIS::Mouse* mouse = reinterpret_cast<OIS::Mouse*>(mouse_handle);
    OIS::MouseState state = mouse->getMouseState();
    MouseState out_state;
    out_state.buttons = state.buttons;
    out_state.height = state.height;
    out_state.width = state.width;
    out_state.X.abs = state.X.abs;
    out_state.X.rel = state.X.rel;
    out_state.X.absOnly = state.X.absOnly;
    out_state.Y.abs = state.Y.abs;
    out_state.Y.rel = state.Y.rel;
    out_state.Y.absOnly = state.Y.absOnly;
    out_state.Z.abs = state.Z.abs;
    out_state.Z.rel = state.Z.rel;
    out_state.Z.absOnly = state.Z.absOnly;
    return out_state;
}
