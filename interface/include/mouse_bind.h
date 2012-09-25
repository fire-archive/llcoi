#pragma once

#include "ois_interface.h"

typedef struct
{
    //! Absolute and Relative value components
    int abs, rel;
    //! Indicates if this Axis only supports absolute (ie JoyStick)
    int absOnly;
} Axis;

typedef struct
{
    int width, height;
    //! X Axis component
    Axis X;
    //! Y Axis Component
    Axis Y;
    //! Z Axis Component
    Axis Z;
    int buttons;
} MouseState;

enum MouseButtonID
{
    MB_Left = 0, MB_Right, MB_Middle,
    MB_Button3, MB_Button4, MB_Button5, MB_Button6, MB_Button7
};

DLL MouseState mouse_get_state(MouseInputHandle mouse_handle);

DLL void mouse_set_buffered(MouseInputHandle mouse_handle, int buffered);

DLL void mouse_capture(MouseInputHandle mouse_handle);
