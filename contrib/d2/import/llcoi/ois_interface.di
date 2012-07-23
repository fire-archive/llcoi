/******************************************************************************
 * ogre_interface.di - main interface file for D clients
 ******************************************************************************
 * This file is part of
 *     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 *                          
 * Low Level C Ogre Interface (llcoi)
 *
 * See http://code.google.com/p/llcoi/ for more information.
 *
 * Copyright (c) 2011, Llcoi Team
 * 
 * License: MIT
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 ******************************************************************************/

import llcoi.ogre_interface;

extern(C):

struct InputSystemHandle__
{
    int unused;
}

alias InputSystemHandle__ *InputSystemHandle;

struct MouseInputHandle__
{
    int unused;
}

alias MouseInputHandle__ *MouseInputHandle;

struct KeyboardInputHandle__
{
    int unused;
}

alias KeyboardInputHandle__ *KeyboardInputHandle;

//! Keyboard scan codes
enum KeyCode
{
    KC_UNASSIGNED,
    KC_ESCAPE,
    KC_1,
    KC_2,
    KC_3,
    KC_4,
    KC_5,
    KC_6,
    KC_7,
    KC_8,
    KC_9,
    KC_0,
    KC_MINUS,
    KC_EQUALS,
    KC_BACK,
    KC_TAB,
    KC_Q,
    KC_W,
    KC_E,
    KC_R,
    KC_T,
    KC_Y,
    KC_U,
    KC_I,
    KC_O,
    KC_P,
    KC_LBRACKET,
    KC_RBRACKET,
    KC_RETURN,
    KC_LCONTROL,
    KC_A,
    KC_S,
    KC_D,
    KC_F,
    KC_G,
    KC_H,
    KC_J,
    KC_K,
    KC_L,
    KC_SEMICOLON,
    KC_APOSTROPHE,
    KC_GRAVE,
    KC_LSHIFT,
    KC_BACKSLASH,
    KC_Z,
    KC_X,
    KC_C,
    KC_V,
    KC_B,
    KC_N,
    KC_M,
    KC_COMMA,
    KC_PERIOD,
    KC_SLASH,
    KC_RSHIFT,
    KC_MULTIPLY,
    KC_LMENU,
    KC_SPACE,
    KC_CAPITAL,
    KC_F1,
    KC_F2,
    KC_F3,
    KC_F4,
    KC_F5,
    KC_F6,
    KC_F7,
    KC_F8,
    KC_F9,
    KC_F10,
    KC_NUMLOCK,
    KC_SCROLL,
    KC_NUMPAD7,
    KC_NUMPAD8,
    KC_NUMPAD9,
    KC_SUBTRACT,
    KC_NUMPAD4,
    KC_NUMPAD5,
    KC_NUMPAD6,
    KC_ADD,
    KC_NUMPAD1,
    KC_NUMPAD2,
    KC_NUMPAD3,
    KC_NUMPAD0,
    KC_DECIMAL,
    KC_OEM_102 = 86,
    KC_F11,
    KC_F12,
    KC_F13 = 100,
    KC_F14,
    KC_F15,
    KC_KANA = 112,
    KC_ABNT_C1 = 115,
    KC_CONVERT = 121,
    KC_NOCONVERT = 123,
    KC_YEN = 125,
    KC_ABNT_C2,
    KC_NUMPADEQUALS = 141,
    KC_PREVTRACK = 144,
    KC_AT,
    KC_COLON,
    KC_UNDERLINE,
    KC_KANJI,
    KC_STOP,
    KC_AX,
    KC_UNLABELED,
    KC_NEXTTRACK = 153,
    KC_NUMPADENTER = 156,
    KC_RCONTROL,
    KC_MUTE = 160,
    KC_CALCULATOR,
    KC_PLAYPAUSE,
    KC_MEDIASTOP = 164,
    KC_VOLUMEDOWN = 174,
    KC_VOLUMEUP = 176,
    KC_WEBHOME = 178,
    KC_NUMPADCOMMA,
    KC_DIVIDE = 181,
    KC_SYSRQ = 183,
    KC_RMENU,
    KC_PAUSE = 197,
    KC_HOME = 199,
    KC_UP,
    KC_PGUP,
    KC_LEFT = 203,
    KC_RIGHT = 205,
    KC_END = 207,
    KC_DOWN,
    KC_PGDOWN,
    KC_INSERT,
    KC_DELETE,
    KC_LWIN = 219,
    KC_RWIN,
    KC_APPS,
    KC_POWER,
    KC_SLEEP,
    KC_WAKE = 227,
    KC_WEBSEARCH = 229,
    KC_WEBFAVORITES,
    KC_WEBREFRESH,
    KC_WEBSTOP,
    KC_WEBFORWARD,
    KC_WEBBACK,
    KC_MYCOMPUTER,
    KC_MAIL,
    KC_MEDIASELECT,
}

//! Button ID for mouse devices
enum MouseButtonID
{
    MB_Left,
    MB_Right,
    MB_Middle,
    MB_Button3,
    MB_Button4,
    MB_Button5,
    MB_Button6,
    MB_Button7,
}

struct KeyEvent
{
    KeyCode key;
    uint text;
}


enum Key_Modifier
{
    Shift = 1,
    Ctrl = 16,
    Alt = 256,
}

struct Axis
{
    //! Absolute and Relative value components
    int abs;
    int rel;
    //! Indicates if this Axis only supports absolute (ie JoyStick)
    int absOnly;
}

struct MouseState
{
    int width;
    int height;
    //! X Axis component
    Axis X;
    //! Y Axis Component
    Axis Y;
    //! Z Axis Component
    Axis Z;
    int buttons;
}

void  create_input_system(uint window_handle);

void  destroy_input_system();

MouseInputHandle  create_mouse_object(int buffered);

KeyboardInputHandle  create_keyboard_object(int buffered);

void  destroy_mouse_object(MouseInputHandle mouse_handle);

void  destroy_keyboard_object(KeyboardInputHandle keyboard_handle);

int  keyboard_is_key_down(KeyboardInputHandle keyboard_handle, KeyCode key_code);

int  keyboard_is_modifier_down(KeyboardInputHandle keyboard_handle, Key_Modifier key_modifier);

MouseState  mouse_get_state(MouseInputHandle mouse_handle);

void  mouse_set_buffered(MouseInputHandle mouse_handle, int buffered);

void  keyboard_set_buffered(KeyboardInputHandle keyboard_handle, int buffered);

void  keyboard_capture(KeyboardInputHandle keyboard_handle);

void  mouse_capture(MouseInputHandle mouse_handle);
