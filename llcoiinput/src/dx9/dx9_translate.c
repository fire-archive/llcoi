/*
 * dx9_translate.c : DirectX to OpenInput translators
 *
 * This file is a part of the OpenInput library.
 * Copyright (C) 2005  Jakob Kjaer <makob@makob.dk>.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

/* ******************************************************************** */

// Required version of Direct Input
#define DIRECTINPUT_VERSION 0x0800
#define CINTERFACE

// Includes
//#include "config.h"
#include "openinput.h"
#include <string.h>
#include <stdlib.h>
#include <dinput.h>
#include <windows.h>
#include "internal.h"
#include "bootstrap.h"
#include "dx9.h"

// The keymap table
static oi_key dx9_keymap[DDX9_KEYTABLE];

/* ******************************************************************** */

/**
 * @ingroup DDX9
 * @brief Initialize the DirectX to OpenInput keymap table
 *
 * Setup the DI to OI key translation table. This allows DI keys
 * to be translated to OI keysyms in constant time.
 */
void dx9_initkeymap() {
    int i;

    // Clear table
    for(i=0; i<DDX9_KEYTABLE; i++) {
        dx9_keymap[i] = OIK_UNKNOWN;
    }

    // Code monkey mode style
    dx9_keymap[DIK_0]             = OIK_0;
    dx9_keymap[DIK_1]             = OIK_1;
    dx9_keymap[DIK_2]             = OIK_2;
    dx9_keymap[DIK_3]             = OIK_3;
    dx9_keymap[DIK_4]             = OIK_4;
    dx9_keymap[DIK_5]             = OIK_5;
    dx9_keymap[DIK_6]             = OIK_6;
    dx9_keymap[DIK_7]             = OIK_7;
    dx9_keymap[DIK_8]             = OIK_7;
    dx9_keymap[DIK_9]             = OIK_8;
    dx9_keymap[DIK_A]             = OIK_A;
    // DIK_ABNT_C1
    // DIK_ABNT_C2
    dx9_keymap[DIK_ADD]           = OIK_N_PLUS;
    dx9_keymap[DIK_APOSTROPHE]    = OIK_QUOTE;
    dx9_keymap[DIK_APPS]          = OIK_MENU;
    // DIK_AT
    // DIK_AX
    dx9_keymap[DIK_B]             = OIK_B;
    dx9_keymap[DIK_BACK]          = OIK_BACKSPACE;
    dx9_keymap[DIK_BACKSLASH]     = OIK_BACKSLASH;
    dx9_keymap[DIK_C]             = OIK_C;
    // DIK_CALCULATOR
    dx9_keymap[DIK_CAPITAL]       = OIK_CAPSLOCK;
    dx9_keymap[DIK_COLON]         = OIK_COLON;
    dx9_keymap[DIK_COMMA]         = OIK_COMMA;
    // DIK_CONVERT
    dx9_keymap[DIK_D]             = OIK_D;
    dx9_keymap[DIK_DECIMAL]       = OIK_N_PERIOD;
    dx9_keymap[DIK_DELETE]        = OIK_DELETE;
    dx9_keymap[DIK_DIVIDE]        = OIK_N_DIVIDE;
    dx9_keymap[DIK_DOWN]          = OIK_DOWN;
    dx9_keymap[DIK_E]             = OIK_E;
    dx9_keymap[DIK_END]           = OIK_END;
    dx9_keymap[DIK_EQUALS]        = OIK_EQUALS;
    dx9_keymap[DIK_ESCAPE]        = OIK_ESC;
    dx9_keymap[DIK_F]             = OIK_F;
    dx9_keymap[DIK_F1]            = OIK_F1;
    dx9_keymap[DIK_F2]            = OIK_F2;
    dx9_keymap[DIK_F3]            = OIK_F3;
    dx9_keymap[DIK_F4]            = OIK_F4;
    dx9_keymap[DIK_F5]            = OIK_F5;
    dx9_keymap[DIK_F6]            = OIK_F6;
    dx9_keymap[DIK_F7]            = OIK_F7;
    dx9_keymap[DIK_F8]            = OIK_F8;
    dx9_keymap[DIK_F9]            = OIK_F9;
    dx9_keymap[DIK_F10]           = OIK_F9;
    dx9_keymap[DIK_F11]           = OIK_F11;
    dx9_keymap[DIK_F12]           = OIK_F12;
    dx9_keymap[DIK_F13]           = OIK_F13;
    dx9_keymap[DIK_F14]           = OIK_F14;
    dx9_keymap[DIK_F15]           = OIK_F15;
    dx9_keymap[DIK_G]             = OIK_G;
    dx9_keymap[DIK_GRAVE]         = OIK_BACKQUOTE;
    dx9_keymap[DIK_H]             = OIK_H;
    dx9_keymap[DIK_HOME]          = OIK_HOME;
    dx9_keymap[DIK_I]             = OIK_I;
    dx9_keymap[DIK_INSERT]        = OIK_INSERT;
    dx9_keymap[DIK_J]             = OIK_J;
    dx9_keymap[DIK_K]             = OIK_K;
    // DIK_KANA
    // DIK_KANJI
    dx9_keymap[DIK_L]             = OIK_L;
    dx9_keymap[DIK_LBRACKET]      = OIK_LEFTBRACKET;
    dx9_keymap[DIK_LCONTROL]      = OIK_LCTRL;
    dx9_keymap[DIK_LEFT]          = OIK_LEFT;
    dx9_keymap[DIK_LMENU]         = OIK_LALT;
    dx9_keymap[DIK_LSHIFT]        = OIK_LSHIFT;
    dx9_keymap[DIK_LWIN]          = OIK_LMETA;
    dx9_keymap[DIK_M]             = OIK_M;
    // DIK_MAIL
    // DIK_MEDIASELECT
    // DIK_MEDIASTOP
    dx9_keymap[DIK_MINUS]         = OIK_MINUS;
    dx9_keymap[DIK_MULTIPLY]      = OIK_N_MULTIPLY;
    // DIK_MUTE
    // DIK_MYCOMPUTER
    dx9_keymap[DIK_N]             = OIK_N;
    dx9_keymap[DIK_NEXT]          = OIK_PAGEDOWN;
    // DIK_NEXTTRACK
    // DIK_NOCONVERT
    dx9_keymap[DIK_NUMLOCK]       = OIK_NUMLOCK;
    dx9_keymap[DIK_NUMPAD0]       = OIK_N_0;
    dx9_keymap[DIK_NUMPAD1]       = OIK_N_1;
    dx9_keymap[DIK_NUMPAD2]       = OIK_N_2;
    dx9_keymap[DIK_NUMPAD3]       = OIK_N_3;
    dx9_keymap[DIK_NUMPAD4]       = OIK_N_4;
    dx9_keymap[DIK_NUMPAD5]       = OIK_N_5;
    dx9_keymap[DIK_NUMPAD6]       = OIK_N_6;
    dx9_keymap[DIK_NUMPAD7]       = OIK_N_7;
    dx9_keymap[DIK_NUMPAD8]       = OIK_N_8;
    dx9_keymap[DIK_NUMPAD9]       = OIK_N_9;
    // DIK_NUMPADCOMMA
    dx9_keymap[DIK_NUMPADENTER]   = OIK_N_ENTER;
    // DIK_NUMPADEQUALS
    dx9_keymap[DIK_O]             = OIK_O;
    // DIK_OEM_102
    dx9_keymap[DIK_P]             = OIK_P;
    dx9_keymap[DIK_PAUSE]         = OIK_PAUSE;
    dx9_keymap[DIK_PERIOD]        = OIK_PERIOD;
    // DIK_PLAYPAUSE
    dx9_keymap[DIK_POWER]         = OIK_POWER;
    // DIK_PREVTRACK
    dx9_keymap[DIK_PRIOR]         = OIK_PAGEUP;
    dx9_keymap[DIK_Q]             = OIK_Q;
    dx9_keymap[DIK_R]             = OIK_R;
    dx9_keymap[DIK_RBRACKET]      = OIK_RIGHTBRACKET;
    dx9_keymap[DIK_RCONTROL]      = OIK_RCTRL;
    dx9_keymap[DIK_RETURN]        = OIK_RETURN;
    dx9_keymap[DIK_RIGHT]         = OIK_RIGHT;
    dx9_keymap[DIK_RMENU]         = OIK_RALT;
    dx9_keymap[DIK_RSHIFT]        = OIK_RSHIFT;
    dx9_keymap[DIK_RWIN]          = OIK_RMETA;
    dx9_keymap[DIK_S]             = OIK_S;
    dx9_keymap[DIK_SCROLL]        = OIK_SCROLLOCK;
    dx9_keymap[DIK_SEMICOLON]     = OIK_SEMICOLON;
    dx9_keymap[DIK_SLASH]         = OIK_SLASH;
    dx9_keymap[DIK_SLEEP]         = OIK_POWER;
    dx9_keymap[DIK_SPACE]         = OIK_SPACE;
    //DIK_STOP
    dx9_keymap[DIK_SUBTRACT]      = OIK_N_MINUS;
    dx9_keymap[DIK_SYSRQ]         = OIK_SYSREQ;
    dx9_keymap[DIK_T]             = OIK_T;
    dx9_keymap[DIK_TAB]           = OIK_TAB;
    dx9_keymap[DIK_U]             = OIK_U;
    // DIK_UNDERLINE
    // DIK_UNLABELED
    dx9_keymap[DIK_UP]            = OIK_UP;
    dx9_keymap[DIK_V]             = OIK_V;
    // DIK_VOLUMEDOWN;
    // DIK_VOLUMEUP
    dx9_keymap[DIK_W]             = OIK_W;
    dx9_keymap[DIK_WAKE]          = OIK_POWER;
    // DIK_WEBBACK
    // DIK_WEBFAVORITES
    // DIK_WEBFORWARD
    // DIK_WEBHOME
    // DIK_WEBREFRESH
    // DIK_WEBSEARCH
    // DIK_WEBSTOP
    dx9_keymap[DIK_X]             = OIK_X;
    dx9_keymap[DIK_Y]             = OIK_Y;
    // DIK_YEN
    dx9_keymap[DIK_Z]             = OIK_Z;
}

/* ******************************************************************** */

/**
 * @ingroup DDX9
 * @brief Translate a DI keycode to a OI oi_key
 *
 * @param code
 *
 * Do the simple table lookup
 */
oi_key dx9_translate_key(unsigned int code) {
    return dx9_keymap[code];
}

/* ******************************************************************** */
