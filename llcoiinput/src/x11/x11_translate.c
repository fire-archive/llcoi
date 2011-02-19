/*
 * x11_translate.c : X11 keyboard translators
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

// Includes
//#include "config.h"
#include "openinput.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include "internal.h"
#include "x11.h"

// Globals
static oi_key x11_oddmap[256];
static oi_key x11_miscmap[256];

/* ******************************************************************** */

/**
 * @ingroup DX11
 * @brief Initialize the X11 to OpenInput keymap table
 *
 * To be able to do constant-time translations form X
 * keyboard button names to OpenInput symbolic keys,
 * we prepare two lookup-tables in this function.
 */
void x11_initkeymap() {
    int i;

    // Clear odd and misc keymaps
    for(i=0; i<TABLESIZE(x11_oddmap); i++) {
        x11_oddmap[i] = OIK_UNKNOWN;
        x11_miscmap[i] = OIK_UNKNOWN;
    }

    // Odd keymap (highbyte 0xFE)
#ifdef XK_dead_circumflex
    x11_oddmap[XK_dead_circumflex & 0xFF] = OIK_CARET;
#endif
#ifdef XK_ISO_Level3_Shift
    x11_oddmap[XK_ISO_Level3_Shift & 0xFF] = OIK_ALTGR;
#endif

    // Misc keymap (highbyte 0xFF)
    x11_miscmap[XK_BackSpace & 0xFF]    = OIK_BACKSPACE;
    x11_miscmap[XK_Tab & 0xFF]          = OIK_TAB;
    x11_miscmap[XK_Clear & 0xFF]        = OIK_CLEAR;
    x11_miscmap[XK_Return & 0xFF]       = OIK_RETURN;
    x11_miscmap[XK_Pause & 0xFF]        = OIK_PAUSE;
    x11_miscmap[XK_Escape & 0xFF]       = OIK_ESC;
    x11_miscmap[XK_Delete & 0xFF]       = OIK_DELETE;

    x11_miscmap[XK_KP_0 & 0xFF]         = OIK_N_0;
    x11_miscmap[XK_KP_1 & 0xFF]         = OIK_N_1;
    x11_miscmap[XK_KP_2 & 0xFF]         = OIK_N_2;
    x11_miscmap[XK_KP_3 & 0xFF]         = OIK_N_3;
    x11_miscmap[XK_KP_4 & 0xFF]         = OIK_N_4;
    x11_miscmap[XK_KP_5 & 0xFF]         = OIK_N_5;
    x11_miscmap[XK_KP_6 & 0xFF]         = OIK_N_6;
    x11_miscmap[XK_KP_7 & 0xFF]         = OIK_N_7;
    x11_miscmap[XK_KP_8 & 0xFF]         = OIK_N_8;
    x11_miscmap[XK_KP_9 & 0xFF]         = OIK_N_9;
    x11_miscmap[XK_KP_Insert & 0xFF]    = OIK_N_0;
    x11_miscmap[XK_KP_End & 0xFF]       = OIK_N_1;
    x11_miscmap[XK_KP_Down & 0xFF]      = OIK_N_2;
    x11_miscmap[XK_KP_Page_Down & 0xFF] = OIK_N_3;
    x11_miscmap[XK_KP_Left & 0xFF]      = OIK_N_4;
    x11_miscmap[XK_KP_Begin & 0xFF]     = OIK_N_5;
    x11_miscmap[XK_KP_Right & 0xFF]     = OIK_N_6;
    x11_miscmap[XK_KP_Home & 0xFF]      = OIK_N_7;
    x11_miscmap[XK_KP_Up & 0xFF]        = OIK_N_8;
    x11_miscmap[XK_KP_Page_Up & 0xFF]   = OIK_N_9;
    x11_miscmap[XK_KP_Delete & 0xFF]    = OIK_N_PERIOD;
    x11_miscmap[XK_KP_Decimal & 0xFF]   = OIK_N_PERIOD;
    x11_miscmap[XK_KP_Divide & 0xFF]    = OIK_N_DIVIDE;
    x11_miscmap[XK_KP_Multiply & 0xFF]  = OIK_N_MULTIPLY;
    x11_miscmap[XK_KP_Subtract & 0xFF]  = OIK_N_MINUS;
    x11_miscmap[XK_KP_Add & 0xFF]       = OIK_N_PLUS;
    x11_miscmap[XK_KP_Enter & 0xFF]     = OIK_N_ENTER;
    x11_miscmap[XK_KP_Equal & 0xFF]     = OIK_N_EQUALS;

    x11_miscmap[XK_Up & 0xFF]           = OIK_UP;
    x11_miscmap[XK_Down & 0xFF]         = OIK_DOWN;
    x11_miscmap[XK_Right & 0xFF]        = OIK_RIGHT;
    x11_miscmap[XK_Left & 0xFF]         = OIK_LEFT;
    x11_miscmap[XK_Insert & 0xFF]       = OIK_INSERT;
    x11_miscmap[XK_Home & 0xFF]         = OIK_HOME;
    x11_miscmap[XK_End & 0xFF]          = OIK_END;
    x11_miscmap[XK_Page_Up & 0xFF]      = OIK_PAGEUP;
    x11_miscmap[XK_Page_Down & 0xFF]    = OIK_PAGEDOWN;

    x11_miscmap[XK_F1 & 0xFF]           = OIK_F1;
    x11_miscmap[XK_F2 & 0xFF]           = OIK_F2;
    x11_miscmap[XK_F3 & 0xFF]           = OIK_F3;
    x11_miscmap[XK_F4 & 0xFF]           = OIK_F4;
    x11_miscmap[XK_F5 & 0xFF]           = OIK_F5;
    x11_miscmap[XK_F6 & 0xFF]           = OIK_F6;
    x11_miscmap[XK_F7 & 0xFF]           = OIK_F7;
    x11_miscmap[XK_F8 & 0xFF]           = OIK_F8;
    x11_miscmap[XK_F9 & 0xFF]           = OIK_F9;
    x11_miscmap[XK_F10 & 0xFF]          = OIK_F10;
    x11_miscmap[XK_F11 & 0xFF]          = OIK_F11;
    x11_miscmap[XK_F12 & 0xFF]          = OIK_F12;
    x11_miscmap[XK_F13 & 0xFF]          = OIK_F13;
    x11_miscmap[XK_F14 & 0xFF]          = OIK_F14;
    x11_miscmap[XK_F15 & 0xFF]          = OIK_F15;

    x11_miscmap[XK_Num_Lock & 0xFF]     = OIK_NUMLOCK;
    x11_miscmap[XK_Caps_Lock & 0xFF]    = OIK_CAPSLOCK;
    x11_miscmap[XK_Scroll_Lock & 0xFF]  = OIK_SCROLLOCK;
    x11_miscmap[XK_Shift_R & 0xFF]      = OIK_RSHIFT;
    x11_miscmap[XK_Shift_L & 0xFF]      = OIK_LSHIFT;
    x11_miscmap[XK_Control_R & 0xFF]    = OIK_RCTRL;
    x11_miscmap[XK_Control_L & 0xFF]    = OIK_LCTRL;
    x11_miscmap[XK_Alt_R & 0xFF]        = OIK_RALT;
    x11_miscmap[XK_Alt_L & 0xFF]        = OIK_LALT;
    x11_miscmap[XK_Meta_R & 0xFF]       = OIK_RMETA;
    x11_miscmap[XK_Meta_L & 0xFF]       = OIK_LMETA;
    x11_miscmap[XK_Super_L & 0xFF]      = OIK_LWINDOWS;
    x11_miscmap[XK_Super_R & 0xFF]      = OIK_RWINDOWS;
    x11_miscmap[XK_Mode_switch & 0xFF]  = OIK_ALTGR;
    x11_miscmap[XK_Multi_key & 0xFF]    = OIK_COMPOSE;

    x11_miscmap[XK_Help & 0xFF]         = OIK_HELP;
    x11_miscmap[XK_Print & 0xFF]        = OIK_PRINT;
    x11_miscmap[XK_Sys_Req & 0xFF]      = OIK_SYSREQ;
    x11_miscmap[XK_Break & 0xFF]        = OIK_BREAK;
    x11_miscmap[XK_Menu & 0xFF]         = OIK_MENU;
    x11_miscmap[XK_Hyper_R & 0xFF]      = OIK_MENU;
}

/* ******************************************************************** */

/**
 * @ingroup DX11
 * @brief Perform a full keyboard state update
 *
 * @param dev pointer to device interface
 * @param d display handle
 * @param keyvec X keyboard vector
 *
 * When OpenInput starts, and when the hook-window
 * receives focus, the keyboard state is unknown since
 * other applications may have altered things.
 * Because of this, we have to sync the OpenInput state
 * with the X server. It's cumbersome, but required!
 */
void x11_keystate(oi_device *dev, Display *d, char *keyvec) {
    char keyret[32];
    KeyCode xcode[OIK_LAST];
    Window w;
    int i;
    int j;
    unsigned int mod;
    unsigned int mask;
    char newstate[OIK_LAST];
    char *curstate;

    debug("x11_keystate");

    // Fetch pressed keys from X if not supplied
    if(!keyvec) {
        XQueryKeymap(d, keyret);
        keyvec = keyret;
    }

    // Query modifiers
    mod = OIM_NONE;
    if(XQueryPointer(d, DefaultRootWindow(d), &w, &w,
                     &i, &j, &i, &j, &mask)) {
        x11_private *priv;
        priv = (x11_private*)dev->private;

        // Capslock
        if(mask & LockMask) {
            mod |= OIM_CAPSLOCK;
        }
        if(mask & priv->mask_altgr) {
            mod |= OIM_ALTGR;
        }
        if(mask & priv->mask_num) {
            mod |= OIM_NUMLOCK;
        }
    }

    // Prepare new and current keystates
    memset(newstate, 0, sizeof(newstate));
    curstate = oi_key_keystate(dev->index, NULL);

    // Check each bit in the 32 bytes of the X keystate
    for(i=0; i<32; i++) {
        // No bits set
        if(!keyvec[i]) {
            continue;
        }

        // Scan bits
        for(j=0; j<8; j++) {
            if(keyvec[i] & (1<<j)) {
                // Find out what that key is
                oi_keysym sks;
                KeyCode kc;

                kc = i << 3 | j;
                x11_translate(d, NULL, kc, &sks);

                newstate[sks.sym] = TRUE;
                xcode[sks.sym] = kc;
            }
        }
    }

    // Set state and fetch modifiers
    for(i=OIK_FIRST+1; i<OIK_LAST; i++) {
        if(newstate[i]) {

            // Store new state of key
            curstate[i] = newstate[i];

            // Fetch normal modifiers
            switch(i) {
            case OIK_LSHIFT:
                mod |= OIM_LSHIFT;
                break;

            case OIK_RSHIFT:
                mod |= OIM_RSHIFT;
                break;

            case OIK_LCTRL:
                mod |= OIM_LCTRL;
                break;

            case OIK_RCTRL:
                mod |= OIM_RCTRL;
                break;

            case OIK_LALT:
                mod |= OIM_LALT;
                break;

            case OIK_RALT:
                mod |= OIM_RALT;
                break;

            case OIK_LMETA:
                mod |= OIM_LMETA;
                break;

            case OIK_RMETA:
                mod |= OIM_RMETA;
                break;

            default:
                break;
            }
        }
    }

    // Correct for locking modifiers
    if(mod & OIM_CAPSLOCK) {
        curstate[OIK_CAPSLOCK] = TRUE;
    }
    else {
        curstate[OIK_CAPSLOCK] = FALSE;
    }

    if(mod & OIM_NUMLOCK) {
        curstate[OIK_NUMLOCK] = TRUE;
    }
    else {
        curstate[OIK_NUMLOCK] = FALSE;
    }

    keyboard_setmodifier(dev->index, mod);
}

/* ******************************************************************** */

/**
 * @ingroup DX11
 * @brief Translate X11 keysym to OpenInput keysym
 *
 * @param d display handle
 * @param xkey X keyboard event
 * @param kc X keycode
 * @param keysym OpenInput symbolic key
 * @returns OpenInput symbolic key
 *
 * Here's where the X key is translate to the corresponding
 * OpenInput keysym. Since we use lookup-tables, it's
 * actually quite fast (and easy), though it requires some
 * deep knowledge of how X keycodes and X keysyms are
 * composed (eg. that X has a different keymaps).
 */
oi_keysym *x11_translate(Display *d, XKeyEvent *xkey,
                                KeyCode kc, oi_keysym *keysym) {
    KeySym xsym;

    // Basic OI keysym
    keysym->scancode = kc;
    keysym->sym = OIK_UNKNOWN;
    keysym->mod = OIM_NONE;

    // Handle X keysym
    xsym = XKeycodeToKeysym(d, kc, 0);
    if(xsym) {
        // Keymap information is in the high byte
        switch(xsym >> 8) {

        case 0x00: // Latin 1
        case 0x01: // Latin 2
        case 0x02: // Latin 3
        case 0x03: // Latin 4
        case 0x04: // Katakana
        case 0x05: // Arabic
        case 0x06: // Cyrillic
        case 0x07: // Greek
        case 0x08: // Technical
        case 0x0A: // Publishing
        case 0x0C: // Hebrew
        case 0x0D: // Thai
            // Normal ASCII keymap
            keysym->sym = (oi_key)(xsym & 0xFF);

            // Fix lowercase
            if((keysym->sym >= 'A') && (keysym->sym <= 'Z')) {
                keysym->sym += 'a'-'A';
            }
            break;

        case 0xFE: // Odd
            keysym->sym = x11_oddmap[xsym & 0xFF];
            break;

        case 0xFF: // Misc
            keysym->sym = x11_miscmap[xsym & 0xFF];
            break;

        default: // Unhandled
            debug("x11_translate: unhandled map 0x%04x", (unsigned int)xsym);
            break;
        }
    }

    // Handle special scancodes
    else {
        switch(kc) {
        case 115:
            keysym->sym = OIK_LWINDOWS;
            break;

        case 116:
            keysym->sym = OIK_RWINDOWS;
            break;

        case 117:
            keysym->sym = OIK_MENU;
            break;
        }
    }

    // Done
    return keysym;
}

/* ******************************************************************** */

/**
 * @ingroup DX11
 * @brief Query the X server for various modifier keys
 *
 * @param d display handle
 * @param dev pointer to device interface
 *
 * Apparantly, the bitmasks of certain modifier keys (shift, meta, etc.)
 * are variable. Because of this, we have to find out which modifier
 * corresponds to which bit.
 */
void x11_modmasks(Display *d, oi_device *dev) {
    XModifierKeymap *xmods;
    int i;
    int j;
    unsigned int n;
    unsigned int mask;
    x11_private *priv;

    priv = (x11_private*)dev->private;

    // Query
    xmods = XGetModifierMapping(d);
    n = xmods->max_keypermod;

    // Decode (lifted from SDL, thanks!)
    for(i = 3; i < 8; i++) {
        for(j = 0; j < n; j++) {

            KeyCode kc = xmods->modifiermap[i * n + j];
            KeySym ks = XKeycodeToKeysym(d, kc, 0);

            mask = 1 << i;

            switch(ks) {
            case XK_Num_Lock:
                priv->mask_num = mask;
                break;

            case XK_Alt_L:
                priv->mask_lalt = mask;
                break;

            case XK_Alt_R:
                priv->mask_ralt = mask;
                break;

            case XK_Meta_L:
                priv->mask_lmeta = mask;
                break;

            case XK_Meta_R:
                priv->mask_rmeta = mask;
                break;

            case XK_Mode_switch:
                priv->mask_altgr = mask;
                break;
            }
        }
    }
    XFreeModifiermap(xmods);
}

/* ******************************************************************** */
