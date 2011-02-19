/*
 * dx9_events.c : DirectX 9 event dispatchers
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

// Globals
static dx9_private *private = NULL;
static WNDPROC old_wndproc = NULL;

/* ******************************************************************** */

/**
 * @ingroup DDX9
 * @brief Keyboard event dispatcher
 *
 * @param dev device pointer to device interface
 * @param data DI object data buffer
 * @param entries number of entries in data buffer
 *
 * The DI device has just been polled and the information is
 * now pending in the data buffer. Parse the input and inject
 * events into the OpenInput system.
 */
void dx9_keyboard_dispatch(oi_device *dev, DIDEVICEOBJECTDATA *data, unsigned int entries) {
    dx9_private *priv;
    unsigned int i;
    unsigned int code;
    oi_keysym keysym;
    char state;

    debug("dx9_keyboard_dispatch");

    priv = (dx9_private*)dev->private;

    for(i=0; i<entries; i++) {
        code = data[i].dwOfs;
        state = (char)data[i].dwData & 0x80;

        keysym.sym = dx9_translate_key(code);
        keysym.mod = OIM_NONE;
        keysym.scancode = code;

        keyboard_update(dev->index, &keysym, state, TRUE);
    }
}

/* ******************************************************************** */

/**
 * @ingroup DDX9
 * @brief Mouse event dispatcher
 *
 * @param dev device pointer to device interface
 * @param data DI object data buffer
 * @param entries number of entries in data buffer
 *
 * The DI device has just been polled and the information is
 * now pending in the data buffer. Parse the input and inject
 * events into the OpenInput system.
 */
void dx9_mouse_dispatch(oi_device *dev, DIDEVICEOBJECTDATA *data, unsigned int entries) {
    dx9_private *priv;
    unsigned int i;
    int x;
    int y;

    priv = (dx9_private*)dev->private;
    x = 0;
    y = 0;

    debug("dx9_mouse_dispatch");

    for(i=0; i<entries; i++) {

        switch(data[i].dwOfs) {
            case DIMOFS_BUTTON0:
                // Left button
                mouse_button(dev->index, OIP_BUTTON_LEFT, ((char)data[i].dwData & 0x80), TRUE);
                break;

            case DIMOFS_BUTTON1:
                // Middle button
                mouse_button(dev->index, OIP_BUTTON_MIDDLE, ((char)data[i].dwData & 0x80), TRUE);
                break;

            case DIMOFS_BUTTON2:
                // Right button
                mouse_button(dev->index, OIP_BUTTON_RIGHT, ((char)data[i].dwData & 0x80), TRUE);
                break;

            case DIMOFS_X:
                // Horizontal movement - send if axis already changed
                if(x) {
                    mouse_move(dev->index, x, 0, FALSE, TRUE);
                    x = 0;
                }
                x = data[i].dwData;
                break;

            case DIMOFS_Y:
                // Vertical movement - send if axis already changed
                if(y) {
                    mouse_move(dev->index, 0, y, FALSE, TRUE);
                    x = 0;
                }
                y = data[i].dwData;
                break;

            case DIMOFS_Z:
                // Wheel movement
                if(data[i].dwData > 0) {
                    mouse_button(dev->index, OIP_WHEEL_UP, TRUE, TRUE);
                }
                else {
                    mouse_button(dev->index, OIP_WHEEL_DOWN, TRUE, TRUE);
                }
                break;

            default:
                debug("dx9_mouse_dispatch: unknown offset");
                break;
        }

        // Send mouse event if both axes changed
        if(x && y) {
            mouse_move(dev->index, x, y, FALSE, TRUE);
            x = 0;
            y = 0;
        }
    }

    // All events handled, make sure nothing remains
    if(x || y) {
        mouse_move(dev->index, x, y, FALSE, TRUE);
    }
}

/* ******************************************************************** */

/**
 * @ingroup DDX9
 * @brief Joystick event dispatcher
 *
 * @param dev device pointer to device interface
 * @param data DI object data buffer
 * @param entries number of entries in data buffer
 *
 * The DI joystick has just been polled and the information is
 * now pending in the data buffer. Parse the input and inject
 * events into the OpenInput system.
 *
 * Axis and buttons are two different things, so do
 * a simple check and use the correct state injector.
 */
void dx9_joy_dispatch(oi_device *dev, DIDEVICEOBJECTDATA *data, unsigned int entries) {
    dx9_private *priv;
    unsigned int i;

    priv = (dx9_private*)dev->private;

    debug("dx9_joystick_dispatch");

    for(i=0; i<entries; i++) {
        if(dev->joyconfig->kind[data[i].dwOfs] == OIJ_GEN_BUTTON) {
            // Button event
            joystick_button(dev->index,
                            (char)data[i].dwOfs,
                            ((char)data[i].dwData & 0x80),
                            TRUE);
        }
        else {
            // Joystick event - axes are absolute
            joystick_axis(dev->index,
                          (unsigned char)data[i].dwOfs,
                          (char)data[i].dwData,
                          FALSE,
                          TRUE);
        }
    }
}

/* ******************************************************************** */

/**
 * @ingroup DWin32
 * @brief The window procedure
 *
 * @param hwnd window handle
 * @param msg message id
 * @param wparam word-length paramter for message
 * @param lparam long-length paramter for message
 * @returns 0 if message was handled, otherwise undefined
 *
 * This is the main Windows callback procedure, which is called
 * whenever the application dispatches messages. This is where
 * we handle all the nitty-gritty inputs!
 *
 * The events we handle are eaten. Neither the default nor the
 * old application handler is called if we catch an event.
 */
LRESULT CALLBACK dx9_wndproc(HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam) {
    // Handle
    switch(msg) {

        // Catch keyboard and mouse events
        case WM_SYSKEYDOWN:
        case WM_KEYDOWN:
        case WM_SYSKEYUP:
        case WM_KEYUP:
        case WM_MOUSEMOVE:
        case WM_LBUTTONDOWN:
        case WM_LBUTTONUP:
        case WM_MBUTTONDOWN:
        case WM_MBUTTONUP:
        case WM_RBUTTONDOWN:
        case WM_RBUTTONUP:
#ifdef WM_MOUSEWHEEL
        case WM_MOUSEWHEEL:
#endif
#ifdef WM_MOUSELEAVE
        case WM_MOUSELEAVE:
#endif
            // Simply ignore the event
            return 0;


            // Receive/loose focus
        case WM_ACTIVATE:
            //FIXME We shold probably do a Acquire for all DI devices here
            return 0;

            // Window moved or resized
        case WM_SIZE:
        case WM_MOVE:
            {
                debug("win32_wndproc: size/move");
                //dx9_movesize();
            }
            return 0;

            // Close
        case WM_CLOSE:
        case WM_DESTROY:
            {
                oi_event ev;
                debug("win32_wndproc: close/destroy");
                ev.type = OI_QUIT;
                queue_add(&ev);
                PostQuitMessage(0);
            }
            return 0;


            // Erase background
        case WM_ERASEBKGND:
            {
                oi_event ev;
                debug("win32_wndproc: erase background");
                ev.type = OI_EXPOSE;
                queue_add(&ev);
            }
            return 0;


            // Unhandled event, send to old handler
        default:
            if(old_wndproc) {
                return CallWindowProc(old_wndproc, hwnd, msg,
                                      wparam, lparam);
            }
            break;
    }

    // The catch-all default handler
    return DefWindowProc(hwnd, msg, wparam, lparam);
}

/* ******************************************************************** */

/**
 * @ingroup DDX9
 * @brief Window was moved or resized
 *
 * When the user moved or resized the window, get the
 * new window sizes.
 */
//void win32_movesize() {
    /*
    RECT rc;

    // Store new boundaries for mouse grabbing
    GetClientRect(private->hwnd, &private->rect);

    // Did anything change?
    if((private->width == private->rect.right) &&
       (private->height == private->rect.bottom)) {
        return;
    }

    // Update and post event
    private->width = rc.right;
    private->height = rc.bottom;
    appstate_resize(device->index, private->width, private->height, TRUE);
    */
//}
