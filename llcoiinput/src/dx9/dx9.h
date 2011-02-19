/*
 * dx9.h : DirectX 9 utility functions (bootstrapping, etc.)
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

#ifndef _OPENINPUT_DX9_H_
#define _OPENINPUT_DX9_H_

/* ******************************************************************** */

// Forward definitions
struct dx9_uid;

/* ******************************************************************** */

// Bootstrap entries
int dx9_avail(unsigned int flags);
oi_device *dx9_device();

/* ******************************************************************** */

// Device entries
int dx9_init(oi_device *dev, char *window_id, unsigned int flags);
int dx9_destroy(oi_device *dev);
void dx9_process(oi_device *dev);
int dx9_grab(oi_device *dev, int on);
int dx9_warp(oi_device *dev, int x, int y);
int dx9_winsize(oi_device *dev, int *w, int *h);
int dx9_reset(oi_device *dev);

/* ******************************************************************** */

// Misc local functions
LPDIENUMDEVICESCALLBACKA dx9_devenum;
LPDIENUMDEVICEOBJECTSCALLBACKA dx9_joyenum;
void dx9_keyboard_dispatch(oi_device *dev, DIDEVICEOBJECTDATA *data, unsigned int entries);
void dx9_mouse_dispatch(oi_device *dev, DIDEVICEOBJECTDATA *data, unsigned int entries);
void dx9_joy_dispatch(oi_device *dev, DIDEVICEOBJECTDATA *data, unsigned int entries);
LRESULT CALLBACK dx9_wndproc(HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam);
oi_key dx9_translate_key(unsigned int code);

/* ******************************************************************** */

/**
 * @ingroup DDX9
 * @brief GUID linked list element
 *
 * As DI uses callbacks for the device enumeration and we need
 * to have each DI device as a seperate OI device, a placeholder
 * for the device identifiers is needed. This linked list structure
 * holds these GUIDs
 */
typedef struct dx9_uid {
    GUID guid;                      /**< Unique DI device indentifier */
    unsigned int provides;          /**< OI provides flags */
    char *name;                     /**< Device name */
    struct dx9_uid *next;           /**< Next pointer */
} dx9_uid;


/**
 * @ingroup DDX9
 * @brief Direct X driver private instance data
 *
 * Private data for the DirectX driver. This includes the
 * primary window handle and the DirectInput interface objects.
 * Windows/DirectInput only supports a single mouse and a single
 * keyboard (which is lame), but multiple joysticks.
 */
typedef struct dx9_private {
    HWND hwnd;                      /**< Window handle */
    LPDIRECTINPUTDEVICE8A obj;      /**< The DI object */
    char exclusive;                 /**< Exclusive flag (grabbed+hidden) */
    int height;                     /**< Window height */
    int width;                      /**< Window width */
    int lastx;                      /**< Old horizontal mouse position */
    int lasty;                      /**< Old vertical mouse position */
} dx9_private;

/* ******************************************************************** */

/**
 * @ingroup DX11
 * @{
 */
#define DDX9_BUFFER_SIZE 16         /**< Events to buffer for each device */
#define DDX9_KEYTABLE 256           /**< Number of DX9 keys */
/** @} */

/* ******************************************************************** */

#endif
