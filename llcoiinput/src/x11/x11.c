/*
 * x11.c : X11 utility functions (bootstrapping, etc.)
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
#include <unistd.h>
#include <X11/Xlib.h>
#include "internal.h"
#include "bootstrap.h"
#include "x11.h"

/**
 * @ingroup Drivers
 * @defgroup DX11 X11 device driver
 * @brief X11 window system driver
 *
 * The X11 window system driver handles mice and
 * keyboards under X11. It also supports hooking
 * into an existing window, grabbing of both
 * input (keyboard) and pointer (mouse). Also,
 * the mouse cursor can be hidden and shown on
 * demand.
 */

// Bootstrap global
oi_bootstrap x11_bootstrap = {
    "x11",
    "X11 Window system",
    OI_PRO_KEYBOARD | OI_PRO_MOUSE | OI_PRO_WINDOW,
    x11_avail,
    x11_device,
};

/* ******************************************************************** */

/**
 * @ingroup DX11
 * @brief Check connection to the X11 server
 *
 * @param flags library initialization flags, see @ref PFlags
 * @returns errorcode, see @ref PErrors
 *
 * This is a bootstrap function.
 *
 * We try to open a dummy-connection to the X-server. That
 * tells us of the X11 system can be used.
 */
int x11_avail(unsigned int flags) {
    Display *disp;

    debug("x11_avail");

    // Check flags
    if(flags & OI_FLAG_NOWINDOW) {
        return FALSE;
    }

    // Check for X11 existence
    disp = XOpenDisplay(NULL);
    if(disp != NULL) {
        // Got it, close display again
        XCloseDisplay(disp);
        return TRUE;
    }
    else {
        return FALSE;
    }
}

/* ******************************************************************** */

/**
 * @ingroup DX11
 * @brief Create X11 device driver interface
 *
 * @returns pointer to device interface, see @ref IDevstructs
 *
 * This is a bootstrap function.
 *
 * Create the internal data structure and the device interface.
 */
oi_device *x11_device() {
    oi_device *dev;
    x11_private *priv;

    debug("x11_device");

    // Alloc
    dev = (oi_device*)malloc(sizeof(oi_device));
    priv = (x11_private*)malloc(sizeof(x11_private));
    if((dev == NULL) || (priv == NULL)) {
        debug("x11_device: device creation failed");
        if(dev) {
            free(dev);
        }
        if(priv) {
            free(priv);
        }
        return NULL;
    }

    // Clear structures
    memset(dev, 0, sizeof(oi_device));
    memset(priv, 0, sizeof(x11_private));

    // Set members
    dev->private = priv;
    dev->init = x11_init;
    dev->destroy = x11_destroy;
    dev->process = x11_process;
    dev->grab = x11_grab;
    dev->hide = x11_hidecursor;
    dev->warp = x11_warp;
    dev->winsize = x11_winsize;

    // Done
    return dev;
}

/* ******************************************************************** */

/**
 * @ingroup DX11
 * @brief Initialize the X11 driver
 *
 * @param dev pointer to created device interface
 * @param window_id window hook parameters, see @ref PWindow
 * @param flags initialization flags, see @ref PFlags
 * @returns errorcode, see @ref PErrors
 *
 * This is a device interface function.
 *
 * Initializes the window hook to the X server and initializes
 * the private data structure. The latter includes generation
 * of the invisible mouse cursor and lookup of variable
 * key modifier masks. Also, the protocol atom for "close window"
 * is fetched, so window-manager close events can be catched.
 *
 * The X11 keymap is also read, and a full reset is performed
 * in order to sync states between X11 and OpenInput.
 */
int x11_init(oi_device *dev, char *window_id, unsigned int flags) {
    x11_private *priv;

    priv = (x11_private*)dev->private;
    debug("x11_init");

    // Parse the window_id flags
    priv->disp = (Display*)device_windowid(window_id, OI_I_CONN);
    priv->screen = (Screen*)device_windowid(window_id, OI_I_SCRN);
    priv->win = (Window)device_windowid(window_id, OI_I_WINID);

    // We require conn and winid parameters
    if(!(priv->disp) || !(priv->win)) {
        debug("x11_init: conn (c) and winid (w) parameters required\n");
        return OI_ERR_NO_DEVICE;
    }

    // Install error handlers
    XSetErrorHandler(x11_error);
    XSetIOErrorHandler(x11_fatal);

    // Initialize blank-cursor, keymapper table, modifier mask and key state
    priv->cursor = x11_mkcursor(priv->disp, priv->win);
    priv->relative = 0;
    x11_initkeymap();
    x11_modmasks(priv->disp, dev);
    x11_keystate(dev, priv->disp, NULL);

    // Start receiving events
    XSelectInput(priv->disp, priv->win, FocusChangeMask | KeyPressMask |
                 KeyReleaseMask | PropertyChangeMask | StructureNotifyMask |
                 KeymapStateMask | ButtonPressMask | ButtonReleaseMask |
                 PointerMotionMask | EnterWindowMask | LeaveWindowMask );

    // Get "close window" window manager protocol atom
    priv->wm_delete_window = XInternAtom(priv->disp, "WM_DELETE_WINDOW", False);
    XSetWMProtocols(priv->disp, priv->win, &(priv->wm_delete_window), 1);

    debug("x11_init: initialized");

    return OI_ERR_OK;
}

/* ******************************************************************** */

/**
 * @ingroup DX11
 * @brief Destroy the X11 device driver
 *
 * @param dev pointer to device interface
 * @returns errorcode, see @ref PErrors
 *
 * This is a device interface function.
 *
 * Shutdown the X11 driver by releasing all
 * allocated memory and the hidden cursor.
 */
int x11_destroy(oi_device *dev) {
    x11_private *priv;

    debug("x11_destroy");

    if(dev) {
        priv = (x11_private*)dev->private;

        // Private members
        if(priv) {
            if(priv->cursor) {
                XFreeCursor(priv->disp, priv->cursor);
            }
            free(priv);
        }

        // Device struct
        free(dev);
        dev = NULL;
    }

    return OI_ERR_OK;
}

/* ******************************************************************** */

/**
 * @ingroup DX11
 * @brief Process events
 *
 * @param dev pointer to device interface
 *
 * This is a device interface function.
 *
 * Process pending X11 events, and pump these
 * into the OpenInput queue. The real functionality
 * is handled in the dispatcher.
 */
void x11_process(oi_device *dev) {
    x11_private *priv;

    priv = (x11_private*)dev->private;

    // Process all pending events
    while(oi_runstate() && x11_pending(priv->disp)) {
        x11_dispatch(dev, priv->disp);
    }
}

/* ******************************************************************** */

/**
 * @ingroup DX11
 * @brief Grab/release mouse and keyboard
 *
 * @param dev pointer to device interface.
 * @param on true (1) turns on grab, false (0) releases grab
 * @returns errorcode, see @ref PErrors
 *
 * This is a device interface function.
 *
 * Grab or release input (keyboard) and pointer (mouse) inside
 * the hook-window.
 */
int x11_grab(oi_device *dev, int on) {
    x11_private *priv;
    int i;

    priv = (x11_private*)dev->private;
    debug("x11_grab: state:%i", on);

    if(on) {
        // Raise window and focus it
        XRaiseWindow(priv->disp, priv->win);
        XSetInputFocus(priv->disp, priv->win, RevertToParent, CurrentTime);

        // Grab input
        XGrabKeyboard(priv->disp, priv->win, TRUE,
                      GrabModeAsync, GrabModeAsync, CurrentTime);

        // Wait for succesfull grabbing of mouse
        while(1) {
            i = XGrabPointer(priv->disp, priv->win, TRUE,
                             ButtonPressMask | ButtonReleaseMask | PointerMotionMask,
                             GrabModeAsync, GrabModeAsync,
                             priv->win, None, CurrentTime);
            if(i == GrabSuccess) {
                break;
            }
            usleep(OI_SLEEP);
        }

        // Set flag for possible relative mouse
        priv->relative |= DX11_GRAB;
    }
    else {
        // Simply ungrab both
        XUngrabKeyboard(priv->disp, CurrentTime);
        XUngrabPointer(priv->disp, CurrentTime);

        // Fix relative mouse motion
        priv->relative &= ~DX11_GRAB;
    }

    return OI_ERR_OK;
}

/* ******************************************************************** */

/**
 * @ingroup DX11
 * @brief Show/hide mouse pointer
 *
 * @param dev pointer to device interface.
 * @param on true (1) hides cursor, false (0) shows cursor
 * @returns errorcode, see @ref PErrors
 *
 * This is a device interface function.
 *
 * If we're to hide the cursor, switch the current cursor
 * in the hook-window to the invisible one (which is to be
 * found in the private data). Otherwise, we simply revert
 * to the default X pointer.
 */
int x11_hidecursor(oi_device *dev, int on) {
    x11_private *priv;

    debug("x11_hidecursor: state:%i", on);
    priv = (x11_private*)dev->private;

    // Hide - set blank cursor
    if(on) {
        XDefineCursor(priv->disp, priv->win, priv->cursor);
        priv->relative |= DX11_HIDE;
    }

    // Show - set default cursor
    else {
        XDefineCursor(priv->disp, priv->win, None);
        priv->relative &= ~DX11_HIDE;
    }

    return OI_ERR_OK;
}

/* ******************************************************************** */

/**
 * @ingroup DX11
 * @brief Warp mouse pointer
 *
 * @param dev pointer to device interface.
 * @param x pointer to horizontal position
 * @param y pointer to vertical position
 * @returns errorcode, see @ref PErrors
 *
 * This is a device interface function.
 *
 * Warp (move) the mouse pointer to the given
 * absolute coordinate within the hook-window.
 */
int x11_warp(oi_device *dev, int x, int y) {
    x11_private *priv;

    priv = (x11_private*)dev->private;

    // Simply go to warp speed
    XWarpPointer(priv->disp, None, priv->win, 0, 0, 0, 0, x, y);
    XSync(priv->disp, False);
    priv->lastx = x;
    priv->lasty = y;

    return OI_ERR_OK;
}

/* ******************************************************************** */

/**
 * @ingroup DX11
 * @brief Get window size
 *
 * @param dev pointer to device interface.
 * @param w pointer to horizontal size
 * @param h pointer to vertical size
 * @returns errorcode, see @ref PErrors
 *
 * This is a device interface function.
 *
 * Read the current size of the hook-window, update
 * the internal state and return the width and height.
 */
int x11_winsize(oi_device *dev, int *w, int *h) {
    x11_private *priv;
    XWindowAttributes attr;

    priv = (x11_private*)dev->private;

    XGetWindowAttributes(priv->disp, priv->win, &attr);

    // Update state
    priv->width = attr.width;
    priv->height = attr.height;

    // Safely store data
    if(w) {
        *w = attr.width;
    }
    if(h) {
        *h = attr.height;
    }

    debug("x11_winsize: width:%i height:%i", *w, *h);

    return OI_ERR_OK;
}

/* ******************************************************************** */

/**
 * @ingroup DFoo
 * @brief Reset internal state
 *
 * @param dev pointer to device interface
 * @returns errorcode, see @ref PErrors
 *
 * This is a device interface function.
 *
 * Force full resync the driver and device state.
 */
int x11_reset(oi_device *dev) {
    x11_private *priv;
    priv = (x11_private*)dev->private;
    debug("x11_reset");

    // Show cursor, grab off
    x11_grab(dev, FALSE);
    x11_hidecursor(dev, FALSE);

    // Query for modifiers
    x11_modmasks(priv->disp, dev);

    // Sync keyboard state
    x11_keystate(dev, priv->disp, NULL);

    // Window size
    x11_winsize(dev, NULL, NULL);

    // Center mouse cursor
    x11_warp(dev, priv->height/2, priv->width/2);

    return OI_ERR_OK;
}

/* ******************************************************************** */

/**
 * @ingroup DX11
 * @brief X11 non-fatal error handler
 *
 * @param d display handle
 * @param e pointer to X11 error
 * @returns ignored
 *
 * Reports non-fatal errors to standard-error when
 * OpenInput is compiled in debug-mode.
 */
int x11_error(Display *d, XErrorEvent *e) {
    debug("x11_error: code %u", e->error_code);

    return 0;
}

/* ******************************************************************** */

/**
 * @ingroup DX11
 * @brief X11 fatal I/O error handler
 *
 * @param d display handle
 * @returns ignored
 *
 * Called on fatal X errors - this is typically due to a lost
 * connection to the X server, in which case the application
 * should terminate.
 */
int x11_fatal(Display *d) {
    debug("x11_fatal: fatal I/O error");

    //FIXME: Send a quit-event?
    return OI_ERR_OK;
}

/* ******************************************************************** */

/**
 * @ingroup DX11
 * @brief Create the invisible pointer
 *
 * @param d display handle
 * @param w window handle
 * @returns pointer handle
 *
 * Create an X pointer from a on-the-fly generated blank
 * pixmap.
 */
Cursor x11_mkcursor(Display *d, Window w) {
    Pixmap pixmap;
    XColor color;
    Colormap colmap;
    Cursor cursor;

    // Make 1x1 pixmap
    pixmap = XCreatePixmap(d, DefaultRootWindow(d), 1, 1, 1);

    // Get the color black
    colmap = XCreateColormap(d, DefaultScreen(d),
                             DefaultVisual(d, DefaultScreen(d)),
                             AllocNone);
    XParseColor(d, colmap, "black", &color);

    // Create the cursor
    cursor = XCreatePixmapCursor(d, pixmap, pixmap,
                                 &color, &color, 1, 1);

    // Free and sync
    XFreePixmap(d, pixmap);
    XFreeColormap(d, colmap);
    XSync(d, False);

    return cursor;
}

/* ******************************************************************** */
