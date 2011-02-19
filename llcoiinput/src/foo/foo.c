/*
 * foo.c : Foo (test) utility functions (bootstrapping, etc.)
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
#include "config.h"
#include "openinput.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "internal.h"
#include "bootstrap.h"
#include "foo.h"

/**
 * @ingroup Drivers
 * @defgroup DFoo Foo test driver
 *
 * The foo test driver can be used to test some
 * basic functionality of OpenInput. Under normal
 * circumstances you really do NOT want to compile
 * this driver--the default is not to use the foo
 * device driver test.
 */

// Bootstrap global
oi_bootstrap foo_bootstrap = {
    "foo",
    "Foo test system",
    OI_PRO_UNKNOWN,
    foo_avail,
    foo_device
};


/* ******************************************************************** */

/**
 * @ingroup DFoo
 * @brief Check availablity of foo device
 *
 * @param flags initialization flags, see @ref PFlags
 * @returns true (1) if device is present, false (0) otherwise
 *
 * This is a bootstrap function.
 *
 * The foo device driver always exists (if compiled in).
 */
int foo_avail(unsigned int flags) {
    debug("foo_avail");

    return TRUE;
}

/* ******************************************************************** */

/**
 * @ingroup DFoo
 * @brief Create foo device structure
 *
 * @returns pointer to allocated device interface
 *
 * This is a bootstrap function.
 *
 * Alloc and initialize the device driver abstration
 * interface.
 */
oi_device *foo_device() {
    oi_device *dev;
    foo_private *priv;

    debug("foo_device");

    // Alloc device and private data
    dev = (oi_device*)malloc(sizeof(oi_device));
    priv = (foo_private*)malloc(sizeof(foo_private));
    if((dev == NULL) || (priv == NULL)) {
        debug("foo_device: device creation failed");
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
    memset(priv, 0, sizeof(foo_private));

    // Set members
    dev->init = foo_init;
    dev->destroy = foo_destroy;
    dev->process = foo_process;
    dev->grab = foo_grab;
    dev->hide = foo_hidecursor;
    dev->warp = foo_warp;
    dev->winsize = foo_winsize;
    dev->reset = foo_reset;
    dev->private = priv;

    // Done
    return dev;
}

/* ******************************************************************** */

/**
 * @ingroup DFoo
 * @brief Initialize the foo test driver
 *
 * @param dev pointer to device interface
 * @param window_id window hook paramaters, see @ref PWindow
 * @param flags initialization flags, see @ref PFlags
 * @returns errorcode, see @ref PErrors
 *
 * This is a device interface function.
 *
 * Initialize the foo test driver - initializes
 * the private structure and other states.
 */
int foo_init(oi_device *dev, char *window_id, unsigned int flags) {
    unsigned int val;
    foo_private *priv;

    debug("foo_init: window '%s', flags %i", window_id, flags);

    // Sniff the handles
    val = device_windowid(window_id, OI_I_CONN);
    debug("foo_init: conn (c) parameter %i", val);

    val = device_windowid(window_id, OI_I_SCRN);
    debug("foo_init: scrn (s) parameter %i", val);

    val = device_windowid(window_id, OI_I_WINID);
    debug("foo_init: winid (w) parameter %i", val);

    // Set some stupid private values
    priv = (foo_private*)dev->private;
    priv->grabstatus = FALSE;
    priv->cursorstatus = FALSE;
    priv->x = 0;
    priv->y = 0;

    return OI_ERR_OK;
}

/* ******************************************************************** */

/**
 * @ingroup DFoo
 * @brief Shutdown the foo device
 *
 * @param dev pointer to device interface
 * @returns errorcode, see @ref PErrors
 *
 * This is a device interface function.
 *
 * Close down driver freeing any allocated memory.
 */
int foo_destroy(oi_device *dev) {
    debug("foo_destroy");

    // Free device
    if(dev) {
        // Private data
        if(dev->private) {
            free(dev->private);
        }
        free(dev);
        dev = NULL;
    }

    return OI_ERR_OK;
}

/* ******************************************************************** */

/**
 * @ingroup DFoo
 * @brief Process events
 *
 * @param dev pointer to device interface
 *
 * This is a device interface function.
 *
 * Pump events into the OpenInput library. As this
 * is a test device, only a few hardcoded events
 * are inserted - all state managers should have an
 * event injected, and a direct queue insertion should
 * also be performed.
 */
void foo_process(oi_device *dev) {
    static oi_event ev;

    debug("foo_process");

    if(!oi_runstate()) {
        debug("foo_process: oi_running false");
        return;
    }

    // Since this is a test device, generate an event
    ev.type = OI_KEYDOWN;
    ev.key.device = dev->index;
    ev.key.state = 1;
    ev.key.keysym.scancode = 65;
    ev.key.keysym.sym = OIK_A;
    ev.key.keysym.mod = OIM_NONE;

    // Post event
    queue_add(&ev);
}

/* ******************************************************************** */

/**
 * @ingroup DFoo
 * @brief Grab/release pointer
 *
 * @param dev pointer to device interface
 * @param on true (1) turns on grab, false (0) releases grab
 * @returns errorcode, see @ref PErrors
 *
 * This is a device interface function.
 *
 * Since the foo device is a virtual test device, this function
 * does not perform any real grabbing. A state is flipped, and
 * in debug-mode, the state change can be verified.
 */
int foo_grab(oi_device *dev, int on) {
    foo_private *priv;
    priv = (foo_private*)dev->private;
    debug("foo_grab: current:%i new:%i", priv->grabstatus, on);
    priv->grabstatus = on;

    return OI_ERR_OK;
}

/* ******************************************************************** */


/**
 * @ingroup DFoo
 * @brief Show/hide pointer
 *
 * @param dev pointer to device interface
 * @param on true (1) hides cursor, false (0) shows cursor
 * @returns errorcode, see @ref PErrors
 *
 * This is a device interface function.
 *
 * Since the foo device is a virtual test device, this function
 * does not perform any real hide/show of the pointer. A state is flipped,
 * and in debug-mode, the state change can be verified.
 */
int foo_hidecursor(oi_device *dev, int on) {
    foo_private *priv;
    priv = (foo_private*)dev->private;
    debug("foo_hidecursor: current:%i new:%i", priv->cursorstatus, on);
    priv->cursorstatus = on;

    return OI_ERR_OK;
}

/* ******************************************************************** */

/**
 * @ingroup DFoo
 * @brief Warp pointer
 *
 * @param dev pointer to device interface
 * @param x pointer to horizontal position
 * @param y pointer to vertical position
 * @returns errorcode, see @ref PErrors
 *
 * This is a device interface function.
 *
 * Since the foo device is a virtual test device, this function
 * does not perform any cursor movement. The internal state
 * is changed, and can be verified in debug-mode.
 */
int foo_warp(oi_device *dev, int x, int y) {
    foo_private *priv;
    priv = (foo_private*)dev->private;
    debug("foo_warp: warp pointer to %i, %i", x, y);
    priv->x = x;
    priv->y = y;

    return OI_ERR_OK;
}

/* ******************************************************************** */

/**
 * @ingroup DFoo
 * @brief Get window size
 *
 * @param dev pointer to device interface
 * @param w pointer to horizontal size
 * @param h pointer to vertical size
 * @returns errorcode, see @ref PErrors
 *
 * This is a device interface function.
 *
 * Since the foo device is a virtual test device, this function
 * does not return the window size. Instead, it returns the
 * virtual mouse pointer coordinate (set using the warp function).
 */
int foo_winsize(oi_device *dev, int *w, int *h) {
    foo_private *priv;
    priv = (foo_private*)dev->private;
    debug("foo_winsize");

    // Just for fun, return the mouse position
    *w = priv->x;
    *h = priv->y;

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
 * Reset/sync the internal states. Since this is
 * a test device, simply just reset the coordinates etc.
 */
int foo_reset(oi_device *dev) {
    foo_private *priv;
    priv = (foo_private*)dev->private;
    debug("foo_reset");

    priv->grabstatus = FALSE;
    priv->cursorstatus = FALSE;
    priv->x = 0;
    priv->y = 0;

    return OI_ERR_OK;
}

/* ******************************************************************** */
