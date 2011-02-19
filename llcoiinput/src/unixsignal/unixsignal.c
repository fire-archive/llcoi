/*
 * unixsignal.c : UNIX signal handler (intr, sigsev, etc.)
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
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include "internal.h"
#include "bootstrap.h"
#include "unixsignal.h"

/**
 * @ingroup Drivers
 * @defgroup DUnix UNIX signal driver
 * @brief UNIX/POSIX signal device driver
 *
 * POSIX systems use "signals" to notify applications
 * of operating systems events such as suspend,
 * interrupt and hang-up, segmentation faults etc.
 * This driver listens for some of the most basic
 * signals and generates a OpenInput quit-event
 * if one of these is received.
 */

// Bootstrap global
oi_bootstrap unixsignal_bootstrap = {
    "unixsignal",
    "UNIX signal handler",
    OI_PRO_UNKNOWN,
    unixsignal_avail,
    unixsignal_device
};

// Private data is global here (it makes no sense with a private struct)
static char pendingsignal;

/* ******************************************************************** */

/**
 * @ingroup DUnix
 * @brief Check if POSIX signals exists
 *
 * @param flags library initialization flags, see @ref PFlags
 * @returns errorcode, see @ref PErrors
 *
 * This is a bootstrap function.
 *
 * Always returns true as this is platform specific.
 */
int unixsignal_avail(unsigned int flags) {
    debug("unixsignal_avail");

    return TRUE;
}

/* ******************************************************************** */

/**
 * @ingroup DUnix
 * @brief Create UNIX signal device driver interface
 *
 * @returns pointer to device interface, see @ref IDevstructs
 *
 * This is a bootstrap function.
 *
 * Create the device interface.
 */
oi_device *unixsignal_device() {
    oi_device *dev;

    debug("unixsignal_device");

    // Alloc device data
    dev = (oi_device*)malloc(sizeof(oi_device));
    if(dev == NULL) {
        debug("unixsignal_device: device creation failed");
        return NULL;
    }

    // Clear structures
    memset(dev, 0, sizeof(oi_device));

    // Set members
    dev->init = unixsignal_init;
    dev->destroy = unixsignal_destroy;
    dev->process = unixsignal_process;
    dev->grab = NULL;
    dev->hide = NULL;
    dev->warp = NULL;
    dev->winsize = NULL;
    dev->reset = unixsignal_reset;
    dev->private = NULL;

    // Done
    return dev;
}

/* ******************************************************************** */

/**
 * @ingroup DUnix
 * @brief Initialize the UNIX signal driver
 *
 * @param dev pointer to created device interface
 * @param window_id window hook parameters, see @ref PWindow
 * @param flags initialization flags, see @ref PFlags
 * @returns errorcode, see @ref PErrors
 *
 * This is a device interface function.
 *
 * Setup the signal handlers for interrupt, terminate and segfault.
 */
int unixsignal_init(oi_device *dev, char *window_id, unsigned int flags) {
    debug("unixsignal_init");

    // Just to be sure, no signal is pending
    pendingsignal = FALSE;

    // Install handler for various shutdown-signals
    signal(SIGINT, unixsignal_handler);  // Interrupt (ctrl+c)
    signal(SIGTERM, unixsignal_handler); // Terminate (kill)
    signal(SIGSEGV, unixsignal_handler); // Segfault

    return OI_ERR_OK;
}

/* ******************************************************************** */

/**
 * @ingroup DUnix
 * @brief Destroy the UNIX signal device driver
 *
 * @param dev pointer to device interface
 * @returns errorcode, see @ref PErrors
 *
 * This is a device interface function.
 *
 * Free device structure and reset signal handlers to
 * the system default.
 */
int unixsignal_destroy(oi_device *dev) {
    debug("unixsignal_destroy");

    // Set default handlers
    signal(SIGINT, SIG_DFL);
    signal(SIGTERM, SIG_DFL);
    signal(SIGSEGV, SIG_DFL);

    // Free device
    if(dev) {
        free(dev);
        dev = NULL;
    }

    return OI_ERR_OK;
}

/* ******************************************************************** */

/**
 * @ingroup DUnix
 * @brief Process events
 *
 * @param dev pointer to device interface
 *
 * This is a device interface function.
 *
 * Pump an OI_QUIT event into the queue if a
 * signal is pending.
 */
void unixsignal_process(oi_device *dev) {
    static oi_event ev;

    if(!oi_runstate()) {
        debug("unixsignal_process: oi_running false");
        return;
    }

    // Bail out if no signal
    if(!pendingsignal) {
        return;
    }

    // Don't forget to clear the flag
    pendingsignal = FALSE;

    // A signal, send quit event
    ev.type = OI_QUIT;
    queue_add(&ev);
}

/* ******************************************************************** */

/**
 * @ingroup DUnix
 * @brief Reset internal state
 *
 * @param dev pointer to device interface
 * @returns errorcode, see @ref PErrors
 *
 * This is a device interface function.
 *
 * Reset pending flag.
 */
int unixsignal_reset(oi_device *dev) {
    debug("unixsignal_reset");

    pendingsignal = FALSE;

    return OI_ERR_OK;
}

/* ******************************************************************** */

/**
 * @ingroup DUnix
 * @brief The POSIX signal handler
 *
 * @param signum signal code
 *
 * This is the POSIX signal handler function. We simply
 * set the pending-flag, so that the next call to the
 * event pump will inject the quit event.
 */
void unixsignal_handler(int signum) {

    if(!pendingsignal) {
        debug("unixsignal_handler: signal %d received", signum);
    }

    // Ok, we've fetched a signal
    pendingsignal = TRUE;
}

/* ******************************************************************** */
