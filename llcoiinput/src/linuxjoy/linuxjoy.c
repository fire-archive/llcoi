/*
 * linuxjoy.h: GNU/Linux joystick driver
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
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <linux/joystick.h>
#include "internal.h"
#include "bootstrap.h"
#include "linuxjoy.h"
#include "linuxjoy_devs.h"

#ifdef DEBUG
#include <errno.h>
#endif

/**
 * @ingroup Drivers
 * @defgroup DLinuxjoy Linux joystick driver
 * @brief GNU/Linux joystick driver
 *
 * Joystick driver for GNU/Linux systems using the v1.0+ joystick API
 * supported by kernels 2.4+. This driver talks directly to the
 * joystick /dev/input/jsX character device(s) and supports up to 32
 * joysticks (X=0..31).
 */

// Bootstrap global
oi_bootstrap linuxjoy_bootstrap = {
    "linuxjoy",
    "GNU/Linux joystick driver",
    OI_PRO_JOYSTICK,
    linuxjoy_avail,
    linuxjoy_device
};

// Joystick to be initialized next
static unsigned char init_next = 0;

/* ******************************************************************** */

/**
 * @ingroup DLinuxjoy
 * @brief Check if POSIX signals exists
 *
 * @param flags library initialization flags, see @ref PFlags
 * @returns errorcode, see @ref PErrors
 *
 * This is a bootstrap function.
 *
 * The easiest test is to check whether the character devices
 * exist. If a single device is found, we assume it's available.
 */
int linuxjoy_avail(unsigned int flags) {
    int i;
    int fd;

    debug("linuxjoy_avail");

    // Simply try to open a joystick
    fd = -1;
    for(i=init_next; i<DLJS_MAX_DEVS; i++) {
        fd = linuxjoy_getfd(i);

        debug("linuxjoy_avail: testing /dev/input/js%u, fd:%i", i, fd);

        // Ok, got on, close it and bail
        if(fd != -1) {
            close(fd);
            break;
        }
    }

    // If filedescriptor isn't -1, a joystick exists
    return (fd != -1);
}

/* ******************************************************************** */

/**
 * @ingroup DLinuxjoy
 * @brief Create joystick device driver interface
 *
 * @returns pointer to device interface, see @ref IDevstructs
 *
 * This is a bootstrap function.
 *
 * Create the device interface. Even though this driver supports
 * multiple devices, the create-function is just a factory.
 * Device parameters are overridden in the device->init() function.
 */
oi_device *linuxjoy_device() {
    oi_device *dev;
    linuxjoy_private *priv;
    oi_joyconfig *conf;

    debug("linuxjoy_device");

    // Alloc device data
    dev = (oi_device*)malloc(sizeof(oi_device));
    priv = (linuxjoy_private*)malloc(sizeof(linuxjoy_private));
    conf = (oi_joyconfig*)malloc(sizeof(oi_joyconfig));
    if(!dev || !priv || !conf) {
        debug("linuxjoy_device: device creation failed");
        if(dev) {
            free(dev);
        }
        if(priv) {
            free(priv);
        }
        if(conf) {
            free(conf);
        }
        return NULL;
    }

    // Clear structures
    memset(dev, 0, sizeof(oi_device));
    memset(priv, 0, sizeof(linuxjoy_private));
    memset(conf, 0, sizeof(oi_joyconfig));

    // Set members
    dev->private = priv;
    dev->joyconfig = conf;
    dev->init = linuxjoy_init;
    dev->destroy = linuxjoy_destroy;
    dev->process = linuxjoy_process;
    dev->grab = NULL;
    dev->hide = NULL;
    dev->warp = NULL;
    dev->winsize = NULL;
    dev->reset = linuxjoy_reset;

    // Done
    return dev;
}

/* ******************************************************************** */

/**
 * @ingroup DLinuxjoy
 * @brief Initialize the joystick driver
 *
 * @param dev pointer to created device interface
 * @param window_id window hook parameters, see @ref PWindow
 * @param flags initialization flags, see @ref PFlags
 * @returns errorcode, see @ref PErrors
 *
 * This is a device interface function.
 *
 * Try to open next joystick.
 */
int linuxjoy_init(oi_device *dev, char *window_id, unsigned int flags) {
    int i;
    int fd;
    char ok;
    char *name;
    linuxjoy_private *priv;

    debug("linuxjoy_init");

    // We can handle more than one device!
    device_moreavail(TRUE);

    // Find next
    fd = -1;
    for(i=init_next; i<DLJS_MAX_DEVS; i++) {
        fd = linuxjoy_getfd(i);
        if(fd != -1) {
            break;
        }
    }

    // No matter what, don't try this device again
    init_next++;

    // Bail now if no fd was found
    if(fd == -1) {
        return OI_ERR_NO_DEVICE;
    }
    debug("linuxjoy_init: file opened");

    // Ok, time to fetch the greasy device details
    priv = (linuxjoy_private*)dev->private;
    priv->fd = fd;
    priv->id = i;

    // Get description using IOCTL (see linux/Documentation/input/joystick-api.h)
    name = (char*)malloc(DLJS_NAME_SIZE);
    memset(name, 0, DLJS_NAME_SIZE);
    if(ioctl(fd, JSIOCGNAME(DLJS_NAME_SIZE), name) < 0) {
        sprintf(name, "Unknown joystick #%u", i);
    }
    debug("linuxjoy_init: kernel description '%s'", name);

    // Find joystick
    ok = FALSE;
    for(i=0; i<TABLESIZE(linuxjoy_specs); i++) {
        if(strcmp(name, linuxjoy_specs[i].name) == 0) {
            // Transfer settings
            dev->joyconfig->name = linuxjoy_specs[i].name;
            dev->joyconfig->buttons = linuxjoy_specs[i].buttons;
            for(i=0; i<OI_JOY_NUM_AXES; i++) {
                dev->joyconfig->kind[i] = linuxjoy_specs[i].kind[i];
                dev->joyconfig->pair[i] = linuxjoy_specs[i].pair[i];
            }
            ok = TRUE;
            break;
        }
    }

    // If joystick was unknown, try the default/fallback handler
    if(!ok) {
        linuxjoy_fallback(dev, name, fd);
    }

    // Done at last!
    return OI_ERR_OK;
}

/* ******************************************************************** */

/**
 * @ingroup DLinuxjoy
 * @brief Default fallback kernel driver
 *
 * @param dev pointer to device structure
 * @param name kernel joystick name
 * @param fd file descriptior for device
 *
 * If the joystick is not on the list of special joysticks,
 * the Linux kernel uses a standard analog driver. Try that
 * one and fetch info about number of buttons, axes and hats.
 * If that fails, be very, very, very conservative
 */
void linuxjoy_fallback(oi_device *dev, char *name, int fd) {
    char ok;
    int noaxes;
    int nohats;
    int nobtns;
    int i;
    int j;

    // Conservative defaults
    ok = FALSE;
    noaxes = 2;
    nobtns = 2;
    nohats = 0;

    /* See if joystick is run by the "generic analog" kernel driver.
     * If so, we can get the number of axes/buttons/hats by decoding
     * the name. Kernel name is "Analog X-axis Y-button Z-hat".
     */
    if((strstr(name, "Analog")==name) && strstr(name, "-hat")) {
        // Try to decode number of axes and hats
        if(sscanf(name, "Analog %d-axis %d-button %d-hat", &noaxes, &nobtns, &nohats) == 3) {
            debug("linuxjoy_fallback: analog kernel driver axes:%u btns:%u hats:%u",
                  noaxes, nobtns, nohats);
            ok = TRUE;
        }
    }

    // Non-analog driver, probe kernel for details
    if(!ok) {
        // Get number of axes using IOCTL
        if(ioctl(fd, JSIOCGAXES, &noaxes) < 0) {
            noaxes = 2;
        }
        debug("linuxjoy_init: axes '%u'", noaxes);

        // Get number of buttons using IOCTL
        if(ioctl(fd, JSIOCGBUTTONS, &nobtns) < 0) {
            nobtns = 2;
        }
        debug("linuxjoy_init: buttons '%u'", nobtns);
    }

    // Buttons are easy
    dev->joyconfig->buttons = nobtns;

    // Fill axes mapping - under Linux, sticks are always first
    j = 0;
    for(i=0; (i<noaxes) && (j<OI_JOY_NUM_AXES); i++) {
        // Standard stick axis, no pair
        dev->joyconfig->kind[j] = OIJ_STICK;
        dev->joyconfig->pair[j] = 0;
        j++;
    }

    // Axes after sticks are hats, which are two-axis (ie. paired)
    for(i=0; (i<nohats) && (j<OI_JOY_NUM_AXES); i++) {
        dev->joyconfig->kind[j] = OIJ_HAT;
        dev->joyconfig->pair[j] = j+1;
        dev->joyconfig->kind[j+1] = OIJ_NONE;
        dev->joyconfig->pair[j+1] = 0;
        j += 2;
    }
}

/* ******************************************************************** */
/**
 * @ingroup DLinuxjoy
 * @brief Destroy the joystick device driver
 *
 * @param dev pointer to device interface
 * @returns errorcode, see @ref PErrors
 *
 * This is a device interface function.
 *
 * Free device structure and reset signal handlers to
 * the system default.
 */
int linuxjoy_destroy(oi_device *dev) {
    linuxjoy_private *priv;

    debug("linuxjoy_destroy");

    // Free device
    if(dev) {

        // Firstly, free the private data
        priv = (linuxjoy_private*)dev->private;
        if(priv) {
            // Close file
            if(priv->fd != -1) {
                close(priv->fd);
            }

            // Free custom name and description
            if(priv->name) {
                free(priv->name);
            }
        }

        // Secondly, free the joystick configuration
        if(dev->joyconfig) {
            free(dev->joyconfig);
        }

        free(dev);
        dev = NULL;
    }

    return OI_ERR_OK;
}

/* ******************************************************************** */

/**
 * @ingroup DLinuxjoy
 * @brief Process events
 *
 * @param dev pointer to device interface
 *
 * This is a device interface function.
 *
 * Read events from the joystick, and inject these
 * directly into the OI joystick interface. Translation
 * of buttons, axes etc. is done by OI.
 */
void linuxjoy_process(oi_device *dev) {
    static struct js_event jse;
    int i;
    linuxjoy_private *priv;

    if(!oi_runstate()) {
        debug("linuxjoy_process: oi_running false");
        return;
    }

    // Prepare
    priv = (linuxjoy_private*)dev->private;

    // We're in non-blocking mode, so empty the event queue
    while((i = read(priv->fd, &jse, sizeof(struct js_event))) > 0) {
        // Button
        if(jse.type & JS_EVENT_BUTTON) {
            // Inject event into joystick state manager
            joystick_button(dev->index, jse.number, jse.value, TRUE);
        }

        // Axis
        else if(jse.type & JS_EVENT_AXIS) {
            // Inject event into joystick state manager
            joystick_axis(dev->index, jse.number, jse.value, OI_QUERY, TRUE);
        }
    }

    // Do some debugging
    debug("joystick_process: errorcode '%i'", errno);
}

/* ******************************************************************** */

/**
 * @ingroup DLinuxjoy
 * @brief Get file descriptor for numbered joystick
 *
 * @param num joystick index (0-31)
 * @returns file descriptor or -1 if joystick not found
 *
 * Open /dev/input/jsX with X=num and return the file descriptor.
 */
int linuxjoy_getfd(unsigned char num) {
    char path[128];

    // Dummy
    if(num >= DLJS_MAX_DEVS) {
        return -1;
    }

    // Make path and open it
    sprintf(path, "/dev/input/js%u", num);
    return open(path, O_RDONLY | O_NONBLOCK, 0);
}

/* ******************************************************************** */

/**
 * @ingroup DLinuxjoy
 * @brief Reset internal state
 *
 * @param dev pointer to device interface
 * @returns errorcode, see @ref PErrors
 *
 * This is a device interface function.
 *
 * Resync the driver-device states.
 */
int linuxjoy_reset(oi_device *dev) {
    linuxjoy_private *priv;
    priv = (linuxjoy_private*)dev->private;
    debug("linuxjoy_reset");

    // Fixme

    return OI_ERR_OK;
}

/* ******************************************************************** */
