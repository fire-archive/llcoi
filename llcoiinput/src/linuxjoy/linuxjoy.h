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

#ifndef _OPENINPUT_LINUXJOY_H_
#define _OPENINPUT_LINUXJOY_H_

/* ******************************************************************** */

// Bootstrap entries
int linuxjoy_avail();
oi_device *linuxjoy_device();

/* ******************************************************************** */

// Device entries
int linuxjoy_init(oi_device *dev, char *window_id, unsigned int flags);
int linuxjoy_enable(oi_device *dev, int on);
int linuxjoy_destroy(oi_device *dev);
void linuxjoy_process(oi_device *dev);
int linuxjoy_reset(oi_device *dev);

/* ******************************************************************** */

// Misc local functions
int linuxjoy_getfd(unsigned char num);
void linuxjoy_fallback(oi_device *dev, char *name, int fd);

/* ******************************************************************** */

/**
 * @ingroup DLinuxjoy
 * @brief Linux joystick driver private instance data
 *
 * Private data for a single joystick device, such as the
 * file handle and information about the hat/ball/button/axis
 * mappings etc.
 *
 * Note that this driver can handle several joysticks!
 */
typedef struct linuxjoy_private {
    int fd;                      /**< File descriptor */
    unsigned char id;                    /**< Device index, ie. the X in /dev/input/jsX */
    char *name;                  /**< Kernel device name */
} linuxjoy_private;

/* ******************************************************************** */

/**
 * @ingroup DLinuxjoy
 * @{
 */
#define DLJS_MAX_DEVS 32         /**< Max joystick devices */
#define DLJS_NAME_SIZE 128       /**< Length of a joystick description (kernel) */
/** @} */

/* ******************************************************************** */

#endif
