/*
 * linuxjoy_devs.h: Special joystick setups
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

#ifndef _OPENINPUT_LINUXJOY_DEVS_H_
#define _OPENINPUT_LINUXJOY_DEVS_H_

/* ******************************************************************** */

/**
 * @ingroup DLinuxjoy
 * @brief Special joysticks
 *
 * Structure to contain information about non-standard joysticks.
 *
 * - Name
 * - Number of buttons
 * - Axes mapping, see @ref PJoyTypes
 * - Axes pairing
 *
 * Note that the joysticks below have are probably incorrect!
 */
const oi_joyconfig linuxjoy_specs[] = {
    { "MadCatz Panther XL",
      8,
      {2, 2, 2, 4, 0, 4, 0, 7},
      {0, 0, 0, 4, 0, 6} },
    { "SideWinder Precision Pro",
      8,
      {2, 2, 2, 2, 4, 0, 7},
      {0, 0, 0, 0, 5} }
};


/* ******************************************************************** */

#endif
