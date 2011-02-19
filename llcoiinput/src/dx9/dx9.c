/*
 * dx9.c : DirectX 9 utility functions (bootstrapping, etc.)
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
#include <windows.h>
#include <string.h>
#include <stdlib.h>
#include <dinput.h>
#include "internal.h"
#include "bootstrap.h"
#include "dx9.h"

/**
 * @ingroup Drivers
 * @defgroup DDX9 DirectX device driver
 * @brief DirectX input system driver
 *
 * The Direct Input 9 wraps the Direct X input API
 * into the cross-platform OpenInput API. This driver
 * was written for DirectX version 9, but may or may
 * not function under other versions. The driver supports
 * both keyboards, mice and joysticks, aswell as the usual
 * pointer and hook functionality found in OpenInput.
 */

// Bootstrap global
oi_bootstrap dx9_bootstrap = {
    "dx9",
    "DirectX input system",
    OI_PRO_KEYBOARD | OI_PRO_MOUSE | OI_PRO_JOYSTICK | OI_PRO_WINDOW,
    dx9_avail,
    dx9_device
};

// Device identifier linked list
static dx9_uid *guid_list;

// Old window procedure
static WNDPROC old_wndproc = NULL;

// Reference counting for number of initialized drivers
static char instances = 0;

// The root object
static IDirectInput8A *object = NULL;

/* ******************************************************************** */

/**
 * @ingroup DDX9
 * @brief Check for existance of Direct Input
 *
 * @param flags library initialization flags, see @ref PFlags
 * @returns errorcode, see @ref PErrors
 *
 * This is a bootstrap function.
 *
 * Try to open the DirectInput object, which tells us if
 * the system supports DI.
 */
int dx9_avail(unsigned int flags) {
    HRESULT res;
    LPDIRECTINPUT8 obj;

    debug("dx9_avail");

    // Get object and free it again
    res = DirectInput8Create(GetModuleHandle(NULL),
                             DIRECTINPUT_VERSION,
                             (REFIID)&IID_IDirectInput8,
                             (void**)&obj,
                             NULL);
    if(obj) {
        obj->lpVtbl->Release(obj);
    }
    if(res != DI_OK) {
        debug("dx9_avail: direct input not found, result code 0x%08X", res);
    }

    return (res == DI_OK);
}

/* ******************************************************************** */

/**
 * @ingroup DDX9
 * @brief Create DX9 device driver interface
 *
 * @returns pointer to device interface, see @ref IDevstructs
 *
 * This is a bootstrap function.
 *
 * If it is the first time we run this function, perform a DI
 * device enumeration, ie. get all GUIDs. The GUIDs are stored
 * in the global guid_list linked list.
 *
 * Afterwards, each time this function is called, the first
 * device identifier is taken from the list. The standard OI
 * device is then created and everything proceeeds more or
 * less as any other device driver.
 */
oi_device *dx9_device() {
    oi_device *dev;
    dx9_private *priv;
    oi_joyconfig *joyconf;
    HRESULT res;

    debug("dx9_device");

    // If no DI root object, create it and run device enumeration
    if(object == NULL) {
        debug("dx9_device: device enumeration");

        // Create the root object
        res = DirectInput8Create(GetModuleHandle(NULL),
                                 DIRECTINPUT_VERSION,
                                 &IID_IDirectInput8,
                                 (void**)&object,
                                 NULL);

        if((res != DI_OK) || (object == NULL)) {
            debug("dx9_device: object creation failed, code 0x%08X", res);
            return NULL;
        }
        res = object->lpVtbl->EnumDevices(object,
                                          DI8DEVCLASS_ALL,
                                          dx9_devenum,
                                          NULL,
                                          DIEDFL_ALLDEVICES);

        if(res != DI_OK) {
            debug("dx9_device: device enumeration failed, code 0x%08X", res);
        }
        else {
            debug("dx9_device: enumeration succeeded");
        }        
    }

    // Don't continue if there's no device
    if(guid_list == NULL) {
        debug("dx9_device: device list is empty, aborting");
        return NULL;
    }

    /* We must know if the device we're creating next is a game controller
     * so the joyconfig structure can be created. The next device is always
     * the head in guid_list, which also holds provide-flags set by the
     * DI device enumeration. Pretty neat! */
    if(guid_list->provides & OI_PRO_JOYSTICK) {
        joyconf = (oi_joyconfig*)malloc(sizeof(oi_joyconfig));
        if(!joyconf) {
            return NULL;
        }
        memset(joyconf, 0, sizeof(oi_joyconfig));
    }
    else {
        joyconf = NULL;
    }

    // Alloc device and private
    dev = (oi_device*)malloc(sizeof(oi_device));
    priv = (dx9_private*)malloc(sizeof(dx9_private));
    if((dev == NULL) || (priv == NULL)) {
        debug("dx9_device: device creation failed");
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
    memset(priv, 0, sizeof(dx9_private));

    // Set members
    dev->private = priv;
    dev->joyconfig = joyconf;

    // Override default device settings
    dev->provides = guid_list->provides;
    dev->name = (char *)guid_list->name;

    // Set interface functions
    dev->init = dx9_init;
    dev->destroy = dx9_destroy;
    dev->process = dx9_process;
    dev->grab = dx9_grab;
    dev->hide = NULL;
    dev->warp = dx9_warp;
    dev->winsize = dx9_winsize;
    dev->reset = dx9_reset;

    // Done
    return dev;
}

/* ******************************************************************** */

/**
 * @ingroup DDX9
 * @brief Initialize the DX9 driver
 *
 * @param dev pointer to created device interface
 * @param window_id window hook parameters, see @ref PWindow
 * @param flags initialization flags, see @ref PFlags
 * @returns errorcode, see @ref PErrors
 *
 * This is a device interface function.
 *
 * This is the function where the DI device is created: We pop
 * the head element off the guid_list and use the GUID to create
 * the object. If the object is a game controller we furthermore
 * initiate the object enumeration to get axes information etc.
 *
 * When the first device is initialized, we furthermore install the
 * window proc hook. Note that this driver uses the 'moreavail'
 * protocol.
 */
int dx9_init(oi_device *dev, char *window_id, unsigned int flags) {
    dx9_private *priv;
    dx9_uid *pop;
    GUID guid;
    HRESULT res;
    DIPROPDWORD prop;

    priv = (dx9_private*)dev->private;

    debug("dx9_init");

    device_moreavail(TRUE);

    // Pop and free the guid_list head
    if(guid_list == NULL) {
        return OI_ERR_NO_DEVICE;
    }
    guid = guid_list->guid;
    pop = guid_list;
    guid_list = guid_list->next;
    free(pop);

    // Parse the window_id flags
    priv->hwnd = (HWND)device_windowid(window_id, OI_I_WINID);

    // We require winid parameter
    if(!priv->hwnd) {
        debug("dx9_init: winid (w) parameter required\n");
        return OI_ERR_NO_DEVICE;
    }

    // If we don't yet have the old window proc handle, fetch it
    if(old_wndproc == NULL) {
#ifdef GWLP_WNDPROC
        old_wndproc = (WNDPROC)GetWindowLongPtr(priv->hwnd, GWLP_WNDPROC);
#else
        old_wndproc = (WNDPROC)GetWindowLong(priv->hwnd, GWL_WNDPROC);
#endif
        if(!old_wndproc) {
            debug("dx9_init: invalid window handle");
            return OI_ERR_NO_DEVICE;
        }
    }

    // Make sure we only get devices that are meaningfull
    if(dev->provides == OI_PRO_UNKNOWN) {
        return OI_ERR_NO_DEVICE;
    }

    // Create the DI device object
    res = object->lpVtbl->CreateDevice(object,
                                       &guid,
                                       (LPDIRECTINPUTDEVICE8A*)&(priv->obj),
                                       NULL);

    if((res != DI_OK) || (priv->obj == NULL)) {
        return OI_ERR_NO_DEVICE;
    }
    instances++;

    // Install our own window handle with the first device
    if(instances == 1) {
        debug("dx9_init: first device installing");

#ifdef GWLP_WNDPROC
        SetWindowLongPtr(priv->hwnd, GWLP_WNDPROC, (LONG)dx9_wndproc);
#else
        SetWindowLong(priv->hwnd, GWL_WNDPROC, (LONG)dx9_wndproc);
#endif
    }

    // Set data format depending on device type
    if(dev->provides & OI_PRO_KEYBOARD) {
        priv->obj->lpVtbl->SetDataFormat(priv->obj, &c_dfDIKeyboard);
    }
    else if(dev->provides & OI_PRO_MOUSE) {
        priv->obj->lpVtbl->SetDataFormat(priv->obj, &c_dfDIMouse2);
    }
    else if(dev->provides & OI_PRO_JOYSTICK) {
        priv->obj->lpVtbl->SetDataFormat(priv->obj, &c_dfDIJoystick2);
    }

    // Set buffered input mode
    prop.diph.dwSize = sizeof(DIPROPDWORD);
    prop.diph.dwHeaderSize = sizeof(DIPROPHEADER);
    prop.diph.dwObj = 0;
    prop.diph.dwHow = DIPH_DEVICE;
    prop.dwData = DDX9_BUFFER_SIZE;
    res = priv->obj->lpVtbl->SetProperty(priv->obj,
                                         DIPROP_BUFFERSIZE,
                                         &(prop.diph));
    if(res != DI_OK) {
        priv->obj->lpVtbl->Release(priv->obj);
        instances--;
        return OI_ERR_DEV_BEHAVE;
    }

    // If it's a joystick, enumerate buttons, joysticks etc.
    if(dev->provides & OI_PRO_JOYSTICK) {
        DIDEVCAPS devcaps;

        // Get number of buttons
        devcaps.dwSize = sizeof(DIDEVCAPS);
        priv->obj->lpVtbl->GetCapabilities(priv->obj,
                                           &devcaps);
        dev->joyconfig->buttons = (unsigned char)devcaps.dwButtons;

        // Enumerate axes
        priv->obj->lpVtbl->EnumObjects(priv->obj,
                                       dx9_joyenum,
                                       dev,
                                       DIDFT_AXIS);
    }

    // Behave nicely and acquire the device
    priv->obj->lpVtbl->SetCooperativeLevel(priv->obj,
                                           priv->hwnd,
                                           DISCL_FOREGROUND | DISCL_NONEXCLUSIVE);
    priv->obj->lpVtbl->Acquire(priv->obj);

    debug("dx9_init: initialized");

    return OI_ERR_OK;
}

/* ******************************************************************** */

/**
 * @ingroup DDX9
 * @brief Destroy the DX9 device driver
 *
 * @param dev pointer to device interface
 * @returns errorcode, see @ref PErrors
 *
 * This is a device interface function.
 *
 * Shutdown the DX9 device: Unacquire the
 * input object and relase it. If the device was
 * the last device driver, remove our window
 * procedure and install the old.
 */
int dx9_destroy(oi_device *dev) {
    dx9_private *priv;

    debug("dx9_destroy");

    if(dev) {
        priv = (dx9_private*)dev->private;

        // Private members
        if(priv) {
            priv->obj->lpVtbl->Unacquire(priv->obj);
            priv->obj->lpVtbl->Release(priv->obj);
            free(priv);
        }

        // Joystick configuration
        if(dev->joyconfig)
        {
            free(dev->joyconfig);
        }

        // If it's the last DI device
        instances--;
        if(instances == 0) {
            debug("dx9_destroy: last device exiting");

            // Reinstall old window proc
#ifdef GWLP_WNDPROC
            SetWindowLongPtr(priv->hwnd, GWLP_WNDPROC, (LONG)old_wndproc);
#else
            SetWindowLong(priv->hwnd, GWL_WNDPROC, (LONG)old_wndproc);
#endif

            // Free the DI root object and reset driver
            object->lpVtbl->Release(object);
            object = NULL;
            guid_list = NULL;
        }

        // Device struct
        free(dev);
        dev = NULL;
    }

    return OI_ERR_OK;
}

/* ******************************************************************** */

/**
 * @ingroup DDX9
 * @brief Process events
 *
 * @param dev pointer to device interface
 *
 * This is a device interface function.
 *
 * Process pending DX9 events, and pump these
 * into the OpenInput queue. The real functionality
 * is handled in the dispatchers, of which there is one for
 * keyboards, one for mice and one for joysticks.
 */
void dx9_process(oi_device *dev) {
    dx9_private *priv;
    HRESULT res;

    priv = (dx9_private*)dev->private;

    // Process all pending events
    if(oi_runstate()) {
        DIDEVICEOBJECTDATA data[DDX9_BUFFER_SIZE];
        unsigned int entries;

        // Poll the device
        res = priv->obj->lpVtbl->Poll(priv->obj);

        // Collect buffered input
        if(res == DI_OK) {
            entries = DDX9_BUFFER_SIZE;
            res = priv->obj->lpVtbl->GetDeviceData(priv->obj,
                                                   sizeof(DIDEVICEOBJECTDATA),
                                                   data, &entries, 0);
        }

        if(res != DI_OK) {
            // Device was lost, try to reacquire and exit
            res = priv->obj->lpVtbl->Acquire(priv->obj);
            while(res == DIERR_INPUTLOST) {
                res = priv->obj->lpVtbl->Acquire(priv->obj);
            }
            return;
        }

        // Bail out if no events were pending
        if(!entries) {
            return;
        }

        // Run the device specific dispatchers
        if(dev->provides & OI_PRO_KEYBOARD) {
            dx9_keyboard_dispatch(dev, data, entries);
        }
        else if(dev->provides & OI_PRO_MOUSE) {
            dx9_mouse_dispatch(dev, data, entries);
        }
        else if(dev->provides & OI_PRO_JOYSTICK) {
            dx9_joy_dispatch(dev, data, entries);
        }
        else {
            debug("dx9_process: unknown event");
        }
    }
}

/* ******************************************************************** */

/**
 * @ingroup DDX9
 * @brief Grab/release mouse and keyboard
 *
 * @param dev pointer to device interface.
 * @param on true (1) turns on grab, false (0) releases grab
 * @returns errorcode, see @ref PErrors
 *
 * This is a device interface function.
 *
 * Grab or release input (keyboard) and pointer (mouse) inside
 * the hook-window. As this is DI, the pointer is hidden when
 * it is grabbed and vice versa.
 */
int dx9_grab(oi_device *dev, int on) {
    dx9_private *priv;

    priv = (dx9_private*)dev->private;
    debug("dx9_grab: state:%i", on);

    if(on) {
        priv->exclusive = TRUE;
        priv->obj->lpVtbl->SetCooperativeLevel(priv->obj,
                                               priv->hwnd,
                                               DISCL_FOREGROUND | DISCL_EXCLUSIVE);
    }
    else {
        priv->exclusive = FALSE;
        priv->obj->lpVtbl->SetCooperativeLevel(priv->obj,
                                               priv->hwnd,
                                               DISCL_FOREGROUND | DISCL_NONEXCLUSIVE);
    }

    return OI_ERR_OK;
}

/* ******************************************************************** */

/**
 * @ingroup DDX9
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
int dx9_warp(oi_device *dev, int x, int y) {
    dx9_private *priv;

    priv = (dx9_private*)dev->private;

    // Simply go to warp speed
    priv->lastx = x;
    priv->lasty = y;

    return OI_ERR_OK;
}

/* ******************************************************************** */

/**
 * @ingroup DDX9
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
int dx9_winsize(oi_device *dev, int *w, int *h) {
    dx9_private *priv;

    priv = (dx9_private*)dev->private;

    // Update state
    priv->width = 42;  // FIXME
    priv->height = 42; // FIXME

    // Safely store data
    if(w) {
        *w = 0;//attr.width;
    }
    if(h) {
        *h = 0;//attr.height;
    }

    debug("dx9_winsize: width:%i height:%i", *w, *h);

    return OI_ERR_OK;
}

/* ******************************************************************** */

/**
 * @ingroup DDX9
 * @brief Reset internal state
 *
 * @param dev pointer to device interface
 * @returns errorcode, see @ref PErrors
 *
 * This is a device interface function.
 *
 * Force full resync the driver and device state.
 */
int dx9_reset(oi_device *dev) {
    dx9_private *priv;
    priv = (dx9_private*)dev->private;
    debug("dx9_reset");

    // Show cursor, grab off
    dx9_grab(dev, FALSE);

    // Sync keyboard state
    //dx9_keystate(dev, priv->disp, NULL);

    // Window size
    dx9_winsize(dev, NULL, NULL);

    // Center mouse cursor
    dx9_warp(dev, priv->height/2, priv->width/2);

    return OI_ERR_OK;
}

/* ******************************************************************** */

/**
 * @ingroup DDX9
 * @brief Direct Input device enumeration callback
 *
 * @param dev_inst input object being enumerated
 * @param pref enumeration private data
 * @return DIENUM_CONTINUE, as we always enumerate everything
 *
 * Direct Input have found an object. We simply store the
 * unique identifier in the linked list and continue.
 */
BOOL dx9_devnum(LPCDIDEVICEINSTANCEA dev_inst, LPVOID pref) {
    dx9_uid *uid;

    debug("dx9_devenum: device '%s' found", dev_inst->tszInstanceName);

    uid = (dx9_uid*)malloc(sizeof(dx9_uid));
    if(!uid) {
        return DIENUM_STOP;
    }

    // Get GUID and determine what the device provides
    uid->guid = dev_inst->guidInstance;
    uid->provides = OI_PRO_WINDOW;
    uid->name = dev_inst->tszInstanceName;

    switch(GET_DIDEVICE_TYPE(dev_inst->dwDevType)) {
        case DI8DEVTYPE_DRIVING:
        case DI8DEVTYPE_FLIGHT:
        case DI8DEVTYPE_GAMEPAD:
        case DI8DEVTYPE_JOYSTICK:
            uid->provides |= OI_PRO_JOYSTICK;
            break;

        case DI8DEVTYPE_KEYBOARD:
            uid->provides |= OI_PRO_KEYBOARD;
            break;

        case DI8DEVTYPE_MOUSE:
            uid->provides |= OI_PRO_MOUSE;
            break;

        default:
            uid->provides = OI_PRO_UNKNOWN;
            break;
    }

    // Install the new element as head
    uid->next = guid_list;
    guid_list = uid;

    return DIENUM_CONTINUE;
}


/* ******************************************************************** */

/**
 * @ingroup DDX9
 * @brief Direct Input joystick enumeration callback
 *
 * @param obj_inst input object being enumerated
 * @param pref enumeration private data
 * @return DIENUM_CONTINUE, as we always enumerate everything
 *
 * Direct Input for a joystick object. Determine how to use
 * that information for the OI translation.
 */
BOOL dx9_objenum(LPCDIDEVICEOBJECTINSTANCE obj_inst, LPVOID pref) {
    DIPROPRANGE range;
    oi_device *dev;
    dx9_private *priv;

    dev = (oi_device*)pref;
    priv = (dx9_private*)dev->private;

    debug("dx9_objenum: object '%s' found at offset %i",
          obj_inst->tszName, obj_inst->dwOfs);

    // Setup axis scale
    range.diph.dwSize       = sizeof(DIPROPRANGE);
    range.diph.dwHeaderSize = sizeof(DIPROPHEADER);
    range.diph.dwHow        = DIPH_BYID;
    range.diph.dwObj        = obj_inst->dwType;
    range.lMin              = OI_JOY_AXIS_MIN;
    range.lMax              = OI_JOY_AXIS_MAX;

    priv->obj->lpVtbl->SetProperty(priv->obj,
                                   DIPROP_RANGE,
                                   &range.diph);

    // Axis information/mapping
    if(!memcmp(&(obj_inst->guidType), &GUID_XAxis, sizeof(GUID)) ||
       !memcmp(&(obj_inst->guidType), &GUID_YAxis, sizeof(GUID)) ||
       !memcmp(&(obj_inst->guidType), &GUID_ZAxis, sizeof(GUID))) {
       dev->joyconfig->kind[obj_inst->dwOfs] = OIJ_GEN_AXIS;
    }
    else if(!memcmp(&(obj_inst->guidType), &GUID_RxAxis, sizeof(GUID)) ||
            !memcmp(&(obj_inst->guidType), &GUID_RxAxis, sizeof(GUID)) ||
            !memcmp(&(obj_inst->guidType), &GUID_RxAxis, sizeof(GUID))) {
      dev->joyconfig->kind[obj_inst->dwOfs] = OIJ_RUDDER;
    }
    else if(!memcmp(&(obj_inst->guidType), &GUID_Slider, sizeof(GUID))) {
      dev->joyconfig->kind[obj_inst->dwOfs] = OIJ_THROTTLE;
    }
    else if(!memcmp(&(obj_inst->guidType), &GUID_POV, sizeof(GUID))) {
      dev->joyconfig->kind[obj_inst->dwOfs] = OIJ_HAT;
    }
    else if(!memcmp(&(obj_inst->guidType), &GUID_Button, sizeof(GUID))) {
      dev->joyconfig->kind[obj_inst->dwOfs] = OIJ_GEN_BUTTON;
    }
    else {
      dev->joyconfig->kind[obj_inst->dwOfs] = OIJ_NONE;
    }

    // DI collects multi-axis information for us, so no need for pairing
    dev->joyconfig->pair[obj_inst->dwOfs] = 0;

    return DIENUM_CONTINUE;
}

/* ******************************************************************** */
