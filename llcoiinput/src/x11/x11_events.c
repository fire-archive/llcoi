/*
 * x11_translate.c : Overall X11 event handler
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
#include <string.h>
#include <stdlib.h>
#include <X11/Xlib.h>
#include <sys/select.h>
#include "internal.h"
#include "x11.h"

/* ******************************************************************** */

/**
 * @ingroup DX11
 * @brief Handle relative mouse movement
 *
 * @param dev pointer to device interface
 * @param xev X mouse motion event
 *
 * When the mouse is grabbed and the cursor is hidden we
 * are in "relative mouse motion mode", as the user expects
 * the mouse to generate events even if the window border is hit
 * as she can't see that the border is actually hit...
 *
 * The trick is to make the cursor stay in the middle of
 * the window (using warp) and eating the extra X motion
 * events.
 */
void x11_relative_mouse(oi_device *dev, XEvent *xev) {
    x11_private *priv;
    int deltax;
    int deltay;
    int i;

    priv = (x11_private*)dev->private;

    // Calculate motion and store current positon
    deltax = xev->xmotion.x - priv->lastx;
    deltay = xev->xmotion.y - priv->lasty;
    priv->lastx = xev->xmotion.x;
    priv->lasty = xev->xmotion.y;

    // Post it
    mouse_move(dev->index, deltax, deltay, TRUE, TRUE);

    // Only warp mouse if we're near the edge of the window
    if((xev->xmotion.x < DX11_FUDGE) ||
       (xev->xmotion.x > (priv->width - DX11_FUDGE)) ||
       (xev->xmotion.y < DX11_FUDGE) ||
       (xev->xmotion.y > (priv->height - DX11_FUDGE))) {

        // Apply the SDL optimization: Events may have accumulated
        while(XCheckTypedEvent(priv->disp, MotionNotify, xev)) {
            // Treat as normal movement like above
            deltax = xev->xmotion.x - priv->lastx;
            deltay = xev->xmotion.y - priv->lasty;
            priv->lastx = xev->xmotion.x;
            priv->lasty = xev->xmotion.y;

            // Post it
            mouse_move(dev->index, deltax, deltay, TRUE, TRUE);
        }

        // Center (warp) mouse
        priv->lastx = priv->width / 2;
        priv->lasty = priv->height / 2;
        XWarpPointer(priv->disp, None, priv->win, 0, 0, 0, 0,
                     priv->lastx, priv->lasty);

        // Remove warp-generated motion events
        for ( i=0; i<10; ++i ) {
            XMaskEvent(priv->disp, PointerMotionMask, xev);
            if((xev->xmotion.x > (priv->lastx - DX11_FUDGE)) &&
               (xev->xmotion.x < (priv->lastx + DX11_FUDGE)) &&
               (xev->xmotion.y > (priv->lasty - DX11_FUDGE)) &&
               (xev->xmotion.y < (priv->lasty + DX11_FUDGE))) {
                break;
            }
        }
    }
}

/* ******************************************************************** */

/**
 * @ingroup DX11
 * @brief Check for pending X events
 *
 * @param d display handle
 * @returns true (1) if events are pending, false (0) otherwise
 *
 * Use this function to check for pending events from the
 * X server without going into a long-time blocking call.
 * The block is avoided using a "select attack" on the
 * X-server - this technique was borrowed from SDL. Thanks!
 */
int x11_pending(Display *d) {
    // Flush to pump the X pipeline
    XFlush(d);

    // Standard X non-blocking check
    if(XEventsQueued(d, QueuedAlready)) {
        return TRUE;
    }

    // Bruteforce-attack the X server to make it talk (thanks SDL!)
    {
        static struct timeval time;
        int fd;
        fd_set set;

        fd = ConnectionNumber(d);
        FD_ZERO(&set);
        FD_SET(fd, &set);

        // Charge!
        if(select(fd+1, &set, NULL, NULL, &time) == 1) {
            // Ok, perform the blocking X call
            return XPending(d);
        }
    }

    // Nothing is pending
    return FALSE;
}

/* ******************************************************************** */

/**
 * @ingroup DX11
 * @brief Cancel repeated key events
 *
 * @param d display handle
 * @param evt X event
 *
 * Make sure that the repeated key-down events from
 * the X server is thrown away. OpenInput has it's internal
 * keyrepeat system if you want repeat-events.
 */
char x11_keyrepeat(Display *d, XEvent *evt) {
    XEvent pev;
    char rep;

    rep = FALSE;

    if(XPending(d)) {
        XPeekEvent(d, &pev);

        // Same key down within threshold
        if((pev.type == KeyPress) &&
           (pev.xkey.keycode == evt->xkey.keycode) &&
           ((pev.xkey.time - evt->xkey.time) < DX11_REP_THRESHOLD)) {

            debug("x11_keyrepeat: repeating key detected");
            rep = 1;
            XNextEvent(d, &pev);
        }
    }

    return(rep);
}

/* ******************************************************************** */

/**
 * @ingroup DX11
 * @brief X11 event dispatcher
 *
 * @param dev pointer to device interface
 * @param d display handle
 *
 * This is where the real X action is! When this function
 * is called, a least a single event must be pending to
 * avoid a long-time block.
 *
 * What we do is fetch the X event, check the type and
 * convert it to the corresponding OpenInput event.
 * For the most part, this is handled in the
 * state managers (ie. keyboard, mouse, appstate, etc.)
 * For the keyboard events, we first translate the
 * X11 scancode into a OpenInput symbolic key.
 */
void x11_dispatch(oi_device *dev, Display *d) {
    XEvent xev;

    // Fetch the event
    XNextEvent(d, &xev);

    // Handle
    switch(xev.type) {

        // Mouse enters/leaves window
    case EnterNotify:
    case LeaveNotify:
        debug("x11_dispatch: enter/leave_notify (in/down:%i)", xev.type == EnterNotify);

        // We're not interested in grab mode events
        if((xev.xcrossing.mode != NotifyGrab) &&
           (xev.xcrossing.mode != NotifyUngrab)) {

            // Move if grabbed, otherwise change focus
            if(oi_app_grab(OI_QUERY) == OI_ENABLE) {
                mouse_move(dev->index, xev.xcrossing.x, xev.xcrossing.y, FALSE, TRUE);
            }
            else {
                appstate_focus(dev->index,
                               OI_FOCUS_MOUSE,
                               xev.type == EnterNotify,
                               TRUE);
            }
        }
        break;


        // Input focus gained/lost
    case FocusIn:
    case FocusOut:
        debug("x11_dispatch: focus_in/out (in/down:%i)", xev.type == FocusIn);
        appstate_focus(dev->index,
                       OI_FOCUS_INPUT,
                       xev.type == FocusIn,
                       TRUE);
        break;


        // Generated on EnterWindow and FocusIn
    case KeymapNotify:
        debug("x11_dispatch: keymap_notify");
        // Do a full reset of keystate/modstates
        x11_keystate(dev, d, xev.xkeymap.key_vector);
        break;

        // Mouse motion
    case MotionNotify:
        debug("x11_dispatch: motion_notify");
        // Mouse grabbed and hidden, using relative motion
        if(((x11_private*)dev->private)->relative == (DX11_GRAB | DX11_HIDE)) {
            x11_relative_mouse(dev, &xev);
        }
        else {
            mouse_move(dev->index, xev.xmotion.x, xev.xmotion.y, FALSE, TRUE);
        }

        break;


        // Mouse button pressed
    case ButtonPress:
    case ButtonRelease:
        debug("x11_dispatch: button_press/release (in/down:%i)", xev.type == ButtonPress);
        mouse_button(dev->index,
                     xev.xbutton.button,
                     xev.type == ButtonPress,
                     TRUE);
        break;


        // Keyboard pressed
    case KeyPress:
    case KeyRelease:
        debug("x11_dispatch: key_press/release (in/down:%i)", xev.type == KeyPress);
        {
            oi_keysym keysym;
            x11_private *priv;

            priv = (x11_private*)dev->private;

            // Do not post repeated keys
            if(!x11_keyrepeat(priv->disp, &xev)) {

                // Decode key and send it to the state manager
                x11_translate(priv->disp,
                              &xev.xkey,
                              xev.xkey.keycode,
                              &keysym);
                keyboard_update(dev->index,
                                &keysym,
                                xev.type == KeyPress,
                                TRUE);
            }
        }
        break;


        // Window gets iconified
    case UnmapNotify:
        debug("x11_dispatch: unmap_notify");
        appstate_focus(dev->index, FALSE, OI_FOCUS_INPUT | OI_FOCUS_VISIBLE, TRUE);
        break;


        // Window gets restored (uniconified)
    case MapNotify:
        debug("x11_dispatch: map_notify");
        appstate_focus(dev->index, TRUE, OI_FOCUS_VISIBLE, TRUE);
        break;


        // Window was resized or moved
    case ConfigureNotify:
        debug("x11_dispatch: configure_notify");
        // Only post update if anything changed
        if((((x11_private*)dev->private)->width != xev.xconfigure.width) &&
           (((x11_private*)dev->private)->height != xev.xconfigure.height)) {

            appstate_resize(dev->index, xev.xconfigure.width, xev.xconfigure.height, TRUE);
            ((x11_private*)dev->private)->width = xev.xconfigure.width;
            ((x11_private*)dev->private)->height = xev.xconfigure.height;
        }
        break;


        // We should quit -- or other messages
    case ClientMessage:
        debug("x11_dispatch: client_message");
        // Window manager close window
        if((xev.xclient.format == 32) &&
           (xev.xclient.data.l[0] == ((x11_private*)dev->private)->wm_delete_window)) {
            oi_event ev;
            ev.type = OI_QUIT;
            queue_add(&ev);
        }
        break;


        // Redraw required
    case Expose:
        debug("x11_dispatch: expose");
        {
            oi_event ev;
            ev.type = OI_EXPOSE;
            queue_add(&ev);
        }
        break;


        // Unhandled event
    default:
        debug("x11_dispatch: unhandled event type %i", xev.type);
        break;
    }
}

/* ******************************************************************** */
