/******************************************************************************
 * framelistener_bind.h - bindings for Ogre::FrameListener
 ******************************************************************************
 * This file is part of
 *     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 *                          
 * Low Level C Ogre Interface (llcoi)
 *
 * See http://code.google.com/p/llcoi/ for more information.
 *
 * Copyright (c) 2011, Llcoi Team
 * 
 * License: MIT
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 ******************************************************************************/
#pragma once

#define EVENT_FRAME_STARTED 1
#define EVENT_FRAME_RENDERING_QUEUED 2
#define EVENT_FRAME_ENDED 4

typedef void* FrameListenerHandle;

#include "ogre_interface.h"

typedef struct
{
    coiReal time_since_last_event;
    coiReal time_since_last_frame;
} FrameEvent;

typedef int(*FrameStarted)(const FrameEvent* event, void* userdata);
typedef int(*FrameEnded)(const FrameEvent* event, void* userdata);
typedef int(*FrameQueued)(const FrameEvent* event, void* userdata);
typedef int(*FrameListenerEvent)(float,float,int);

DLL void add_frame_listener(RootHandle root_handle, FrameListenerEvent frame_event,int frame_event_type);

DLL void remove_frame_listener(RootHandle root_handle, FrameListenerEvent frame_event);

DLL void add_frame_listener(FrameListenerEvent frame_event,int frame_event_type);

DLL void remove_frame_listener(FrameListenerEvent frame_event);

DLL FrameListenerHandle add_frame_listener_ctx(FrameStarted started_cb, FrameQueued queued_cb, FrameEnded ended_cb, void* userdata);

DLL void remove_frame_listener_ctx(FrameListenerHandle handle);
