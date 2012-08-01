/******************************************************************************
 * windoweventlistener_bind.cpp - bindings for Ogre::WindowEventListener
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
#include "ogre_interface.h"

#include <vector>
#include <OgreRoot.h>
#include <OgreWindowEventUtilities.h>

// this is a binding class, it has 1 function pointer for 
// window event listening, it gets called if not null
// Only supports one window event listener - for now
class WindowEventListenerBind : public Ogre::WindowEventListener
{
public:
	WindowEventListenerBind(WindowListenerEvent wc)
		: windowClosedHandle(wc)
	{

	}

	void windowClosed(Ogre::RenderWindow* rw)
	{
		if (windowClosedHandle) 
			windowClosedHandle(reinterpret_cast<RenderWindowHandle>(rw));
	}

	WindowListenerEvent windowClosedHandle;
};

WindowEventListenerBind *windowEventListener;

void add_window_listener(RenderWindowHandle window_handle, WindowListenerEvent window_event)
{
	windowEventListener = new WindowEventListenerBind(window_event);
	
	Ogre::WindowEventUtilities::addWindowEventListener(reinterpret_cast<Ogre::RenderWindow*>(window_handle), windowEventListener);
}

void remove_window_listener(RenderWindowHandle window_handle)
{
	Ogre::WindowEventUtilities::removeWindowEventListener(reinterpret_cast<Ogre::RenderWindow*>(window_handle), windowEventListener);
}


class WindowEventListenerCTX: public Ogre::WindowEventListener
{
public:
	WindowEventListenerCTX(WindowListenerEvent wc, void* data) : windowClosedHandle(wc), userdata(data)
	{
	}

	void windowClosed(Ogre::RenderWindow* rw)
	{
		if (windowClosedHandle) 
			windowClosedHandle(reinterpret_cast<RenderWindowHandle>(rw));
	}

	WindowListenerEvent windowClosedHandle;
    void* userdata;
};

WindowListenerHandle add_window_listener_ctx(RenderWindowHandle handle, WindowListenerEvent window_event, void* userdata)
{
    WindowEventListenerCTX *listener = new WindowEventListenerCTX(window_event, userdata);
    Ogre::WindowEventUtilities::addWindowEventListener(reinterpret_cast<Ogre::RenderWindow*>(handle), listener);
    return reinterpret_cast<WindowListenerHandle>(listener);
}
void remove_window_listener_ctx(RenderWindowHandle window_handle, WindowListenerHandle listener_handle)
{
    WindowEventListenerCTX* listener = reinterpret_cast<WindowEventListenerCTX*>(listener_handle);
    Ogre::RenderWindow* window = reinterpret_cast<Ogre::RenderWindow*>(window_handle);

	Ogre::WindowEventUtilities::removeWindowEventListener(window, listener);
    delete listener;
}


/*
Ogre::WindowEventUtilities::_msListeners
Ogre::WindowEventUtilities::_msWindows
Ogre::WindowEventUtilities::~WindowEventUtilities()
Ogre::WindowEventUtilities::operator=(Ogre::WindowEventUtilities const&)
Ogre::WindowEventUtilities::WindowEventUtilities(Ogre::WindowEventUtilities const&)
Ogre::WindowEventUtilities::WindowEventUtilities()
Ogre::WindowEventUtilities::messagePump()
Ogre::WindowEventUtilities::addWindowEventListener(Ogre::RenderWindow*, Ogre::WindowEventListener*)
Ogre::WindowEventUtilities::removeWindowEventListener(Ogre::RenderWindow*, Ogre::WindowEventListener*)
Ogre::WindowEventUtilities::_addRenderWindow(Ogre::RenderWindow*)
Ogre::WindowEventUtilities::_removeRenderWindow(Ogre::RenderWindow*)
*/
