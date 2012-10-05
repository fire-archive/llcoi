/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "windoweventlistener_bind.h"

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

void pump_messages()
{
    Ogre::WindowEventUtilities::messagePump();
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
