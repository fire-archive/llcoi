/******************************************************************************
 * framelistener_bind.cpp - bindings for Ogre::FrameListener
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
#include <OgreFrameListener.h>

// this is a binding class, it has 3 function pointers for 
// frame listening event, each gets called if not null
class FrameListenerBind : public Ogre::FrameListener
{
public:
	FrameListenerBind(FrameListenerEvent fs,FrameListenerEvent frq,FrameListenerEvent fe)
		: frameStartedHandle(fs),frameRenderingQueuedHandle(frq),frameEndedHandle(fe)
	{

	}

	bool frameStarted(const Ogre::FrameEvent& evt)
	{
		if (frameStartedHandle) 
			return frameStartedHandle(evt.timeSinceLastEvent,evt.timeSinceLastFrame,EVENT_FRAME_STARTED);
		return true;
	}
	bool frameRenderingQueued(const Ogre::FrameEvent& evt)
	{
		if (frameRenderingQueuedHandle) 
			return frameRenderingQueuedHandle(evt.timeSinceLastEvent,evt.timeSinceLastFrame,EVENT_FRAME_RENDERING_QUEUED);
		return true;
	}
	bool frameEnded(const Ogre::FrameEvent& evt)
	{
		if (frameEndedHandle) 
			return frameEndedHandle(evt.timeSinceLastEvent,evt.timeSinceLastFrame,EVENT_FRAME_ENDED);
		return true;
	}

	FrameListenerEvent frameStartedHandle;
	FrameListenerEvent frameEndedHandle;
	FrameListenerEvent frameRenderingQueuedHandle;
};

// list of frame listeners for deleting
std::vector<FrameListenerBind*> frameListenerList;

void add_frame_listener(FrameListenerEvent frame_event,int frame_event_type)
{
	FrameListenerBind *frameListener =
		new FrameListenerBind(
		( frame_event_type&EVENT_FRAME_STARTED ? frame_event : 0 ),
		( frame_event_type&EVENT_FRAME_RENDERING_QUEUED ? frame_event : 0 ),
		( frame_event_type&EVENT_FRAME_ENDED ? frame_event : 0 ));
	Ogre::Root::getSingletonPtr()->addFrameListener(frameListener);
	frameListenerList.push_back(frameListener);
}

void remove_frame_listener(FrameListenerEvent frame_event)
{
	// this loop handles multiple occurances of frame_event in vector
	// and deletes them all
	for(std::vector<FrameListenerBind*>::iterator it = frameListenerList.begin();it!=frameListenerList.end();)
	{
		if ((*it)->frameStartedHandle == frame_event ||
				(*it)->frameEndedHandle == frame_event ||
				(*it)->frameRenderingQueuedHandle == frame_event)
		{
			Ogre::Root::getSingletonPtr()->removeFrameListener(*it);
			delete *it;
			it = frameListenerList.erase(it);
		}	else {
			++it;
		}
	}
}
