/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "framelistener_bind.h"

#include <vector>
#include <OgreRoot.h>
#include <OgreFrameListener.h>

// TODO: framelistener cache vector has 1:1 relationship with a Root instance; use hashtable or create Root struct with Root insance and this list


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

void add_frame_listener(RootHandle root_handle, FrameListenerEvent frame_event,int frame_event_type)
{
  Ogre::Root* root = reinterpret_cast<Ogre::Root*>(root_handle);
  FrameListenerBind *frameListener =
    new FrameListenerBind(
			  ( frame_event_type&EVENT_FRAME_STARTED ? frame_event : 0 ),
			  ( frame_event_type&EVENT_FRAME_RENDERING_QUEUED ? frame_event : 0 ),
			  ( frame_event_type&EVENT_FRAME_ENDED ? frame_event : 0 ));
  root->addFrameListener(frameListener);
  frameListenerList.push_back(frameListener);
}

void remove_frame_listener(RootHandle root_handle, FrameListenerEvent frame_event)
{
  Ogre::Root* root = reinterpret_cast<Ogre::Root*>(root_handle);
  // this loop handles multiple occurances of frame_event in vector
  // and deletes them all
  for(std::vector<FrameListenerBind*>::iterator it = frameListenerList.begin();it!=frameListenerList.end();)
    {
      if ((*it)->frameStartedHandle == frame_event ||
	  (*it)->frameEndedHandle == frame_event ||
	  (*it)->frameRenderingQueuedHandle == frame_event)
	{
	  root->removeFrameListener(*it);
	  delete *it;
	  it = frameListenerList.erase(it);
	}	else {
	++it;
      }
    }
}


// Variation on FrameListenerBind, allows the client to pass void* userdata back and forth.
class FrameListenerCTX: public Ogre::FrameListener
{
public:
    FrameListenerCTX(FrameStarted started_cb, FrameQueued queued_cb, FrameEnded ended_cb, void* data)
        : started(started_cb), queued(queued_cb), ended(ended_cb), userdata(data)
    {
    }

    bool frameStarted(const Ogre::FrameEvent& evt)
    {
        FrameEvent e;
        e.time_since_last_event = evt.timeSinceLastEvent;
        e.time_since_last_frame = evt.timeSinceLastFrame;
        return started(&e, userdata);
    }

    bool frameRenderingQueued(const Ogre::FrameEvent& evt)
    {
        FrameEvent e;
        e.time_since_last_event = evt.timeSinceLastEvent;
        e.time_since_last_frame = evt.timeSinceLastFrame;
        return queued(&e, userdata);
    }

    bool frameEnded(const Ogre::FrameEvent& evt)
    {
        FrameEvent e;
        e.time_since_last_event = evt.timeSinceLastEvent;
        e.time_since_last_frame = evt.timeSinceLastFrame;
        return ended(&e, userdata);
    }

    FrameStarted started;
    FrameQueued queued;
    FrameEnded ended;
    void* userdata;
};



FrameListenerHandle add_frame_listener_ctx(FrameStarted started_cb, FrameQueued queued_cb, FrameEnded ended_cb, void* userdata)
{
    FrameListenerCTX *frameListener = new FrameListenerCTX(started_cb, queued_cb, ended_cb, userdata);
    Ogre::Root::getSingletonPtr()->addFrameListener(frameListener);
    return reinterpret_cast<FrameListenerHandle>(frameListener);
}

void remove_frame_listener_ctx(FrameListenerHandle handle)
{
    FrameListenerCTX* listener = reinterpret_cast<FrameListenerCTX*>(handle);
    Ogre::Root::getSingletonPtr()->addFrameListener(listener);
    delete listener;
}
