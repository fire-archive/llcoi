#pragma once

#include <OgreSingleton.h>
#include <OgreSceneManager.h>

class OgreManager : public Ogre::Singleton<OgreManager> 
{
public:
    OgreManager() :
        active_scene_manager_name("default"),
        plugin_folder("/usr/local/lib/OGRE"),
        activeRenderWindow(0),
        initialized(false)
    {};
    
    ~OgreManager()
    {};

    Ogre::String get_active_scene_manager_name() {
        return active_scene_manager_name;
    }
    Ogre::String get_plugin_folder() {
        return plugin_folder;
    }
    Ogre::RenderWindow* getActiveRenderWindow() {
        return activeRenderWindow;
    }
    bool get_initialized() {
        return initialized;
    }

    void set_active_scene_manager_name(Ogre::String name) {
        active_scene_manager_name = name;
    }
    void set_plugin_folder(Ogre::String folder) {
        plugin_folder = folder;
    }
    void setActiveRenderWindow(Ogre::RenderWindow* window) {
        activeRenderWindow = window;
    }
    void set_initialized(bool init) {
        initialized = init;
    }

private:
    Ogre::String active_scene_manager_name;
    Ogre::String plugin_folder;
    Ogre::RenderWindow* activeRenderWindow;
    bool initialized;
};

