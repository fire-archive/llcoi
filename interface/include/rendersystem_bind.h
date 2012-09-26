#pragma once

typedef void* RenderSystemHandle;
typedef void* RenderSystemListHandle;

#include "ogre_interface.h"

DLL void set_render_system(RenderSystemHandle render_system);
DLL void add_render_system(RenderSystemHandle render_system);
DLL RenderSystemHandle get_render_system();
DLL RenderSystemHandle get_render_system_by_name(const char* render_system_name);
DLL const char * render_system_get_name(RenderSystemHandle handle);
DLL void render_system_set_config_option(RenderSystemHandle render_system_handle, const char* option, const char* value);
DLL unsigned int render_system_list_size(RenderSystemListHandle list_handle);
DLL RenderSystemHandle render_system_list_get(RenderSystemListHandle list_handle, unsigned int index);
DLL void destroy_render_system_list(RenderSystemListHandle handle);
