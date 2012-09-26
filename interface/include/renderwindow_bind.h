#pragma once

typedef void* RenderWindowHandle;
typedef void* ViewportHandle;
typedef void* CameraHandle;

#include "ogre_interface.h"

DLL RenderWindowHandle create_render_window(const char* name, const int width, const int height, const int full_screen);
DLL RenderWindowHandle create_render_window_gl_context(const char* name, const int width, const int height, const int full_screen);
DLL RenderWindowHandle create_render_window_hwnd(const char* name, const int width, const int height, const int full_screen, unsigned long hwnd);
DLL ViewportHandle render_window_add_viewport(RenderWindowHandle window_handle, CameraHandle camera_handle, int zorder, float left, float top, float width, float height);
DLL void render_window_set_active(RenderWindowHandle handle, int state);
DLL void render_window_swap_buffers(RenderWindowHandle handle, int wait_for_vsync);
DLL void render_window_get_custom_attribute(RenderWindowHandle handle, const char* attribute, void* pdata);
DLL unsigned int render_window_get_width(RenderWindowHandle handle);
DLL unsigned int render_window_get_height(RenderWindowHandle handle);
DLL void renderwindow_get_statistics(RenderWindowHandle handle, FrameStats* stats);
DLL void renderwindow_get_statistics_ex(RenderWindowHandle handle, float* lastFPS, float* avgFPS, float* bestFPS, float* worstFPS);
DLL void render_window_update(RenderWindowHandle window_handle, int swap_buffers);
DLL unsigned int render_window_get_hwnd(RenderWindowHandle window_handle);

DLL void render_window_set_visible(RenderWindowHandle window_handle, int visible);

DLL void current_window_update(RenderWindowHandle window_handle, int swap_buffers);

DLL void render_window_resize(RenderWindowHandle window_handle, unsigned int width, unsigned int height);

DLL void render_window_moved_or_resized(RenderWindowHandle window_handle);

DLL int render_window_closed(RenderWindowHandle window_handle);