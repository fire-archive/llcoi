/******************************************************************************
 * ois_bind.cpp - bindings for OIS
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
#include "ois_interface.h"

#include <OISMouse.h>
#include <OISKeyboard.h>
#include <OISJoyStick.h>
#include <OISInputManager.h>


InputSystemHandle create_input_system(unsigned int window_handle)
{
	OIS::InputManager* input = OIS::InputManager::createInputSystem(window_handle);
    return reinterpret_cast<InputSystemHandle>(input);
}

InputSystemHandle create_input_system_ex(ParamListHandle handle)
{
    OIS::ParamList* paramlist = reinterpret_cast<OIS::ParamList*>(handle);
    OIS::InputManager* input = OIS::InputManager::createInputSystem(*paramlist);
    return reinterpret_cast<InputSystemHandle>(input);
}

void destroy_input_system(InputSystemHandle handle)
{
    OIS::InputManager* input = reinterpret_cast<OIS::InputManager*>(handle);
	OIS::InputManager::destroyInputSystem(input);
}

MouseInputHandle create_mouse_object(InputSystemHandle handle, int buffered)
{
    OIS::InputManager* input = reinterpret_cast<OIS::InputManager*>(handle);
	OIS::Mouse* mouse = static_cast<OIS::Mouse*>(input->createInputObject( OIS::OISMouse, (bool)buffered ));
	return reinterpret_cast<MouseInputHandle>(mouse);
}

KeyboardInputHandle create_keyboard_object(InputSystemHandle handle, int buffered)
{
    OIS::InputManager* input = reinterpret_cast<OIS::InputManager*>(handle);
    OIS::Keyboard* keyboard = static_cast<OIS::Keyboard*>(input->createInputObject( OIS::OISKeyboard, (bool)buffered ));
	return reinterpret_cast<KeyboardInputHandle>(keyboard);
}

void destroy_mouse_object(InputSystemHandle handle, MouseInputHandle mouse_handle)
{
    OIS::InputManager* input = reinterpret_cast<OIS::InputManager*>(handle);
	OIS::Mouse* mouse = reinterpret_cast<OIS::Mouse*>(mouse_handle);
	input->destroyInputObject(mouse);
}

void destroy_keyboard_object(InputSystemHandle handle, KeyboardInputHandle keyboard_handle)
{
    OIS::InputManager* input = reinterpret_cast<OIS::InputManager*>(handle);
	OIS::Keyboard* keyboard = reinterpret_cast<OIS::Keyboard*>(keyboard_handle);
	input->destroyInputObject(keyboard);
}

int keyboard_is_key_down(KeyboardInputHandle keyboard_handle, enum KeyCode key_code)
{
    OIS::Keyboard* keyboard = reinterpret_cast<OIS::Keyboard*>(keyboard_handle);
    if(keyboard->isKeyDown((OIS::KeyCode)key_code))
        return 1;
    return 0;
}

int keyboard_is_modifier_down(KeyboardInputHandle keyboard_handle, Key_Modifier key_modifier)
{
    OIS::Keyboard* keyboard = reinterpret_cast<OIS::Keyboard*>(keyboard_handle);
    if(keyboard->isModifierDown((OIS::Keyboard::Modifier)key_modifier))
        return 1;
    return 0;
}

MouseState mouse_get_state(MouseInputHandle mouse_handle)
{
    OIS::Mouse* mouse = reinterpret_cast<OIS::Mouse*>(mouse_handle);
    OIS::MouseState state = mouse->getMouseState();
    MouseState out_state;
    out_state.buttons = state.buttons;
    out_state.height = state.height;
    out_state.width = state.width;
    out_state.X.abs = state.X.abs;
    out_state.X.rel = state.X.rel;
    out_state.X.absOnly = state.X.absOnly;
    out_state.Y.abs = state.Y.abs;
    out_state.Y.rel = state.Y.rel;
    out_state.Y.absOnly = state.Y.absOnly;
    out_state.Z.abs = state.Z.abs;
    out_state.Z.rel = state.Z.rel;
    out_state.Z.absOnly = state.Z.absOnly;
    return out_state;
}

void mouse_set_buffered(MouseInputHandle mouse_handle, int buffered)
{
    OIS::Mouse* mouse = reinterpret_cast<OIS::Mouse*>(mouse_handle);
    mouse->setBuffered((bool)buffered);
}

void keyboard_set_buffered(KeyboardInputHandle keyboard_handle, int buffered)
{
    OIS::Keyboard* keyboard = reinterpret_cast<OIS::Keyboard*>(keyboard_handle);
    keyboard->setBuffered((bool)buffered);
}

void keyboard_capture(KeyboardInputHandle keyboard_handle)
{
    OIS::Keyboard* keyboard = reinterpret_cast<OIS::Keyboard*>(keyboard_handle);
    keyboard->capture();
}

void mouse_capture(MouseInputHandle mouse_handle)
{
    OIS::Mouse* mouse = reinterpret_cast<OIS::Mouse*>(mouse_handle);
    mouse->capture();
}

ParamListHandle ois_create_paramlist()
{
    OIS::ParamList* paramlist = new OIS::ParamList;
    return reinterpret_cast<ParamListHandle>(paramlist);
}

void ois_destroy_paramlist(ParamListHandle handle)
{
    OIS::ParamList* paramlist = reinterpret_cast<OIS::ParamList*>(handle);
    delete paramlist;
}

void ois_add_pair(ParamListHandle handle, const char* field, const char* value)
{
    OIS::ParamList* paramlist = reinterpret_cast<OIS::ParamList*>(handle);
    paramlist->insert(
        std::make_pair(
            std::string(field), std::string(value)
        )
    );
}
