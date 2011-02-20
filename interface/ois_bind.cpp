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

OIS::InputManager* input_manager = 0;

void create_input_system(unsigned int window_handle)
{
	input_manager = OIS::InputManager::createInputSystem(window_handle);
}

void destroy_input_system()
{
	OIS::InputManager::destroyInputSystem(input_manager);
}

MouseInputHandle create_mouse_object()
{
	OIS::Mouse* mouse = static_cast<OIS::Mouse*>(input_manager->createInputObject( OIS::OISMouse, true ));
	return reinterpret_cast<MouseInputHandle>(mouse);
}

KeyboardInputHandle create_keyboard_object()
{
	OIS::Keyboard* keyboard = static_cast<OIS::Keyboard*>(input_manager->createInputObject( OIS::OISKeyboard, true ));
	return reinterpret_cast<KeyboardInputHandle>(keyboard);
}

void destroy_mouse_object(MouseInputHandle mouse_handle)
{
	OIS::Mouse* mouse = reinterpret_cast<OIS::Mouse*>(mouse_handle);
	input_manager->destroyInputObject(mouse);
}

void destroy_keyboard_object(KeyboardInputHandle keyboard_handle)
{
	OIS::Keyboard* keyboard = reinterpret_cast<OIS::Keyboard*>(keyboard_handle);
	input_manager->destroyInputObject(keyboard);
}
