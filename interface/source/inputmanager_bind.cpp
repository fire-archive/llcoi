/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "inputmanager_bind.h"

#include <OISInputManager.h>
#include <OISMouse.h>
#include <OISKeyboard.h>

InputManagerHandle create_input_system(unsigned int window_handle)
{
  OIS::InputManager* input_manager = OIS::InputManager::createInputSystem(window_handle);
  return reinterpret_cast<OIS::InputManager*>(input_manager);
}

void destroy_input_system(InputManagerHandle input_manager_handle)
{
  OIS::InputManager* input_manager = reinterpret_cast<OIS::InputManager*>(input_manager_handle);
  OIS::InputManager::destroyInputSystem(input_manager);
}

MouseInputHandle create_mouse_object(InputManagerHandle input_manager_handle, int buffered)
{
  OIS::InputManager* input_manager = reinterpret_cast<OIS::InputManager*>(input_manager_handle);
  OIS::Mouse* mouse = static_cast<OIS::Mouse*>(input_manager->createInputObject( OIS::OISMouse, (bool)buffered ));
  return reinterpret_cast<MouseInputHandle>(mouse);
}

KeyboardInputHandle create_keyboard_object(InputManagerHandle input_manager_handle, int buffered)
{
  OIS::InputManager* input_manager = reinterpret_cast<OIS::InputManager*>(input_manager_handle);
  OIS::Keyboard* keyboard = static_cast<OIS::Keyboard*>(input_manager->createInputObject( OIS::OISKeyboard, (bool)buffered ));
  return reinterpret_cast<KeyboardInputHandle>(keyboard);
}

void destroy_mouse_object(InputManagerHandle input_manager_handle, MouseInputHandle mouse_handle)
{
  OIS::InputManager* input_manager = reinterpret_cast<OIS::InputManager*>(input_manager_handle);
  OIS::Mouse* mouse = reinterpret_cast<OIS::Mouse*>(mouse_handle);
  input_manager->destroyInputObject(mouse);
}


void destroy_keyboard_object(InputManagerHandle input_manager_handle, KeyboardInputHandle keyboard_handle)
{
  OIS::InputManager* input_manager = reinterpret_cast<OIS::InputManager*>(input_manager_handle);
  OIS::Keyboard* keyboard = reinterpret_cast<OIS::Keyboard*>(keyboard_handle);
  input_manager->destroyInputObject(keyboard);
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
