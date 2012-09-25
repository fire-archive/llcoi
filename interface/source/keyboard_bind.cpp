#include "keyboard_bind.h"

#include <OISKeyboard.h>

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
