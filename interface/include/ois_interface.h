/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#pragma once

#include "ogre_interface.h"

#include "keyboard_bind.h"
#include "mouse_bind.h"
#include "inputmanager_bind.h"


// TODO: move somehwere else
DLL ParamListHandle ois_create_paramlist();

DLL void ois_destroy_paramlist(ParamListHandle handle);

DLL void ois_add_pair(ParamListHandle handle, const char* field, const char* value);
