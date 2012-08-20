/******************************************************************************
 * light_bind.h - bindings for Ogre::Light
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
#pragma once
#ifndef LIGHT_BIND_H
#define LIGHT_BIND_H

#include "ogre_interface.h"

#define LightHandle void*

// Light
DLL LightHandle create_light(const char* light_name);
DLL void destroy_light(LightHandle handle);
//Ogre::Light::setPosition() const
DLL void light_set_position(LightHandle light_handle, const float x, const float y, const float z);
//Ogre::Light::getPosition() const
DLL void light_get_position(LightHandle handle, coiVector3* pos);
//Ogre::Light::getPosition() const
DLL void light_get_position_xyz(LightHandle handle, float* x, float* y, float* z);
//Ogre::Light::setDirection(float, float, float)
DLL void light_set_direction_xyz(LightHandle handle, const float x, const float y, const float z);
//Ogre::Light::setDirection(Ogre::Vector3 const&)
DLL void light_set_direction(LightHandle handle, const coiVector3* direction);
//Ogre::Light::getDirection() const
DLL void light_get_direction(LightHandle handle, coiVector3* direction);
//Ogre::Light::setSpotlightRange(Ogre::Radian const&, Ogre::Radian const&, float)
DLL void light_set_spotlight_range(LightHandle handle, const coiReal inner_angle, const coiReal outer_angle, coiReal fall_off);
DLL void light_set_type(LightHandle handle, light_types type);
DLL void light_set_diffuse_colour(LightHandle handle, const ColourValue* colour);
DLL void light_set_specular_colour(LightHandle handle, const ColourValue* colour);

#endif
