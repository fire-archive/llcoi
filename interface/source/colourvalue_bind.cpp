/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "ogre_interface.h"
#include "colourvalue_bind.h"
#include <OgreColourValue.h>

void colourvalue_zero(coiColourValue* c)
{
    Ogre::ColourValue cv = Ogre::ColourValue::ZERO;

    c->r = cv.r;
    c->g = cv.g;
    c->b = cv.b;
    c->a = cv.a;
}

void colourvalue_black(coiColourValue* c)
{
    Ogre::ColourValue cv = Ogre::ColourValue::Black;

    c->r = cv.r;
    c->g = cv.g;
    c->b = cv.b;
    c->a = cv.a;
}

void colourvalue_white(coiColourValue* c)
{
    Ogre::ColourValue cv = Ogre::ColourValue::White;
    c->r = cv.r;
    c->g = cv.g;
    c->b = cv.b;
    c->a = cv.a;
}

void colourvalue_red(coiColourValue* c)
{
    Ogre::ColourValue cv = Ogre::ColourValue::Red;
    c->r = cv.r;
    c->g = cv.g;
    c->b = cv.b;
    c->a = cv.a;
}

void colourvalue_green(coiColourValue* c)
{
    Ogre::ColourValue cv = Ogre::ColourValue::Green;
    c->r = cv.r;
    c->g = cv.g;
    c->b = cv.b;
    c->a = cv.a;
}

void colourvalue_blue(coiColourValue* c)
{
    Ogre::ColourValue cv = Ogre::ColourValue::Blue;
    c->r = cv.r;
    c->g = cv.g;
    c->b = cv.b;
    c->a = cv.a;
}

