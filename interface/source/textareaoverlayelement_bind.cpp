/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "textareaoverlayelement_bind.h"
#include "binding_utils.h"
#include <Overlay/OgreTextAreaOverlayElement.h>

//TextAreaOverlayElement(const String& name);
TextAreaOverlayElementHandle create_textareaoverlayelement(const char* name)
{
    Ogre::TextAreaOverlayElement* toe = new Ogre::TextAreaOverlayElement(Ogre::String(name));
    return static_cast<TextAreaOverlayElementHandle>(toe);
}

//~TextAreaOverlayElement();
void destroy_textareaoverlayelement(TextAreaOverlayElementHandle handle)
{
    Ogre::TextAreaOverlayElement* toe = static_cast<Ogre::TextAreaOverlayElement*>(handle);
    delete toe;
}

//void initialise(void);
void textareaoverlayelement_initialise(TextAreaOverlayElementHandle handle)
{
    Ogre::TextAreaOverlayElement* toe = static_cast<Ogre::TextAreaOverlayElement*>(handle);
    toe->initialise();
}

//void setCaption(const DisplayString& text);
void textareaoverlayelement_set_caption(TextAreaOverlayElementHandle handle, const char* text)
{
    Ogre::TextAreaOverlayElement* toe = static_cast<Ogre::TextAreaOverlayElement*>(handle);
    toe->setCaption(Ogre::DisplayString(text));
}

//void setCharHeight( Real height );
void textareaoverlayelement_set_char_height(TextAreaOverlayElementHandle handle, coiReal height)
{
    Ogre::TextAreaOverlayElement* toe = static_cast<Ogre::TextAreaOverlayElement*>(handle);
    toe->setCharHeight(height);
}

//Real getCharHeight() const;
coiReal textareaoverlayelement_get_char_height(const TextAreaOverlayElementHandle handle)
{
    const Ogre::TextAreaOverlayElement* toe = static_cast<const Ogre::TextAreaOverlayElement*>(handle);
    return toe->getCharHeight();
}

//void setSpaceWidth( Real width );
void textareaoverlayelement_set_space_width(TextAreaOverlayElementHandle handle, coiReal width)
{
    Ogre::TextAreaOverlayElement* toe = static_cast<Ogre::TextAreaOverlayElement*>(handle);
    toe->setSpaceWidth(width);
}

//Real getSpaceWidth() const;
coiReal textareaoverlayelement_get_space_width(const TextAreaOverlayElementHandle handle)
{
    const Ogre::TextAreaOverlayElement* toe = static_cast<const Ogre::TextAreaOverlayElement*>(handle);
    return toe->getSpaceWidth();
}

//void setFontName( const String& font );
void textareaoverlayelement_set_font_name(TextAreaOverlayElementHandle handle, const char* font)
{
    Ogre::TextAreaOverlayElement* toe = static_cast<Ogre::TextAreaOverlayElement*>(handle);
    toe->setFontName(Ogre::String(font));
}

//const String& getFontName() const;
const char* textareaoverlayelement_get_font_name(const TextAreaOverlayElementHandle handle)
{
    const Ogre::TextAreaOverlayElement* toe = static_cast<const Ogre::TextAreaOverlayElement*>(handle);
    return toe->getFontName().c_str();
}

//const String& getTypeName(void) const;
const char* textareaoverlayelement_get_type_name(const TextAreaOverlayElementHandle handle)
{
    const Ogre::TextAreaOverlayElement* toe = static_cast<const Ogre::TextAreaOverlayElement*>(handle);
    return toe->getTypeName().c_str();
}

//TODO: const MaterialPtr& getMaterial(void) const;
//TODO: void getRenderOperation(RenderOperation& op);

//void setMaterialName(const String& matName);
void textareaoverlayelement_set_material_name(TextAreaOverlayElementHandle handle, const char* mat_name)
{
    Ogre::TextAreaOverlayElement* toe = static_cast<Ogre::TextAreaOverlayElement*>(handle);
    toe->setMaterialName(Ogre::String(mat_name));
}

//void setColour(const ColourValue& col);
void textareaoverlayelement_set_colour(TextAreaOverlayElementHandle handle, const coiColourValue* c)
{
    Ogre::TextAreaOverlayElement* toe = static_cast<Ogre::TextAreaOverlayElement*>(handle);
    const Ogre::ColourValue colour(c->r, c->b, c->g, c->a);
    toe->setColour(colour);
}

//const ColourValue& getColour(void) const;
void textareaoverlayelement_get_colour(const TextAreaOverlayElementHandle handle, coiColourValue* result)
{
    const Ogre::TextAreaOverlayElement* toe = static_cast<const Ogre::TextAreaOverlayElement*>(handle);
    const Ogre::ColourValue& colour = toe->getColour();

    result->r = colour.r;
    result->g = colour.b;
    result->b = colour.g;
    result->a = colour.a;
}

//void setColourBottom(const ColourValue& col);
void textareaoverlayelement_set_colour_bottom(TextAreaOverlayElementHandle handle, const coiColourValue* c)
{
    Ogre::TextAreaOverlayElement* toe = static_cast<Ogre::TextAreaOverlayElement*>(handle);
    const Ogre::ColourValue colour(c->r, c->b, c->g, c->a);
    toe->setColourBottom(colour);
}

//const ColourValue& getColourBottom(void) const;
void textareaoverlayelement_get_colour_bottom(const TextAreaOverlayElementHandle handle, coiColourValue* result)
{
    const Ogre::TextAreaOverlayElement* toe = static_cast<const Ogre::TextAreaOverlayElement*>(handle);
    const Ogre::ColourValue& colour = toe->getColourBottom();

    result->r = colour.r;
    result->g = colour.b;
    result->b = colour.g;
    result->a = colour.a;
}

//void setColourTop(const ColourValue& col);
void textareaoverlayelement_set_colour_top(TextAreaOverlayElementHandle handle, const coiColourValue* c)
{
    Ogre::TextAreaOverlayElement* toe = static_cast<Ogre::TextAreaOverlayElement*>(handle);
    const Ogre::ColourValue colour(c->r, c->b, c->g, c->a);
    toe->setColourTop(colour);
}

//const ColourValue& getColourTop(void) const;
void textareaoverlayelement_get_colour_top(const TextAreaOverlayElementHandle handle, coiColourValue* result)
{
    const Ogre::TextAreaOverlayElement* toe = static_cast<const Ogre::TextAreaOverlayElement*>(handle);
    const Ogre::ColourValue& colour = toe->getColourTop();

    result->r = colour.r;
    result->g = colour.b;
    result->b = colour.g;
    result->a = colour.a;
}

//void setAlignment( Alignment a );
void textareaoverlayelement_set_alignment(TextAreaOverlayElementHandle handle, textarea_overlayelement_alignment a)
{
    Ogre::TextAreaOverlayElement* toe = static_cast<Ogre::TextAreaOverlayElement*>(handle);
    Ogre::TextAreaOverlayElement::Alignment align = llcoi_textarea_alignment_to_ogre_textarea_alignment(a);
    toe->setAlignment(align);
}

//Alignment getAlignment() const
textarea_overlayelement_alignment textareaoverlayelement_get_alignment(const TextAreaOverlayElementHandle handle)
{
    const Ogre::TextAreaOverlayElement* toe = static_cast<const Ogre::TextAreaOverlayElement*>(handle);
    Ogre::TextAreaOverlayElement::Alignment align = toe->getAlignment();
    return ogre_textarea_alignment_to_llcoi_textarea_alignment(align);
}

//void setMetricsMode(GuiMetricsMode gmm);
void textareaoverlayelement_set_metrics_mode(TextAreaOverlayElementHandle handle, gui_metrics_mode gmm)
{
    Ogre::TextAreaOverlayElement* toe = static_cast<Ogre::TextAreaOverlayElement*>(handle);
    Ogre::GuiMetricsMode GMM = llcoi_gui_metrics_mode_to_ogre_gui_metrics_mode(gmm);
    toe->setMetricsMode(GMM);
}

//void _update(void);
void textareaoverlayelement__update(TextAreaOverlayElementHandle handle)
{
    Ogre::TextAreaOverlayElement* toe = static_cast<Ogre::TextAreaOverlayElement*>(handle);
    toe->_update();
}
