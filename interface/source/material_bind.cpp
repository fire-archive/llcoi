/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "material_bind.h"
#include "binding_utils.h"
#include <OgreTechnique.h>

//Material(ResourceManager* creator, const String& name, ResourceHandle handle, const String& group, bool isManual = false, ManualResourceLoader* loader = 0)
//~Material()
void destroy_material(MaterialHandle handle)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    delete mat;
}

//Material& operator=( const Material& rhs )
//bool isTransparent() const
int material_is_transparent(const MaterialHandle handle)
{
    const Ogre::Material* mat = static_cast<const Ogre::Material*>(handle);
    return mat->isTransparent();
}

//void setReceiveShadows(bool enabled) 
void material_set_receive_shadows(MaterialHandle handle, int enabled)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    mat->setReceiveShadows(enabled);
}

//bool getReceiveShadows() const 
int material_get_receive_shadows(const MaterialHandle handle)
{
    const Ogre::Material* mat = static_cast<const Ogre::Material*>(handle);
    return mat->getReceiveShadows();
}

//void setTransparencyCastsShadows(bool enabled)
void material_set_transparency_casts_shadows(MaterialHandle handle, int enabled)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    mat->setTransparencyCastsShadows(enabled);
}

//bool getTransparencyCastsShadows() const
int material_get_transparency_casts_shadows(const MaterialHandle handle)
{
    const Ogre::Material* mat = static_cast<const Ogre::Material*>(handle);
    return mat->getTransparencyCastsShadows();
}

//Technique* createTechnique()
TechniqueHandle material_create_technique(MaterialHandle handle)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    return static_cast<TechniqueHandle>(
        mat->createTechnique()
    );
}

//Technique* getTechnique(unsigned short index)
TechniqueHandle material_get_technique_by_index(MaterialHandle handle, unsigned short index)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    return static_cast<TechniqueHandle>(
        mat->getTechnique(index)
    );
}

//Technique* getTechnique(const String& name)
TechniqueHandle material_get_technique_by_name(MaterialHandle handle, const char* name)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    return static_cast<TechniqueHandle>(
        mat->getTechnique(Ogre::String(name))
    );
}

//unsigned short getNumTechniques() const
unsigned short material_get_num_techniques(const MaterialHandle handle)
{
    const Ogre::Material* mat = static_cast<const Ogre::Material*>(handle);
    return mat->getNumTechniques();
}

//void removeTechnique(unsigned short index);		
void material_remove_technique(MaterialHandle handle, unsigned short index)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    return mat->removeTechnique(index);
}

//void removeAllTechniques()
void material_remove_all_techniques(MaterialHandle handle)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    mat->removeAllTechniques();
}

//typedef VectorIterator<Techniques> TechniqueIterator
//TODO: TechniqueIterator getTechniqueIterator()
//TODO: TechniqueIterator getSupportedTechniqueIterator()
//Technique* getSupportedTechnique(unsigned short index)
TechniqueHandle material_get_supported_technique(MaterialHandle handle, unsigned short index)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    return static_cast<TechniqueHandle>(
        mat->getSupportedTechnique(index)
    );
}

//unsigned short getNumSupportedTechniques() const
unsigned short material_get_num_supported_techniques(const MaterialHandle handle)
{
    const Ogre::Material* mat = static_cast<const Ogre::Material*>(handle);
    return mat->getNumSupportedTechniques();
}

//const String& getUnsupportedTechniquesExplanation() const
const char* material_get_unsupported_techniques_explanation(const MaterialHandle handle)
{
    const Ogre::Material* mat = static_cast<const Ogre::Material*>(handle);
    return mat->getUnsupportedTechniquesExplanation().c_str();
}

//unsigned short getNumLodLevels(unsigned short schemeIndex) const
unsigned short material_get_num_lod_levels_by_index(const MaterialHandle handle, unsigned short scheme_index)
{
    const Ogre::Material* mat = static_cast<const Ogre::Material*>(handle);
    return mat->getNumLodLevels(scheme_index);
}

//unsigned short getNumLodLevels(const String& schemeName) const
unsigned short material_get_num_lod_levels_by_name(const MaterialHandle handle, const char* scheme_name)
{
    const Ogre::Material* mat = static_cast<const Ogre::Material*>(handle);
    return mat->getNumLodLevels(Ogre::String(scheme_name));
}

//Technique* getBestTechnique(unsigned short lodIndex = 0, const Renderable* rend = 0)
TechniqueHandle material_get_best_technique(MaterialHandle handle,  unsigned short lod_index, const RenderableHandle rend)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    const Ogre::Renderable* r = static_cast<const Ogre::Renderable*>(rend);
    return static_cast<TechniqueHandle>(
        mat->getBestTechnique(lod_index, r)
    );
}

//MaterialPtr clone(const String& newName, bool changeGroup = false,  const String& newGroup = StringUtil::BLANK) const
MaterialPtrHandle material_clone(const MaterialHandle handle, const char* new_name, int change_group, const char* new_group)
{
    const Ogre::Material* mat = static_cast<const Ogre::Material*>(handle);
    Ogre::MaterialPtr ptr = mat->clone(Ogre::String(new_name), change_group, Ogre::String(new_group));
    return static_cast<MaterialPtrHandle>(ptr.get());
}

//void copyDetailsTo(MaterialPtr& mat) const
void material_copy_details_to(const MaterialHandle handle, MaterialPtrHandle ptrhnd)
{
    const Ogre::Material* mat = static_cast<const Ogre::Material*>(handle);
    Ogre::MaterialPtr ptr(static_cast<Ogre::Material*>(ptrhnd));
    mat->copyDetailsTo(ptr);
}

//void compile(bool autoManageTextureUnits = true)
void material_compile(MaterialHandle handle, int auto_manage_texture_units)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    mat->compile(auto_manage_texture_units);
}

//void setPointSize(Real ps)
void material_set_point_size(MaterialHandle handle, coiReal ps)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    mat->setPointSize(ps);
}

//void setAmbient(Real red, Real green, Real blue)
void material_set_ambient_rgb(MaterialHandle handle, coiReal red, coiReal green, coiReal blue)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    mat->setAmbient(red, green, blue);
}

//void setAmbient(const ColourValue& ambient)
void material_set_ambient(MaterialHandle handle, const coiColourValue* c)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    const Ogre::ColourValue ambient(c->r, c->g, c->b, c->a);
    mat->setAmbient(ambient);
}

//void setDiffuse(Real red, Real green, Real blue, Real alpha)
void material_set_diffuse_rgba(MaterialHandle handle, coiReal red, coiReal green, coiReal blue, coiReal alpha)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    mat->setDiffuse(red, green, blue, alpha);
}

//void setDiffuse(const ColourValue& diffuse)
void material_set_diffuse(MaterialHandle handle, const coiColourValue* c)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    const Ogre::ColourValue diffuse(c->r, c->g, c->b, c->a);
    mat->setDiffuse(diffuse);
}

//void setSpecular(Real red, Real green, Real blue, Real alpha)
void material_set_specular_rgba(MaterialHandle handle, coiReal red, coiReal green, coiReal blue, coiReal alpha)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    mat->setSpecular(red, green, blue, alpha);
}

//void setSpecular(const ColourValue& specular)
void material_set_specular(MaterialHandle handle, const coiColourValue* c)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    const Ogre::ColourValue specular(c->r, c->g, c->b, c->a);
    mat->setSpecular(specular);
}

//void setShininess(Real val)
void material_set_shininess(MaterialHandle handle, coiReal val)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    mat->setShininess(val);
}

//void setSelfIllumination(Real red, Real green, Real blue)
void material_set_self_illumination_rgb(MaterialHandle handle, coiReal red, coiReal green, coiReal blue)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    mat->setSelfIllumination(red, green, blue);
}

//void setSelfIllumination(const ColourValue& selfIllum)
void material_set_self_illumination(MaterialHandle handle, const coiColourValue* c)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    const Ogre::ColourValue selfIllum(c->r, c->g, c->b, c->a);
    mat->setSelfIllumination(selfIllum);
}

//void setDepthCheckEnabled(bool enabled)
void material_set_depth_check_enabled(MaterialHandle handle, int enabled)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    mat->setDepthCheckEnabled(enabled);
}

//void setDepthWriteEnabled(bool enabled)
void material_set_depth_write_enabled(MaterialHandle handle, int enabled)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    mat->setDepthWriteEnabled(enabled);
}

//void setDepthFunction( CompareFunction func )
void material_set_depth_function(MaterialHandle handle, compare_function func)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    mat->setDepthFunction(enum_converter(func));
}

//void setColourWriteEnabled(bool enabled)
void material_set_colour_write_enabled(MaterialHandle handle, int enabled)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    mat->setColourWriteEnabled(enabled);
}

//void setCullingMode( CullingMode mode )
void material_set_culling_mode(MaterialHandle handle, culling_mode mode)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    mat->setCullingMode(enum_converter(mode));
}

//void setManualCullingMode( ManualCullingMode mode )
void material_set_manual_culling_mode(MaterialHandle handle, manual_culling_mode mode)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle); 
    mat->setManualCullingMode(enum_converter(mode));
}

//void setLightingEnabled(bool enabled)
void material_set_lighting_enabled(MaterialHandle handle, int enabled)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle); 
    mat->setLightingEnabled(enabled);
}

//void setShadingMode( ShadeOptions mode )
void material_set_shading_mode(MaterialHandle handle, shade_options mode)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    mat->setShadingMode(enum_converter(mode));
}

//void setFog(bool overrideScene,  FogMode mode = FOG_NONE, const ColourValue& colour = ColourValue::White, Real expDensity = 0.001, Real linearStart = 0.0, Real linearEnd = 1.0 )
void material_set_fog(MaterialHandle handle, int overrride_scene, fog_mode mode, const coiColourValue* c, coiReal exp_density, coiReal linear_start, coiReal linear_end)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    const Ogre::ColourValue colour(c->r, c->g, c->b, c->a);
    mat->setFog(
        overrride_scene,
        enum_converter(mode),
        colour,
        exp_density,
        linear_start,
        linear_end
    );
}

//void setDepthBias(float constantBias, float slopeScaleBias)
void material_set_depth_bias(MaterialHandle handle, float constant_bias, float slope_scale_bias)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    mat->setDepthBias(constant_bias, slope_scale_bias);
}

//void setTextureFiltering(TextureFilterOptions filterType)
void material_set_texture_filtering(MaterialHandle handle, texture_filter_options filter_type)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    mat->setTextureFiltering(enum_converter(filter_type));
}

//void setTextureAnisotropy(int maxAniso)
void material_set_texture_anisotropy(MaterialHandle handle, int max_aniso)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    mat->setTextureAnisotropy(max_aniso);
}

//void setSceneBlending( const SceneBlendType sbt )
void material_set_scene_blending(MaterialHandle handle, const scene_blend_type sbt)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    mat->setSceneBlending(enum_converter(sbt));

}

//void setSeparateSceneBlending( const SceneBlendType sbt, const SceneBlendType sbta )
void material_set_separate_scene_blending(MaterialHandle handle, const scene_blend_type sbt, const scene_blend_type sbta)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle); 
    mat->setSeparateSceneBlending(enum_converter(sbt), enum_converter(sbta));
}

//void setSceneBlending( const SceneBlendFactor sourceFactor, const SceneBlendFactor destFactor)
void material_set_scene_blending_ex(MaterialHandle handle, const scene_blend_factor source_factor, const scene_blend_factor dest_factor)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    mat->setSceneBlending(enum_converter(source_factor), enum_converter(dest_factor));
}

//void setSeparateSceneBlending( const SceneBlendFactor sourceFactor, const SceneBlendFactor destFactor, const SceneBlendFactor sourceFactorAlpha, const SceneBlendFactor destFactorAlpha)
void material_set_separate_scene_blending_ex(MaterialHandle handle, const scene_blend_factor source_factor, const scene_blend_factor dest_factor, const scene_blend_factor source_factor_alpha, const scene_blend_factor dest_factor_alpha)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    mat->setSeparateSceneBlending(
        enum_converter(source_factor),
        enum_converter(dest_factor),
        enum_converter(source_factor_alpha),
        enum_converter(dest_factor_alpha)
    );
}

//void _notifyNeedsRecompile()
void material__notify_needs_recompile(MaterialHandle handle)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    mat->_notifyNeedsRecompile();
}

//void setLodLevels(const LodValueList& lodValues)
void material_set_lod_levels(MaterialHandle handle, coiReal lod_values[], size_t lod_count)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    const Ogre::Material::LodValueList lodValues(&lod_values[0], &lod_values[lod_count - 1]);
    mat->setLodLevels(lodValues);
}

//TODO: LodValueIterator getLodValueIterator() const
//TODO: LodValueIterator getUserLodValueIterator() const
//ushort getLodIndex(Real value) const
unsigned short material_get_lod_index(const MaterialHandle handle, coiReal value)
{
    const Ogre::Material* mat = static_cast<const Ogre::Material*>(handle);
    return mat->getLodIndex(value);
}

//TODO: const LodStrategy *getLodStrategy() const
//TODO: void setLodStrategy(LodStrategy *lodStrategy)
//void touch() 
void material_touch(MaterialHandle handle)
{
    Ogre::Material* mat = static_cast<Ogre::Material*>(handle);
    mat->touch();
}

//bool applyTextureAliases(const AliasTextureNamePairList& aliasList, const bool apply = true) const
int material_apply_texture_aliases(const MaterialHandle handle, const NameValuePairListHandle alias_list, int apply)
{
    const Ogre::Material* mat = static_cast<const Ogre::Material*>(handle);
    const Ogre::NameValuePairList* nvp = static_cast<const Ogre::NameValuePairList*>(alias_list);
    return mat->applyTextureAliases(*nvp, apply);
}

//bool getCompilationRequired() const
int material_get_compilation_required(const MaterialHandle handle)
{
    const Ogre::Material* mat = static_cast<const Ogre::Material*>(handle);
    return mat->getCompilationRequired();
}

