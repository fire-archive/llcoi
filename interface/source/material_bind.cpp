/******************************************************************************
 * material_bind.cpp - bindings for Ogre::Material
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

#include "material_bind.h"
#include "binding_utils.h"

//Material(ResourceManager* creator, const String& name, ResourceHandle handle, const String& group, bool isManual = false, ManualResourceLoader* loader = 0)
//~Material()
void destroy_material(MaterialHandle handle);
//Material& operator=( const Material& rhs )
//bool isTransparent() const
int material_is_transparent(const MaterialHandle handle);
//void setReceiveShadows(bool enabled) 
void material_set_receive_shadows(MaterialHandle handle);
//bool getReceiveShadows() const 
int material_get_receive_shadows(const MaterialHandle handle);
//void setTransparencyCastsShadows(bool enabled)
void material_set_transparency_casts_shadows(MaterialHandle handle, int enabled);
//bool getTransparencyCastsShadows() const
int material_get_transparency_casts_shadows(const MaterialHandle handle);
//Technique* createTechnique()
TechniqueHandle material_create_technique(MaterialHandle handle);
//Technique* getTechnique(unsigned short index)
TechniqueHandle material_get_technique_by_index(MaterialHandle handle, unsigned short index);
//Technique* getTechnique(const String& name)
TechniqueHandle material_get_technique_by_name(MaterialHandle handle, const char* name);
//unsigned short getNumTechniques() const
unsigned short material_get_num_techniques(const MaterialHandle handle);
//void removeTechnique(unsigned short index);		
void material_remove_technique(MaterialHandle handle, unsigned short index);
//void removeAllTechniques()
void material_remove_all_techniques(MaterialHandle handle);
//typedef VectorIterator<Techniques> TechniqueIterator
//TODO: TechniqueIterator getTechniqueIterator()
//TODO: TechniqueIterator getSupportedTechniqueIterator()
//Technique* getSupportedTechnique(unsigned short index)
TechniqueHandle material_get_supported_technique(MaterialHandle handle, unsigned short index);
//unsigned short getNumSupportedTechniques() const
unsigned short material_get_num_supported_techniques(const MaterialHandle handle);
//const String& getUnsupportedTechniquesExplanation() const
const char* material_get_unsupported_techniques_explanation(const MaterialHandle handle);
//unsigned short getNumLodLevels(unsigned short schemeIndex) const
unsigned short material_get_num_lod_levels_by_index(const MaterialHandle handle, unsigned short scheme_index);
//unsigned short getNumLodLevels(const String& schemeName) const
unsigned short material_get_num_lod_levels_by_name(const MaterialHandle handle, const char* scheme_name);
//Technique* getBestTechnique(unsigned short lodIndex = 0, const Renderable* rend = 0)
TechniqueHandle material_get_best_technique(MaterialHandle handle,  unsigned short lod_index, const RenderableHandle rend);
//MaterialPtr clone(const String& newName, bool changeGroup = false,  const String& newGroup = StringUtil::BLANK) const
MaterialPtrHandle material_clone(const MaterialHandle handle, const char* new_name, int change_group, const char* new_group);
//void copyDetailsTo(MaterialPtr& mat) const
void material_copy_details_to(const MaterialHandle handle, MaterialPtrHandle mat);
//void compile(bool autoManageTextureUnits = true)
void material_compile(MaterialHandle handle, int auto_manage_texture_units);
//void setPointSize(Real ps)
void material_set_point_size(MaterialHandle handle, coiReal ps);
//void setAmbient(Real red, Real green, Real blue)
void material_set_ambient_rgb(MaterialHandle handle, coiReal red, coiReal green, coiReal blue);
//void setAmbient(const ColourValue& ambient)
void material_set_ambient(MaterialHandle handle, const coiColourValue* ambient);
//void setDiffuse(Real red, Real green, Real blue, Real alpha)
void material_set_diffuse_rgba(MaterialHandle handle, coiReal red, coiReal green, coiReal blue, coiReal alpha);
//void setDiffuse(const ColourValue& diffuse)
void material_set_diffuse(MaterialHandle handle, const coiColourValue* diffuse);
//void setSpecular(Real red, Real green, Real blue, Real alpha)
void material_set_specular_rgba(MaterialHandle handle, coiReal red, coiReal green, coiReal blue, coiReal alpha);
//void setSpecular(const ColourValue& specular)
void material_set_specular(MaterialHandle handle, const coiColourValue* specular);
//void setShininess(Real val)
void material_set_shininess(MaterialHandle handle, coiReal val);
//void setSelfIllumination(Real red, Real green, Real blue)
void material_set_self_illumination_rgb(MaterialHandle handle, coiReal red, coiReal green, coiReal blue);
//void setSelfIllumination(const ColourValue& selfIllum)
void material_set_self_illumination(MaterialHandle handle, const coiColourValue* self_illum);
//void setDepthCheckEnabled(bool enabled)
void material_set_depth_check_enabled(MaterialHandle handle, int enabled);
//void setDepthWriteEnabled(bool enabled)
void material_set_depth_write_enabled(MaterialHandle handle, int enabled);
//void setDepthFunction( CompareFunction func )
void material_set_depth_function(MaterialHandle handle, compare_function func);
//void setColourWriteEnabled(bool enabled)
void material_set_colour_write_enabled(MaterialHandle handle, int enabled);
//void setCullingMode( CullingMode mode )
void material_set_culling_mode(MaterialHandle handle, culling_mode mode);
//void setManualCullingMode( ManualCullingMode mode )
void material_set_manual_culling_mode(MaterialHandle handle, manual_culling_mode mode);
//void setLightingEnabled(bool enabled)
void material_set_lighting_enabled(MaterialHandle handle, int enabled);
//void setShadingMode( ShadeOptions mode )
void material_set_shading_mode(MaterialHandle handle, shade_options mode);
//void setFog(bool overrideScene,  FogMode mode = FOG_NONE, const ColourValue& colour = ColourValue::White, Real expDensity = 0.001, Real linearStart = 0.0, Real linearEnd = 1.0 )
void material_set_fog(MaterialHandle handle, int overrride_scene, fog_mode mode, const coiColourValue* colour, coiReal exp_density, coiReal linear_start, coiReal linear_end);
//void setDepthBias(float constantBias, float slopeScaleBias)
void material_set_depth_bias(MaterialHandle handle, float constant_bias, float slope_scale_bias);
//void setTextureFiltering(TextureFilterOptions filterType)
void material_set_texture_filtering(MaterialHandle handle, texture_filter_options filter_type);
//void setTextureAnisotropy(int maxAniso)
void material_set_texture_anisotropy(MaterialHandle handle, int max_aniso);
//void setSceneBlending( const SceneBlendType sbt )
void material_set_scene_blending(MaterialHandle handle, const scene_blend_type sbt);
//void setSeparateSceneBlending( const SceneBlendType sbt, const SceneBlendType sbta )
void material_set_separate_scene_blending(MaterialHandle handle, const scene_blend_type sbt, const scene_blend_type sbta);
//void setSceneBlending( const SceneBlendFactor sourceFactor, const SceneBlendFactor destFactor)
void material_set_scene_blending_ex(MaterialHandle handle, const scene_blend_factor source_factor, const scene_blend_factor dest_factor);
//void setSeparateSceneBlending( const SceneBlendFactor sourceFactor, const SceneBlendFactor destFactor, const SceneBlendFactor sourceFactorAlpha, const SceneBlendFactor destFactorAlpha)
void material_set_separate_scene_blending_ex(MaterialHandle handle, const scene_blend_factor source_factor, const scene_blend_factor dest_factor, const scene_blend_factor source_factor_alpha, const scene_blend_factor dest_factor_alpha);
//void _notifyNeedsRecompile()
void material__notify_needs_recompile(MaterialHandle handle);
//void setLodLevels(const LodValueList& lodValues)
void material_set_lod_levels(MaterialHandle handle, coiReal lod_values[], size_t lod_count);
//TODO: LodValueIterator getLodValueIterator() const
//TODO: LodValueIterator getUserLodValueIterator() const
//ushort getLodIndex(Real value) const
unsigned short material_get_lod_index(const MaterialHandle handle, coiReal value);
//TODO: const LodStrategy *getLodStrategy() const
//TODO: void setLodStrategy(LodStrategy *lodStrategy)
//void touch() 
void material_touch(MaterialHandle handle);
//bool applyTextureAliases(const AliasTextureNamePairList& aliasList, const bool apply = true) const
int material_apply_texture_aliases(const MaterialHandle handle, const NameValuePairListHandle alias_list, int apply);
//bool getCompilationRequired() const
int material_get_compilation_required(const MaterialHandle handle);

//Material(ResourceManager* creator, const String& name, ResourceHandle handle, const String& group, bool isManual = false, ManualResourceLoader* loader = 0)
//~Material()
void destroy_material(MaterialHandle handle);
//Material& operator=( const Material& rhs )
//bool isTransparent() const
int material_is_transparent(const MaterialHandle handle);
//void setReceiveShadows(bool enabled) 
void material_set_receive_shadows(MaterialHandle handle);
//bool getReceiveShadows() const 
int material_get_receive_shadows(const MaterialHandle handle);
//void setTransparencyCastsShadows(bool enabled)
void material_set_transparency_casts_shadows(MaterialHandle handle, int enabled);
//bool getTransparencyCastsShadows() const
int material_get_transparency_casts_shadows(const MaterialHandle handle);
//Technique* createTechnique()
TechniqueHandle material_create_technique(MaterialHandle handle);
//Technique* getTechnique(unsigned short index)
TechniqueHandle material_get_technique_by_index(MaterialHandle handle, unsigned short index);
//Technique* getTechnique(const String& name)
TechniqueHandle material_get_technique_by_name(MaterialHandle handle, const char* name);
//unsigned short getNumTechniques() const
unsigned short material_get_num_techniques(const MaterialHandle handle);
//void removeTechnique(unsigned short index);		
void material_remove_technique(MaterialHandle handle, unsigned short index);
//void removeAllTechniques()
void material_remove_all_techniques(MaterialHandle handle);
//typedef VectorIterator<Techniques> TechniqueIterator
//TODO: TechniqueIterator getTechniqueIterator()
//TODO: TechniqueIterator getSupportedTechniqueIterator()
//Technique* getSupportedTechnique(unsigned short index)
TechniqueHandle material_get_supported_technique(MaterialHandle handle, unsigned short index);
//unsigned short getNumSupportedTechniques() const
unsigned short material_get_num_supported_techniques(const MaterialHandle handle);
//const String& getUnsupportedTechniquesExplanation() const
const char* material_get_unsupported_techniques_explanation(const MaterialHandle handle);
//unsigned short getNumLodLevels(unsigned short schemeIndex) const
unsigned short material_get_num_lod_levels_by_index(const MaterialHandle handle, unsigned short scheme_index);
//unsigned short getNumLodLevels(const String& schemeName) const
unsigned short material_get_num_lod_levels_by_name(const MaterialHandle handle, const char* scheme_name);
//Technique* getBestTechnique(unsigned short lodIndex = 0, const Renderable* rend = 0)
TechniqueHandle material_get_best_technique(MaterialHandle handle,  unsigned short lod_index, const RenderableHandle rend);
//MaterialPtr clone(const String& newName, bool changeGroup = false,  const String& newGroup = StringUtil::BLANK) const
MaterialPtrHandle material_clone(const MaterialHandle handle, const char* new_name, int change_group, const char* new_group);
//void copyDetailsTo(MaterialPtr& mat) const
void material_copy_details_to(const MaterialHandle handle, MaterialPtrHandle mat);
//void compile(bool autoManageTextureUnits = true)
void material_compile(MaterialHandle handle, int auto_manage_texture_units);
//void setPointSize(Real ps)
void material_set_point_size(MaterialHandle handle, coiReal ps);
//void setAmbient(Real red, Real green, Real blue)
void material_set_ambient_rgb(MaterialHandle handle, coiReal red, coiReal green, coiReal blue);
//void setAmbient(const ColourValue& ambient)
void material_set_ambient(MaterialHandle handle, const coiColourValue* ambient);
//void setDiffuse(Real red, Real green, Real blue, Real alpha)
void material_set_diffuse_rgba(MaterialHandle handle, coiReal red, coiReal green, coiReal blue, coiReal alpha);
//void setDiffuse(const ColourValue& diffuse)
void material_set_diffuse(MaterialHandle handle, const coiColourValue* diffuse);
//void setSpecular(Real red, Real green, Real blue, Real alpha)
void material_set_specular_rgba(MaterialHandle handle, coiReal red, coiReal green, coiReal blue, coiReal alpha);
//void setSpecular(const ColourValue& specular)
void material_set_specular(MaterialHandle handle, const coiColourValue* specular);
//void setShininess(Real val)
void material_set_shininess(MaterialHandle handle, coiReal val);
//void setSelfIllumination(Real red, Real green, Real blue)
void material_set_self_illumination_rgb(MaterialHandle handle, coiReal red, coiReal green, coiReal blue);
//void setSelfIllumination(const ColourValue& selfIllum)
void material_set_self_illumination(MaterialHandle handle, const coiColourValue* self_illum);
//void setDepthCheckEnabled(bool enabled)
void material_set_depth_check_enabled(MaterialHandle handle, int enabled);
//void setDepthWriteEnabled(bool enabled)
void material_set_depth_write_enabled(MaterialHandle handle, int enabled);
//void setDepthFunction( CompareFunction func )
void material_set_depth_function(MaterialHandle handle, compare_function func);
//void setColourWriteEnabled(bool enabled)
void material_set_colour_write_enabled(MaterialHandle handle, int enabled);
//void setCullingMode( CullingMode mode )
void material_set_culling_mode(MaterialHandle handle, culling_mode mode);
//void setManualCullingMode( ManualCullingMode mode )
void material_set_manual_culling_mode(MaterialHandle handle, manual_culling_mode mode);
//void setLightingEnabled(bool enabled)
void material_set_lighting_enabled(MaterialHandle handle, int enabled);
//void setShadingMode( ShadeOptions mode )
void material_set_shading_mode(MaterialHandle handle, shade_options mode);
//void setFog(bool overrideScene,  FogMode mode = FOG_NONE, const ColourValue& colour = ColourValue::White, Real expDensity = 0.001, Real linearStart = 0.0, Real linearEnd = 1.0 )
void material_set_fog(MaterialHandle handle, int overrride_scene, fog_mode mode, const coiColourValue* colour, coiReal exp_density, coiReal linear_start, coiReal linear_end);
//void setDepthBias(float constantBias, float slopeScaleBias)
void material_set_depth_bias(MaterialHandle handle, float constant_bias, float slope_scale_bias);
//void setTextureFiltering(TextureFilterOptions filterType)
void material_set_texture_filtering(MaterialHandle handle, texture_filter_options filter_type);
//void setTextureAnisotropy(int maxAniso)
void material_set_texture_anisotropy(MaterialHandle handle, int max_aniso);
//void setSceneBlending( const SceneBlendType sbt )
void material_set_scene_blending(MaterialHandle handle, const scene_blend_type sbt);
//void setSeparateSceneBlending( const SceneBlendType sbt, const SceneBlendType sbta )
void material_set_separate_scene_blending(MaterialHandle handle, const scene_blend_type sbt, const scene_blend_type sbta);
//void setSceneBlending( const SceneBlendFactor sourceFactor, const SceneBlendFactor destFactor)
void material_set_scene_blending_ex(MaterialHandle handle, const scene_blend_factor source_factor, const scene_blend_factor dest_factor);
//void setSeparateSceneBlending( const SceneBlendFactor sourceFactor, const SceneBlendFactor destFactor, const SceneBlendFactor sourceFactorAlpha, const SceneBlendFactor destFactorAlpha)
void material_set_separate_scene_blending_ex(MaterialHandle handle, const scene_blend_factor source_factor, const scene_blend_factor dest_factor, const scene_blend_factor source_factor_alpha, const scene_blend_factor dest_factor_alpha);
//void _notifyNeedsRecompile()
void material__notify_needs_recompile(MaterialHandle handle);
//void setLodLevels(const LodValueList& lodValues)
void material_set_lod_levels(MaterialHandle handle, coiReal lod_values[], size_t lod_count);
//TODO: LodValueIterator getLodValueIterator() const
//TODO: LodValueIterator getUserLodValueIterator() const
//ushort getLodIndex(Real value) const
unsigned short material_get_lod_index(const MaterialHandle handle, coiReal value);
//TODO: const LodStrategy *getLodStrategy() const
//TODO: void setLodStrategy(LodStrategy *lodStrategy)
//void touch() 
void material_touch(MaterialHandle handle);
//bool applyTextureAliases(const AliasTextureNamePairList& aliasList, const bool apply = true) const
int material_apply_texture_aliases(const MaterialHandle handle, const NameValuePairListHandle alias_list, int apply);
//bool getCompilationRequired() const
int material_get_compilation_required(const MaterialHandle handle);
