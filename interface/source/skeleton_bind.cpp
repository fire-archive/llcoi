/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "skeleton_bind.h"
#include "binding_utils.h"
#include <OgreSkeleton.h>
#include <OgreBone.h>


//~Skeleton();
void destroy_skeleton(SkeletonHandle handle)
{
    Ogre::Skeleton* skeleton = static_cast<Ogre::Skeleton*>(handle);
    delete skeleton;
}

//Bone* createBone(void);
BoneHandle skeleton_create_bone(SkeletonHandle handle)
{
    Ogre::Skeleton* skeleton = static_cast<Ogre::Skeleton*>(handle);
    Ogre::Bone* bone = skeleton->createBone();
    return static_cast<BoneHandle>(bone);
}

//Bone* createBone(unsigned short handle);
BoneHandle skeleton_create_bone_with_handle(SkeletonHandle handle, unsigned short bone_handle)
{
    Ogre::Skeleton* skeleton = static_cast<Ogre::Skeleton*>(handle);
    Ogre::Bone* bone = skeleton->createBone(bone_handle);
    return static_cast<BoneHandle>(bone);
}

//Bone* createBone(const String& name);
BoneHandle skeleton_create_bone_with_name(SkeletonHandle handle, const char* name)
{
    Ogre::Skeleton* skeleton = static_cast<Ogre::Skeleton*>(handle);
    Ogre::Bone* bone = skeleton->createBone(Ogre::String(name));
    return static_cast<BoneHandle>(bone);
}

//Bone* createBone(const String& name, unsigned short handle);
BoneHandle skeleton_create_bone_with_name_and_handle(SkeletonHandle handle, const char* name, unsigned short bone_handle)
{
    Ogre::Skeleton* skeleton = static_cast<Ogre::Skeleton*>(handle);
    Ogre::Bone* bone = skeleton->createBone(Ogre::String(name), bone_handle);
    return static_cast<BoneHandle>(bone);
}

//unsigned short getNumBones(void) const;
unsigned short skeleton_get_num_bones(const SkeletonHandle handle)
{
    const Ogre::Skeleton* skeleton = static_cast<const Ogre::Skeleton*>(handle);
    return skeleton->getNumBones();
}

//Bone* getRootBone(void) const;
BoneHandle skeleton_get_root_bone(const SkeletonHandle handle)
{
    const Ogre::Skeleton* skeleton = static_cast<const Ogre::Skeleton*>(handle);
    Ogre::Bone* bone = skeleton->getRootBone();
    return static_cast<BoneHandle>(bone);
}

//typedef vector<Bone*>::type BoneList;
//typedef VectorIterator<BoneList> BoneIterator;
//TODO: BoneIterator getRootBoneIterator(void);
//TODO: BoneIterator getBoneIterator(void);
//Bone* getBone(unsigned short handle) const;
BoneHandle skeleton_get_bone_by_handle(const SkeletonHandle handle, unsigned short bone_handle)
{
    const Ogre::Skeleton* skeleton = static_cast<const Ogre::Skeleton*>(handle);
    Ogre::Bone* bone = skeleton->getBone(bone_handle);
    return static_cast<BoneHandle>(bone);
}

//Bone* getBone(const String& name) const;
BoneHandle skeleton_get_bone_by_name(const SkeletonHandle handle, const char* name)
{
    const Ogre::Skeleton* skeleton = static_cast<const Ogre::Skeleton*>(handle);
    Ogre::Bone* bone = skeleton->getBone(Ogre::String(name));
    return static_cast<BoneHandle>(bone);
}

//bool hasBone(const String& name) const;
int skeleton_has_bone(const SkeletonHandle handle, const char* name)
{
    const Ogre::Skeleton* skeleton = static_cast<const Ogre::Skeleton*>(handle);
    return skeleton->hasBone(Ogre::String(name));
}

//void setBindingPose(void);
void skeleton_set_binding_pose(SkeletonHandle handle)
{
    Ogre::Skeleton* skeleton = static_cast<Ogre::Skeleton*>(handle);
    skeleton->setBindingPose();
}

//void reset(bool resetManualBones = false);
void skeleton_reset(SkeletonHandle handle, int reset_manual_bones)
{
    Ogre::Skeleton* skeleton = static_cast<Ogre::Skeleton*>(handle);
    skeleton->reset(reset_manual_bones);
}

//TODO: Animation* createAnimation(const String& name, Real length);
//TODO: Animation* getAnimation(const String& name, const LinkedSkeletonAnimationSource** linker) const;
//TODO: Animation* getAnimation(const String& name) const;
//TODO: Animation* _getAnimationImpl(const String& name, const LinkedSkeletonAnimationSource** linker = 0) const;
//bool hasAnimation(const String& name) const;
int skeleton_has_animation(const SkeletonHandle handle, const char* name)
{
    const Ogre::Skeleton* skeleton = static_cast<const Ogre::Skeleton*>(handle);
    return skeleton->hasAnimation(Ogre::String(name));
}

//void removeAnimation(const String& name);
void skeleton_remove_animation(SkeletonHandle handle, const char* name)
{
    Ogre::Skeleton* skeleton = static_cast<Ogre::Skeleton*>(handle);
    skeleton->removeAnimation(Ogre::String(name));
}

//TODO: void setAnimationState(const AnimationStateSet& animSet);
//TODO: void _initAnimationState(AnimationStateSet* animSet);
//TODO: void _refreshAnimationState(AnimationStateSet* animSet);

//void _getBoneMatrices(Matrix4* pMatrices);
void skeleton__get_bone_matrices(SkeletonHandle handle, coiMatrix4* matrices[])
{
    Ogre::Skeleton* skeleton = static_cast<Ogre::Skeleton*>(handle);

    // Borrowed from OgreEntity.cpp
    unsigned short num = skeleton->getNumBones();
    Ogre::Matrix4* BoneMatrices = static_cast<Ogre::Matrix4*>(OGRE_MALLOC_SIMD(sizeof(Ogre::Matrix4) * num, Ogre::MEMCATEGORY_ANIMATION));
    skeleton->_getBoneMatrices(BoneMatrices);

    for (unsigned short current = 0; current != num; ++current)
    {
        Ogre::Matrix4 o_matrix = *BoneMatrices;
        ogre_matrix4_to_llcoi_matrix4(o_matrix, *matrices[current]);
    }
}

//unsigned short getNumAnimations(void) const;
unsigned short skeleton_get_num_animations(const SkeletonHandle handle)
{
    const Ogre::Skeleton* skeleton = static_cast<const Ogre::Skeleton*>(handle);
    return skeleton->getNumAnimations();
}

//TODO: Animation* getAnimation(unsigned short index) const;
//SkeletonAnimationBlendMode getBlendMode() const;
skeleton_animation_blend_mode skeleton_get_blend_mode(const SkeletonHandle handle)
{
    const Ogre::Skeleton* skeleton = static_cast<const Ogre::Skeleton*>(handle);
    Ogre::SkeletonAnimationBlendMode mode = skeleton->getBlendMode();
    return ogre_skeleton_blend_mode_to_llcoi(mode);
}

//void setBlendMode(SkeletonAnimationBlendMode state);
void skeleton_set_blend_mode(SkeletonHandle handle, skeleton_animation_blend_mode state)
{
    Ogre::Skeleton* skeleton = static_cast<Ogre::Skeleton*>(handle);
    Ogre::SkeletonAnimationBlendMode mode = llcoi_skeleton_blend_mode_to_ogre(state);
    skeleton->setBlendMode(mode);
}

//void _updateTransforms(void);
void skeleton__update_transforms(SkeletonHandle handle)
{
    Ogre::Skeleton* skeleton = static_cast<Ogre::Skeleton*>(handle);
    skeleton->_updateTransforms();
}

//void optimiseAllAnimations(bool preservingIdentityNodeTracks = false);
void skeleton_optimise_all_animations(SkeletonHandle handle, int preserving_identity_node_tracks)
{
    Ogre::Skeleton* skeleton = static_cast<Ogre::Skeleton*>(handle);
    skeleton->optimiseAllAnimations(preserving_identity_node_tracks);
}

//void addLinkedSkeletonAnimationSource(const String& skelName, Real scale = 1.0f);
void skeleton_add_linked_skeleton_animation_source(SkeletonHandle handle, const char* skel_name, coiReal scale)
{
    Ogre::Skeleton* skeleton = static_cast<Ogre::Skeleton*>(handle);
    skeleton->addLinkedSkeletonAnimationSource(Ogre::String(skel_name), scale);
}

//void removeAllLinkedSkeletonAnimationSources(void);
void skeleton_remove_all_linked_skeleton_animation_sources(SkeletonHandle handle)
{
    Ogre::Skeleton* skeleton = static_cast<Ogre::Skeleton*>(handle);
    skeleton->removeAllLinkedSkeletonAnimationSources();
}

//typedef vector<LinkedSkeletonAnimationSource>::type LinkedSkeletonAnimSourceList;
//typedef ConstVectorIterator<LinkedSkeletonAnimSourceList> LinkedSkeletonAnimSourceIterator;
//TODO: LinkedSkeletonAnimSourceIterator getLinkedSkeletonAnimationSourceIterator(void) const;
//void _notifyManualBonesDirty(void);
void skeleton__notify_manual_bones_dirty(SkeletonHandle handle)
{
    Ogre::Skeleton* skeleton = static_cast<Ogre::Skeleton*>(handle);
    skeleton->_notifyManualBonesDirty();
}

//void _notifyManualBoneStateChange(Bone* bone);
void skeleton__notify_manual_bone_state_change(SkeletonHandle handle, BoneHandle bone)
{
    Ogre::Skeleton* skeleton = static_cast<Ogre::Skeleton*>(handle);
    Ogre::Bone* b = static_cast<Ogre::Bone*>(bone);
    skeleton->_notifyManualBoneStateChange(b);
}

//bool getManualBonesDirty(void) const;
int skeleton_get_manual_bones_dirty(const SkeletonHandle handle)
{
    const Ogre::Skeleton* skeleton = static_cast<const Ogre::Skeleton*>(handle);
    return skeleton->getManualBonesDirty();
}

//bool hasManualBones(void) const;
int skeleton_has_manual_bones(const SkeletonHandle handle)
{
    const Ogre::Skeleton* skeleton = static_cast<const Ogre::Skeleton*>(handle);
    return skeleton->hasManualBones();
}

//typedef vector<ushort>::type BoneHandleMap;
//TODO: void _mergeSkeletonAnimations(const Skeleton* source, const BoneHandleMap& boneHandleMap, const StringVector& animations = StringVector());
//TODO: void _buildMapBoneByHandle(const Skeleton* source, BoneHandleMap& boneHandleMap) const;
//TODO: void _buildMapBoneByName(const Skeleton* source,BoneHandleMap& boneHandleMap) const;
