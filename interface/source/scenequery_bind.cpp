/*     __ __              _ 
 *    / // /_____ ____   (_)
 *   / // // ___// __ \ / / 
 *  / // // /__ / /_/ // /  
 * /_//_/ \___/ \____//_/   
 * https://bitbucket.org/galaktor/llcoi 
 * copyright (c) 2011, llcoi Team
 * MIT license applies - see file "LICENSE" for details.
*/
#include "binding_utils.h" // llcoi_wft_to_ogre_wft
#include "scenequery_bind.h"
#include <OgreSceneQuery.h>
#include <OgreSceneManager.h>



// internal helper method
void ogre_wf_to_llcoi_wf(const Ogre::SceneQuery::WorldFragment& o, world_fragment & l)
{
    l.fragment_type = ogre_wft_to_llcoi_wft(o.fragmentType);

    l.single_intersection.x = o.singleIntersection.x;
    l.single_intersection.y = o.singleIntersection.y;
    l.single_intersection.z = o.singleIntersection.z;

    l.planes = reinterpret_cast<PlaneListHandle>(o.planes);

    l.geometry = o.geometry;
    l.render_op = reinterpret_cast<RenderOperationHandle>(o.renderOp);
}


// SceneQuery::setQueryMask(uint32 mask)
void scenequery_set_query_mask(SceneQueryHandle handle, uint32 mask)
{
    Ogre::SceneQuery* query = reinterpret_cast<Ogre::SceneQuery*>(handle);
    query->setQueryMask(mask);
}

//uint32 SceneQuery::getQueryMask(void) const
uint32 scenequery_get_query_mask(SceneQueryHandle handle)
{
    Ogre::SceneQuery* query = reinterpret_cast<Ogre::SceneQuery*>(handle);
    return query->getQueryMask();
}

//void setWorldFragmentType(enum WorldFragmentType wft);
void scenequery_set_world_fragment_type(SceneQueryHandle handle, world_fragment_type wft)
{
    Ogre::SceneQuery* query = reinterpret_cast<Ogre::SceneQuery*>(handle);
    Ogre::SceneQuery::WorldFragmentType WFT = llcoi_wft_to_ogre_wft(wft);
    query->setWorldFragmentType(WFT);
}

//WorldFragmentType SceneQuery::getWorldFragmentType(void) const;
world_fragment_type scenequery_get_world_fragment_type(SceneQueryHandle handle)
{
    Ogre::SceneQuery* query = reinterpret_cast<Ogre::SceneQuery*>(handle);
    Ogre::SceneQuery::WorldFragmentType wft = query->getWorldFragmentType();
    return ogre_wft_to_llcoi_wft(wft);
}





class RaySceneQueryListenerCTX : public Ogre::RaySceneQueryListener
{
public:
    RaySceneQueryListenerCTX(RaySceneQueryFragmentResult fragment_callback,
                             RaySceneQueryObjectResult object_callback,
                             void* data) : FragmentResult(fragment_callback), ObjectResult(object_callback), userdata(data)
    {
    }

    bool queryResult(Ogre::MovableObject* obj, Ogre::Real distance)
    {

        int result;
        MovableObjectHandle handle = reinterpret_cast<MovableObjectHandle>(obj);
        result = ObjectResult(handle, distance, userdata);
        return result;
    }

    bool queryResult(Ogre::SceneQuery::WorldFragment* fragment, Ogre::Real distance)
    {
        int result;
        world_fragment wf;
        ogre_wf_to_llcoi_wf(*fragment, wf);
        result = ObjectResult(&wf, distance, userdata);
        return result;
    }

    RaySceneQueryFragmentResult FragmentResult;
    RaySceneQueryObjectResult ObjectResult;
    void* userdata;
};

class SceneQueryListenerCTX : public Ogre::SceneQueryListener
{
public:
    SceneQueryListenerCTX(SceneQueryFragmentResult fragment_callback,
                          SceneQueryObjectResult object_callback,
                          void* data) : FragmentResult(fragment_callback), ObjectResult(object_callback), userdata(data)
    {
    }

    bool queryResult(Ogre::MovableObject* obj)
    {
        int result;
        MovableObjectHandle handle = reinterpret_cast<MovableObjectHandle>(obj);
        result = ObjectResult(handle, userdata);
        return result;
    }

    bool queryResult(Ogre::SceneQuery::WorldFragment* fragment)
    {
        int result;
        world_fragment wf;

        wf.fragment_type = ogre_wft_to_llcoi_wft(fragment->fragmentType);

        wf.single_intersection.x = fragment->singleIntersection.x;
        wf.single_intersection.y = fragment->singleIntersection.y;
        wf.single_intersection.z = fragment->singleIntersection.z;

        wf.planes = reinterpret_cast<PlaneListHandle>(fragment->planes);

        wf.geometry = fragment->geometry;
        wf.render_op = reinterpret_cast<RenderOperationHandle>(fragment->renderOp);

        result = FragmentResult(&wf, userdata);
        return result;
    }

    virtual ~SceneQueryListenerCTX() {}

    SceneQueryFragmentResult FragmentResult;
    SceneQueryObjectResult ObjectResult;
    void* userdata;
};

SceneQueryListenerHandle create_scenequerylistener(SceneQueryFragmentResult fragment_callback, SceneQueryObjectResult object_callback, void* userdata)
{
    SceneQueryListenerCTX* listener = new SceneQueryListenerCTX(fragment_callback, object_callback, userdata);
    return reinterpret_cast<SceneQueryListenerHandle>(listener);
    
}

void destroy_scenequerylistener(SceneQueryListenerHandle handle)
{
    SceneQueryListenerCTX* listener = reinterpret_cast<SceneQueryListenerCTX*>(handle);
    delete listener;
}


RaySceneQueryListenerHandle create_rayscenequerylistener(RaySceneQueryFragmentResult fragment_callback, RaySceneQueryObjectResult object_callback, void* userdata)
{
    RaySceneQueryListenerCTX* listener = new RaySceneQueryListenerCTX(fragment_callback, object_callback, userdata);
    return reinterpret_cast<RaySceneQueryListenerHandle>(listener);
}

void destroy_rayscenequerylistener(RaySceneQueryListenerHandle handle)
{
    RaySceneQueryListenerCTX* listener = reinterpret_cast<RaySceneQueryListenerCTX*>(handle);
    delete listener;
}


size_t scenequeryresult_movables_count(SceneQueryResultHandle handle)
{
    Ogre::SceneQueryResult* result = reinterpret_cast<Ogre::SceneQueryResult*>(handle);
    return result->movables.size();
}

MovableObjectHandle scenequeryresult_movables_at(SceneQueryResultHandle handle, int index)
{
    int cnt = 0;
    Ogre::SceneQueryResult* result = reinterpret_cast<Ogre::SceneQueryResult*>(handle);
    Ogre::SceneQueryResultMovableList::iterator it = result->movables.begin();

    for (;it != result->movables.end(); ++it)
    {
        if (cnt == index)
            return reinterpret_cast<MovableObjectHandle>(*it);
        cnt++;
    }
}

size_t scenequeryresult_worldfragments_count(SceneQueryResultHandle handle, int index)
{
    Ogre::SceneQueryResult* result = reinterpret_cast<Ogre::SceneQueryResult*>(handle);
    return result->worldFragments.size();
}

void scenequeryresult_worldfragments_at(SceneQueryResultHandle handle, int index, world_fragment* result)
{
    int cnt = 0;
    Ogre::SceneQueryResult* sqr = reinterpret_cast<Ogre::SceneQueryResult*>(handle);
    Ogre::SceneQueryResultWorldFragmentList::iterator it = sqr->worldFragments.begin();

    for (;it != sqr->worldFragments.end(); ++it)
    {
        if(cnt == index)
        {
            result->fragment_type = ogre_wft_to_llcoi_wft((*it)->fragmentType);

            result->single_intersection.x = (*it)->singleIntersection.x;
            result->single_intersection.y = (*it)->singleIntersection.y;
            result->single_intersection.z = (*it)->singleIntersection.z;

            result->planes = reinterpret_cast<PlaneListHandle>((*it)->planes);

            result->geometry = (*it)->geometry;
            result->render_op = reinterpret_cast<RenderOperationHandle>((*it)->renderOp);
            break;
        }
        cnt++;
    }
}



void rayscenequery_set_ray(RaySceneQueryHandle handle, RayHandle ray_handle)
{
    Ogre::RaySceneQuery* query = reinterpret_cast<Ogre::RaySceneQuery*>(handle);
    Ogre::Ray* ray = reinterpret_cast<Ogre::Ray*>(ray_handle);
    query->setRay(*ray);
}

RayHandle rayscenequery_get_ray(RaySceneQueryHandle handle)
{
    Ogre::RaySceneQuery* query = reinterpret_cast<Ogre::RaySceneQuery*>(handle);
    Ogre::Ray& ray = const_cast<Ogre::Ray&>(
        query->getRay()
    );

    return reinterpret_cast<RayHandle>(&ray);
}


//void setSortByDistance(bool sort, ushort maxresults = 0);
void rayscenequery_set_sort_by_distance(RaySceneQueryHandle handle, int on, unsigned short maxresults)
{
    Ogre::RaySceneQuery* query = reinterpret_cast<Ogre::RaySceneQuery*>(handle);
    query->setSortByDistance(on, maxresults);
}

//bool getSortByDistance(void) const;
int rayscenequery_get_sort_by_distance(RaySceneQueryHandle handle)
{
    Ogre::RaySceneQuery* query = reinterpret_cast<Ogre::RaySceneQuery*>(handle);
    return query->getSortByDistance();
}

//ushort getMaxResults(void) const;
unsigned short rayscenequery_get_max_results(RaySceneQueryHandle handle)
{
    Ogre::RaySceneQuery* query = reinterpret_cast<Ogre::RaySceneQuery*>(handle);
    return query->getMaxResults();
}


size_t rayscenequeryresult_count(RaySceneQueryResultHandle handle)
{
    Ogre::RaySceneQueryResult* result = reinterpret_cast<Ogre::RaySceneQueryResult*>(handle);
    return result->size();
}

void rayscenequeryresult_at(RaySceneQueryResultHandle handle, int index, rayscenequery_result_entry* result)
{
    Ogre::RaySceneQueryResult* query = reinterpret_cast<Ogre::RaySceneQueryResult*>(handle);
    Ogre::RaySceneQueryResultEntry entry = query->at(index);

    result->distance = entry.distance;
    result->movable  = reinterpret_cast<MovableObjectHandle>(entry.movable);
    ogre_wf_to_llcoi_wf(*entry.worldFragment, *result->fragment);
}
