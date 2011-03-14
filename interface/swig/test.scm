(load "ogre.so")

(define *windowhandle* #f)
(define *camerahandle* #f)
(define *viewporthandle* #f)
(define *windowhwnd* #f)
(define *entityhandle* #f)
(define *nodehandle* #f)
(define *lighthandle* #f)
(define *mouse* #f)
(define *keyboard* #f)
(define *timer* #f)
(define *use-mouse* #f)


(create-root "plugins.cfg" "ogre.cfg" "ogre.log")

(restore-config)

(setup-resources "resources.cfg")

(set! *windowhandle* (root-initialise 1 "therenderwindow"))

(set-default-num-mipmaps 5)

(initialise-all-resourcegroups)

(create-scene-manager "OctreeSceneManager" "scenemanager")

(set! *camerahandle* (create-camera "myCam"))

(camera-set-position *camerahandle* 0.0 0.0 500.0)

(camera-lookat *camerahandle* 0.0 0.0 -300.0)

;;(camera-set-near-clip-distance *camerahandle* 5.0)

(set! *viewporthandle* (add-viewport *camerahandle*))

(viewport-set-background-colour *viewporthandle* 0.0 0.0 0.0)

(camera-set-aspect-ratio *camerahandle* 800.0 600.0)

;;(set! *entityhandle* (create-entity "OgreHead" "ogrehead.mesh"))

;;(set! *nodehandle* (create-child-scenenode "headnode"))

;;(attach-entity-to-scenenode *entityhandle* *nodehandle*)

(set-ambient-light-rgb 0.5 0.5 0.5)

(set! *lighthandle* (create-light "mainLight"))

(light-set-position *lighthandle* 20.0 80.0 50.0)

(log-message "Hello World from Scheme")

(release-engine)

