(require 'ogre)

(defparameter *windowhandle* nil)
(defparameter *camerahandle* nil)
(defparameter *viewporthandle* nil)
(defparameter *windowhwnd* nil)
(defparameter *entityhandle* nil)
(defparameter *nodehandle* nil)
(defparameter *lighthandle* nil)
(defparameter *mouse* nil)
(defparameter *keyboard* nil)
(defparameter *timer* 0)


(ogre:create-root "plugins.cfg" "ogre.cfg" "ogre.log")

(ogre:restore-config)

(ogre:setup-resources "resources.cfg")

(setq *windowhandle* (ogre:root-initialise 1 "therenderwindow"))

(ogre:set-default-num-mipmaps 5)

(ogre:initialise-all-resourcegroups)

(ogre:create-scene-manager "OctreeSceneManager" "scenemanager")

(setq *camerahandle* (ogre:create-camera "myCam"))

(ogre:camera-set-position *camerahandle* 0.0 0.0 500.0)

(ogre:camera-lookat *camerahandle* 0.0 0.0 -300.0)

(ogre:camera-set-near-clip-distance *camerahandle* 5.0)

(setq *viewporthandle* (ogre:add-viewport *camerahandle*))

(ogre:viewport-set-background-colour *viewporthandle* 0.0 0.0 0.0)

(ogre:camera-set-aspect-ratio *camerahandle* 800.0 600.0)

(setq *entityhandle* (ogre:create-entity "OgreHead" "ogrehead.mesh"))

(setq *nodehandle* (ogre:create-child-scenenode "headnode"))

(ogre:attach-entity-to-scenenode *entityhandle* *nodehandle*)

(ogre:set-ambient-light-rgb 0.5 0.5 0.5)

(setq *lighthandle* (ogre:create-light "mainLight"))

(ogre:light-set-position *lighthandle* 20.0 80.0 50.0)

(ogre:log-message "Hello World from Lisp")

(setq *windowhwnd* (ogre:render-window-get-hwnd *windowhandle*))

(ogre:create-input-system *windowhwnd*);

(setq *keyboard* (ogre:create-keyboard-object 0))


(cffi:defcallback framequeued :int
    ((evt_time :float)
    (frame_time :float)
    (event_type :int))
        (setq *timer* (+ *timer* frame_time))
        (let ((x (* (cos *timer*) 100.0))
            (y 50.0)
            (z (* (sin *timer*) 100.0)))
                (ogre:camera-set-position *camerahandle* x y z)
                (ogre::camera-lookat *camerahandle* 0.0 0.0 0.0))
    (if (= (ogre:keyboard-is-key-down *keyboard* :+KC-ESCAPE+) 1) 0 1)
)

(ogre:add-frame-listener (cffi:callback framequeued) 2)

;;(ogre:render-loop)

(loop
    (if (= (ogre:keyboard-is-key-down *keyboard* :+KC-ESCAPE+) 1) (return))
    (ogre:keyboard-capture *keyboard*)
    (ogre:pump-messages)
    (ogre:render-one-frame)
)

(ogre:release-engine)

