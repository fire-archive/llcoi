(load "ogre.so")

(define *windowhandle* 0)
(define *camerahandle* 0)
(define *viewporthandle* 0)
(define *windowhwnd* 0)
(define *entityhandle* 0)
(define *nodehandle* 0)
(define *lighthandle* 0)
(define *mouse* 0)
(define *keyboard* 0)
(define *timer* 0)
(define *use-mouse* 0)


(create-root "plugins.cfg" "ogre.cfg" "ogre.log")

(restore-config)

(setup-resources "resources.cfg")

(set! *windowhandle* (root-initialise 1 "therenderwindow"))
;;(root-initialise 1 "therenderwindow")

(release-engine)

