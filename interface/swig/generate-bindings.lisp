(in-package :cl-user)

(ql:quickload "cffi")
(ql:quickload "verrazano")

(in-package :verrazano-user)

(defun generate-binding* (name headers &rest args
                          &key (working-directory #p"/home/jacmoe/programming/llcoi/interface/")
                          (gccxml-flags "-I/usr/include")
                          &allow-other-keys)
  (format *debug-io* "~%~%; *** Processing binding ~S~%" name)
  (remove-from-plistf args :working-directory :gccxml-flags)
  (block try
    (handler-bind ((serious-condition
                    (lambda (error)
                      (warn "Failed to generated binding for ~S, error: ~A" name error)
                      (return-from try))))
      (let ((*print-right-margin* 100))
        (generate-binding (append
                           (list :cffi
                                 :package-name name
                                 :input-files headers
                                 :working-directory working-directory
                                 :gccxml-flags gccxml-flags)
                           args)
                          :keep-temporary-files nil))))
  (values))


(defun generate-llcoi-binding ()
  (generate-binding*
   :llcoi
   '("ogre_interface.h")
   :gccxml-node-types-to-output '(gccxml:typedef)
   #+unix :gccxml-flags #+unix "-I/home/jacmoe/programming/llcoi/interface/"))

