;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; ogre.asd --- ASDF system definition for Ogre.

(require 'cffi)

(in-package :cl-user)

(defpackage :ogre
    (:use :cl :asdf :cffi))

(in-package :ogre)

(defsystem "ogre"
    :description "an ogre wrapper"
    :version "0.1"
    :author "jacob moen"
    :license "MIT"
    :components ((:file "ogre"))
    :depends-on (:cffi))
