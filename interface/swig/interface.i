%module llcoi
%feature("export");
%feature("intern_function","1");
%insert("lisphead") %{
(in-package :ogre)

(define-foreign-library libllcoi
    (:unix (:or "libllcoi.so.3" "libllcoi.so"))
    (t (:default "libllcoi")))

(defun load-foreign-libraries ()
  (use-foreign-library libllcoi)
  (format t "~&[ogre] foreign library libllcoi loaded~%"))


(load-foreign-libraries)

(in-package :ogre)

%}
%include "../ogre_interface.h"
%include "../ois_interface.h"


