%module llcoi
%feature("intern_function","1");
%feature("export");

%{
    #include "../ogre_interface.h"
    #include "../ois_interface.h"
%}

%include "../ogre_interface.h"
%include "../ois_interface.h"


