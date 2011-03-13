#! /bin/bash
csc -s -O2 -d0 ogre.scm chicken_wrap.c  -I "/usr/local/include/llcoi/ogre_interface.h" -I "/usr/local/include/llcoi/ois_interface.h" -L "/usr/local/lib/libllcoi.so"
