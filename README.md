```
    __ __              _ 
   / // /_____ ____   (_)
  / // // ___// __ \ / / 
 / // // /__ / /_/ // /  
/_//_/ \___/ \____//_/   
```
https://bitbucket.org/galaktor/llcoi 

# license
MIT license applies - see file "LICENSE" for details. Copyright (c) 2011, llcoi Team

# TODOs
* fix CMake; consider simplifying the build altogether (compare to simplebuild.sh)
* make typedefs consistent (C-style): typedef enum {a,b,c} MyEnum;
* re-add lightweight file headers with license and copyright reference
* remove comment boilerplate
* update/cleanup sample app (maybe take the gogre3d demo app for easier side-by-side dev/comparing)
* organize bindings better
 * remove "_bind" from the file names
 * several smaller fwd.h files?

# Building 

Copy ogre mingw files into Build/Run/

```
$ mkdir Build
$ cd Build
$ cmake -G "MinGW Makefiles" -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=./Run/ ..
$ c:/mingw/bin/mingw32-make install
```
