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

## Building on Windows
###Build Ogredeps
```
mkdir Build
cd Build
cmake -G "MinGW Makefiles" -DCMAKE_CXX_FLAGS=-fpermissive -DOGREDEPS_BUILD_OIS=0 -DCMAKE_INSTALL_PREFIX=./Run/ ..
c:/mingw/bin/mingw32-make install

mkdir Build-Debug
cd Build-Debug
cmake -G "MinGW Makefiles" -DCMAKE_BUILD_TYPE=Debug -DCMAKE_CXX_FLAGS=-fpermissive -DOGREDEPS_BUILD_OIS=0 -DCMAKE_INSTALL_PREFIX=./Run/ ..
c:/mingw/bin/mingw32-make install
```
Copy Build/Run and Build-Debug/Run into the next Build/Run
### Build Ogre


```
mkdir Build
cd Build
cmake -G "MinGW Makefiles" -DOGRE_PLATFORM_X64=1 -DOGRE_BUILD_RENDERSYSTEM_D3D11=0 -DOGRE_BUILD_RENDERSYSTEM_D3D9=0 -DOGRE_BUILD_RENDERSYSTEM_GL3PLUS=1 -DOGRE_BUILD_RENDERSYSTEM_GL=1 -DOGRE_BUILD_RENDERSYSTEM_GL=0 -DOGRE_DEPENDENCIES_DIR=./Run/ -DOGRE_BUILD_SAMPLES=0 -DOGRE_UNITY_BUILD=1 -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=./Run/ ..
c:/mingw/bin/mingw32-make install
```

Copy ogre mingw files from Build/Run into the next Build/Run/
### Build llcoi
```
$ mkdir Build
$ cd Build
$ cmake -G "MinGW Makefiles" -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=./Run/ ..
$ c:/mingw/bin/mingw32-make install
```
## Building on the mac
### Ogredeps
```
mkdir Build || true
cd Build
cmake -G "Xcode" -DCMAKE_CXX_FLAGS="-mmacosx-version-min=10.7 -stdlib=libc++ -std=c++11" -DOGREDEPS_BUILD_OIS=0 -DCMAKE_INSTALL_PREFIX=Run/ ..
xcodebuild -project Ogredeps.xcodeproj -configuration Release -target install
```
### Ogre
```
mkdir Build || true
cd Build
cmake -G "Xcode" -DOGRE_UNITY_BUILD=1 -DCMAKE_CXX_FLAGS="${CMAKE_CXX_FLAGS} -v -stdlib=libc++ -std=c++11" -DCMAKE_INSTALL_PREFIX=Run/ ..
xcodebuild -project Ogre.xcodeproj -configuration Release -target install
```
### llcoi
```
mkdir Build || true
cd Build
cmake -G "Xcode" -DCMAKE_CXX_FLAGS="${CMAKE_CXX_FLAGS} -v -stdlib=libc++ -std=c++11" -DCMAKE_INSTALL_PREFIX=Run/ ..
xcodebuild -project llcoi.xcodeproj -configuration Release -target install
```
