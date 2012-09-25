#!/bin/bash

# uses g++ to compile llcoi against Ogre and OIS
# you might need to link against other DLLs if you want to
# use llcoi with it's other (optional) dependencies

# in this example I have the ogre 1.8 sources, and built them
# via CMake to an 'ogrebuild' directory. OIS is built from 
# the ogredeps project: https://bitbucket.org/cabalistic/ogredeps

# where the ogre binaries are, i.e. libOgreMain.so
ogrelibdir=~/downloads/ogrebuild/lib
# where the ogre header files are, i.e. OgreRoot.h
ogrehdrdir=~/downloads/ogre_src_v1-8-0/OgreMain/include
# where OgreBuildSettings.h is located (created by Ogre cmake)
ogrebstdir=~/downloads/ogrebuild/include

# where the OIS binaries are, i.e. libOIS.so
oislibdir=~/downloads/ogredeps/build/ogredeps/lib
# where the OIS header files are, i.e. OIS.h
oishdrdir=~/downloads/ogredeps/build/ogredeps/include/OIS

# compile llcoi to shared object
# outputs libllcoi.so in this same folder
# use -ansi -pedantic to check c90 compatibility
g++ -g -O2 -fPIC -m64 -I$ogrehdrdir -I$ogrebstdir -I$oishdrdir -I./interface -L$ogrelibdir -L$oislibdir -lOgreMain -lOIS -shared -o libllcoi.so interface/*.cpp #-ansi -pedantic
