D2 (http://dlang.org/) interface files for LLCOI. Tested with dmd 2.059 and dmd 2.060HEAD - thus far, only tested on Linux 32bit.

Compile:
dmd -I./import main.d

Copy to test dir:
cp main ../../test/dist/bin/ && cd ../../test/dist/bin/

Run:
./main


Many thanks to the LLCOI team for providing this awesome C interface. I hereby release this code under the same license as the main LLCOI project. - Jeremy "voyvf" Sandell
