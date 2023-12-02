#!/bin/bash
fileName=cloudObjectFilter_2Layer
ifort -free -heap-arrays 1024 -shared-intel -mcmodel=large $fileName.F -I/opt/vvmlibs/include -L/opt/vvmlibs/lib -lnetcdff -lnetcdf
mv a.out $fileName.exe

#echo "Press Enter to Execute"
#read var
#if [[ $var = "" ]]; then
#    ./$fileName.exe
#fi
