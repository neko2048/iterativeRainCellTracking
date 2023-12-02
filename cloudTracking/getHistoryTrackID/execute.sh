#!/bin/bash
ifort -free -heap-arrays 1024 -shared-intel -mcmodel=large getHistoryTrackID_v2.F -I/opt/vvmlibs/include -L/opt/vvmlibs/lib -lnetcdff -lnetcdf
mv a.out getHistoryTrackID.exe

#echo "Press Enter to Execute"
#read var
#if [[ $var = "" ]]; then
#    ./getHistoryTrackID.exe
#fi
