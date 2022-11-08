#!/bin/bash
ifort -free -heap-arrays 1024 -shared-intel -mcmodel=large cloud_object_v2.F -I/opt/vvmlibs/include -L/opt/vvmlibs/lib -lnetcdff -lnetcdf
mv a.out cloud_object_v2.exe
