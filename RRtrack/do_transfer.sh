#!/bin/bash

caselist=(tpe20050712 tpe20050723 tpe20060508 tpe20060718 tpe20060721 tpe20070830 tpe20080715 tpe20090707 tpe20090817 tpe20090827 tpe20100629 tpe20100630 tpe20100802 tpe20100803 tpe20100912 tpe20110615 tpe20110616 tpe20110702 tpe20110723 tpe20110802 tpe20110816 tpe20110821 tpe20120715 tpe20120819 tpe20130723 tpe20130807 tpe20130825 tpe20140525 tpe20140703 tpe20140825)
total=${#caselist[*]}

for ((i=0;i<=$(($total-1));i++))
do
echo $i 
echo ${caselist[$i]}
   cd /data3/C.brobbtyh/CCN_TSA/RRtracking/

   # CHOOSE
#     # CLEAN
#     mkdir ${caselist[$i]}"cln"
#     cp transfer.f90 ./${caselist[$i]}"cln"
#     cd /data3/C.brobbtyh/AnthForcTSA/RRtracking/${caselist[$i]}"cln"
#     # DIRTY
#     mkdir ${caselist[$i]}"dty"
#     cp transfer.f90 ./${caselist[$i]}"dty" 
#     cd /data3/C.brobbtyh/AnthForcTSA/RRtracking/${caselist[$i]}"dty"
#     sed -i -e "s/cln/dty/g" transfer.f90
     # FUTURE
     mkdir ${caselist[$i]}"future"
     cp transfer.f90 ./${caselist[$i]}"future"
     cd /data3/C.brobbtyh/AnthForcTSA/RRtracking/${caselist[$i]}"future"
     sed -i -e "s/cln/dtyP3K/g" transfer.f90


   
   sed -i -e "s/e=1/e=$(($i+1))/g" transfer.f90

   ifort -free -mcmodel=large transfer.f90 -I/opt/vvmlibs/include -L/opt/vvmlibs/lib -lnetcdff -lnetcdf
   mv a.out transfer.exe
   ./transfer.exe
done

