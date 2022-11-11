#!/bin/bash

caselist=(mjo)
total=${#caselist[*]}

for ((i=0;i<=$(($total-1));i++))
do
echo $i 
echo ${caselist[$i]}
   #cd /data3/C.brobbtyh/CCN_TSA/RRtracking/
   cd /home/atmenu10246/iterativeRainCellTracking/RRtrack/
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
#     # FUTURE
#     mkdir ${caselist[$i]}"future"
#     cp transfer.f90 ./${caselist[$i]}"future"
#     cd /data3/C.brobbtyh/AnthForcTSA/RRtracking/${caselist[$i]}"future"
#     sed -i -e "s/cln/dtyP3K/g" transfer.f90
    mkdir ./mjo
    cp transfer.f90 ./mjo
    cd /home/atmenu10246/iterativeRainCellTracking/RRtrack/mjo
#    sed -i -e "" transfer.f90

#   change cases   
#   sed -i -e "s/e=1/e=$(($i+1))/g" transfer.f90

   ifort -free -mcmodel=large transfer.f90 -I/opt/vvmlibs/include -L/opt/vvmlibs/lib -lnetcdff -lnetcdf
   mv a.out transfer.exe
   ./transfer.exe
done

