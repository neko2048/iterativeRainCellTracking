#!/bin/bash

# mkdir data
caselist=(tpe20050712 tpe20050723 tpe20060508 tpe20060718 tpe20060721 tpe20070830 tpe20080715 tpe20090707 tpe20090817 tpe20090827 tpe20100629 tpe20100630 tpe20100802 tpe20100803 tpe20100912 tpe20110615 tpe20110616 tpe20110702 tpe20110723 tpe20110802 tpe20110816 tpe20110821 tpe20120715 tpe20120819 tpe20130723 tpe20130807 tpe20130825 tpe20140525 tpe20140703 tpe20140825)
total=${#caselist[*]}

#EXPT=cln
#EXPT=dty
EXPT=future

for ((i=0;i<=$(($total-1));i++))
do
echo $i 
echo ${caselist[$i]}

   ln -s /data3/C.brobbtyh/AnthForcTSA/RRtracking/${caselist[$i]}${EXPT}/irt_tracks_mask_${caselist[$i]}_${EXPT}.nc ./data
   ln -s /data3/C.brobbtyh/AnthForcTSA/RRtracking/${caselist[$i]}${EXPT}/irt_tracklinks_${caselist[$i]}_${EXPT}.txt ./data

done

