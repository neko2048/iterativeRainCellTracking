import sys
import numpy as np
import netCDF4 as nc
from utils import DataProcessor, DataRetriever
from matplotlib.pyplot import *

if __name__ == "__main__":
    #inputConfig = sys.argv
    #initTimeStep, endTimeStep = int(inputConfig[1]), int(inputConfig[2])
    caseName = "mjo"
    homePath = "/home/atmenu10246/"
    vvmPath = homePath + "transition/dat/{CN}/archive/".format(CN=caseName)
    #cloudObjectPath = homePath + "transition/dat/cloudLabel/filter/"
    #cloudObjectPath = homePath + "transition/dat/cloudLabel/test/"
    cloudObjectPath = homePath + "transition/dat/cloudLabel/origin/"
    irctNCPath = homePath + "iterativeRainCellTracking/RRtrack/mjo-cwp-1e-1/"
    minPerTimeIdx = 10

    dataRetriever = DataRetriever(vvmPath, "mjo_std_mg")
    thData = dataRetriever.getThermoDynamic(0)
    xc, yc, zc = np.array(thData['xc']), np.array(thData['yc']), np.array(thData['zc'])
    lwpTrackData = nc.Dataset(irctNCPath + "irt_objects_mask_mjo_.nc")["var1"]

    #tIdxArange = np.arange(initTimeStep, endTimeStep, 1)
    tIdxArange = np.arange(410, 415, 1)

    for tIdx in tIdxArange:
        print("========== {:06d} ==========".format(tIdx))
        cloudObject = np.array(nc.Dataset(cloudObjectPath+"cloudLabel-{:06d}.nc".format(tIdx-1))["cloudLabel"][0])

        randomCloudObject = cloudObject % 93 + 1
        randomCloudObject[cloudObject==0] = -1
        figure(figsize=(15, 12), dpi=200)
        #pcolormesh(xc/1e3, yc/1e3, np.ma.masked_array(lwpTrackData[tIdx, 0, :, :], lwpTrackData[tIdx, 0, :, :]<=0), cmap="jet")
        #colorbar()
        #for i in np.unique(cloudObject)[1:]:
        #    print(i, np.max(cloudObject))
        #    contour(xc/1e3, yc/1e3, np.sum(cloudObject==i, axis=0).astype(np.bool), color='black', linewidths=1)
        pcolormesh(xc/1e3, yc/1e3, np.ma.masked_array(np.max(randomCloudObject, axis=0), np.max(randomCloudObject, axis=0)==-1), cmap='jet')
        title("Time: {:04d} hr {:02d} min".format(tIdx*minPerTimeIdx//60, tIdx*minPerTimeIdx%60), fontsize=20, loc="right")
        xlim(0, 102.4)
        ylim(0, 102.4)
        xticks(fontsize=20)
        yticks(fontsize=20)
        xlabel("X [km]", fontsize=20)
        ylabel("Y [km]", fontsize=20)
        savefig("checkReIndex-{:06d}.jpg".format(tIdx), dpi=300)
        clf()
