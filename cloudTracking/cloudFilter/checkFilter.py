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
    cloudObjectPath = homePath + "transition/dat/cloudLabel/filter/"
    #cloudObjectPath = homePath + "transition/dat/cloudLabel/test/"
    #cloudObjectPath = homePath + "transition/dat/cloudLabel/origin/"
    irctNCPath = homePath + "iterativeRainCellTracking/RRtrack/mjo-cwp-1e-1/"
    minPerTimeIdx = 10

    dataRetriever = DataRetriever(vvmPath, "mjo_std_mg")
    thData = dataRetriever.getThermoDynamic(0)
    xc, yc, zc = np.array(thData['xc']), np.array(thData['yc']), np.array(thData['zc'])
    lwpTrackData = nc.Dataset(irctNCPath + "irt_objects_mask_mjo_.nc")["var1"]

    #tIdxArange = np.arange(initTimeStep, endTimeStep, 1)
    tIdxArange = np.arange(430, 450, 1)

    for tIdx in tIdxArange:
        print("========== {:06d} ==========".format(tIdx))
        cloudObject = np.array(nc.Dataset(cloudObjectPath+"cloudLabel-{:06d}.nc".format(tIdx-1))["cloudLabel"][0])

        cloudProjection = np.max(cloudObject, axis=0)
        cloudProjection = np.ma.masked_array(cloudProjection, cloudProjection<=0)
        cloudProjection = cloudProjection % 23 # Easy way to mess index continuity.

        figure(figsize=(12, 12), dpi=200)
        pcolormesh(xc/1e3, yc/1e3, cloudProjection, cmap='rainbow', vmin=0, vmax=23, shading="nearest")
        #pcolormesh(xc/1e3, yc/1e3, np.ma.masked_array(lwpTrackData[tIdx, 0, :, :]>0, lwpTrackData[tIdx, 0, :, :]<=0), cmap='Reds', vmin=0, vmax=2)
        title("Time: {:04d} hr {:02d} min".format(tIdx*minPerTimeIdx//60, tIdx*minPerTimeIdx%60), fontsize=20, loc="right")
        xlim(0, 102.4)
        ylim(0, 102.4)
        xticks(fontsize=20)
        yticks(fontsize=20)
        xlabel("X [km]", fontsize=20)
        ylabel("Y [km]", fontsize=20)
        savefig("checkFilterFilter-{:06d}.jpg".format(tIdx), dpi=300)
        clf()
