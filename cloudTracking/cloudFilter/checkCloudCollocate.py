import sys
import numpy as np
import netCDF4 as nc
from matplotlib.pyplot import *
from utils import DataProcessor, DataRetriever, RRtrackRetriever

if __name__ == "__main__":
    inputConfig = sys.argv
    initTimeStep, endTimeStep = int(inputConfig[1]), int(inputConfig[2])
    
    lastTrackRecordTimeStep = 627
    caseName = "mjo"
    homePath = "/home/atmenu10246/"
    lastTrackRecordPath = homePath + "dat/indexMap/trackRecorder-{:06d}.npy".format(lastTrackRecordTimeStep)
    cloudObjectpath = homePath + "transition/dat/collocateCloud/ver2-1000/"
    reIdxLWP = homePath + "transition/dat/reindexLWP/"
    vvmPath = homePath + "transition/dat/{CN}/archive/".format(CN=caseName)
    dataRetriever = DataRetriever(vvmPath, "mjo_std_mg")
    thData = dataRetriever.getThermoDynamic(0)
    xc, yc, zc = np.array(thData['xc']), np.array(thData['yc']), np.array(thData['zc'])
    
    #timeIdxArange = np.arange(400, 420)

    timeIdxArange = np.arange(initTimeStep, endTimeStep, 1)
    minPerTimeIdx = 10

    for tIdx in timeIdxArange:
        print("========== {:06d} ==========".format(tIdx))
        cloudObject = np.array(nc.Dataset(cloudObjectpath + "cloudLabel-{:06d}.nc".format(tIdx))["cloudLabel"][0])
        thData = dataRetriever.getThermoDynamic(tIdx)

        lwpTrackField = np.array(nc.Dataset(reIdxLWP+"lwp-{:06d}.nc".format(tIdx))["lwp"][0])
        cloudProject = cloudObject[0:np.argmin(np.abs(zc - 10000)), :, :]#np.max(cloudObject, axis=0)
        cloudProject = np.max(cloudProject, axis=0)
        #cloudLevels = np.zeros(shape=cloudProject.shape)
        #for i in np.unique(cloudProject)[1:]:
        #    cloudLevels += (cloudObject == i).any(axis=(0))
        #cloudProject = np.ma.masked_array(cloudProject%23, cloudProject<=0)
        figure(figsize=(12, 12))
        #pcolormesh(xc/1e3, yc/1e3, np.ma.masked_array(cloudLevels, cloudLevels<=0), 
        #           shading="nearest", cmap="rainbow", vmin=1, vmax=5)
        pcolormesh(xc/1e3, yc/1e3, np.ma.masked_array(cloudProject%23, cloudProject<=0), cmap='rainbow', vmin=0, vmax=23, shading="nearest")
        #colorbar()
        contour(xc/1e3, yc/1e3, lwpTrackField>0, colors='black', linewidths=0.1)
        xlim(0, 102.4)
        ylim(0, 102.4)
        xlabel("XC [km]", fontsize=20)
        ylabel("YC [km]", fontsize=20)
        xticks(fontsize=20)
        yticks(fontsize=20)
        title("Time: {hr:04d} hr {minute:02d} min".format(
                hr = tIdx * minPerTimeIdx // 60, 
                minute = tIdx * minPerTimeIdx % 60),
                loc='right', fontsize=20)
        savefig("cloudCollocate-{:06d}.jpg".format(tIdx), dpi=300)
        clf()
