import sys
import numpy as np
import xarray as xr
import netCDF4 as nc
from scipy import ndimage
from utils import DataProcessor, DataRetriever, RRtrackRetriever
from matplotlib.pyplot import *



if __name__ == "__main__":
    inputConfig = sys.argv
    initTimeStep, endTimeStep = int(inputConfig[1]), int(inputConfig[2])
    caseName = "mjo"
    homePath = "/home/atmenu10246/"
    vvmPath = homePath + "transition/dat/{CN}/archive/".format(CN=caseName)
    cloudObjectPath = homePath + "transition/dat/collocateCloud/collocateCloudByQc/"
    finalTrackPath = homePath + "transition/src/mergeSplitIRT/"
    irctPath = homePath + "iterativeRainCellTracking/RRtrack/mjo-cwp-1e-1/"
    outputDir = homePath + "transition/src/"
    minPerTimeIdx = 10

    dataRetriever = DataRetriever(vvmPath, "mjo_std_mg")
    thData = dataRetriever.getThermoDynamic(0)
    xc, yc, zc = np.array(thData['xc']), np.array(thData['yc']), np.array(thData['zc'])

    finalTrack = np.loadtxt(finalTrackPath + "finalTrack_v2.txt").astype(int)
    trackList = np.arange(1, len(finalTrack)+1)
    rrTrackRetriever = RRtrackRetriever()
    trackOutputData = rrTrackRetriever.readTrackFile(irctPath +  "irt_tracks_output.txt")
    objectsOutputData = rrTrackRetriever.readObjectFile(irctPath +  "irt_objects_output.txt")


    timeIdxArange = np.arange(initTimeStep, endTimeStep, 1)

    for tIdx in timeIdxArange:
        print("========== {:06d} ==========".format(tIdx))
        cloudObject = np.array(nc.Dataset(cloudObjectPath+"cloudLabel-{:06d}.nc".format(tIdx))["cloudLabel"][0])
        cloudObject = np.max(cloudObject, axis=0)
        lwpTrackMetaData = trackOutputData[trackOutputData['timeStep'] == tIdx+1]
        lwpObjectMetaData = objectsOutputData[objectsOutputData['timeStep'] == tIdx]
        
        
        numColor = 23
        cmap = cm.get_cmap('rainbow', numColor)
        figure(figsize=(12, 12))
        pcolormesh(xc/1e3, yc/1e3, np.ma.masked_array(cloudObject%23, cloudObject<=0), cmap="rainbow", shading="nearest", vmin=0, vmax=numColor)
        scatter(lwpTrackMetaData["weightCenX"]*1e2/1e3, lwpTrackMetaData["weightCenY"]*1e2/1e3, 
                c=[finalTrack[trackList == int(x)][0]%numColor for x in lwpTrackMetaData["trackID"]], 
                cmap="rainbow", edgecolor="black")
        for _, i in lwpTrackMetaData.iterrows():
            text(i["weightCenX"]*1e2/1e3+0.5, i["weightCenY"]*1e2/1e3+0.5, s=finalTrack[trackList == int(i["trackID"])][0], color='black', fontsize=12)
        xlim(0, 102.4)
        ylim(0, 102.4)
        xlabel("XC", fontsize=20)
        ylabel("YC", fontsize=20)
        xticks(fontsize=20)
        yticks(fontsize=20)
        title("Time: {hr:04d} hr {minute:02d} min".format(
                hr = tIdx * minPerTimeIdx // 60, 
                minute = tIdx * minPerTimeIdx % 60),
                loc='right', fontsize=20)
        savefig(outputDir + "cloudCollocate-{:06d}.jpg".format(tIdx), dpi=300)
        clf()

