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
    qcObjectPath = homePath + "transition/dat/collocateCloud/qcCollocate/"
    cloudObjectPath = homePath + "transition/dat/cloudLabel/filter/"
    irctPath = homePath + "iterativeRainCellTracking/RRtrack/mjo-cwp-1e-1/"
    reIdxLWP = homePath + "transition/dat/reindexLWP/"
    vvmPath = homePath + "transition/dat/{CN}/archive/".format(CN=caseName)
    dataRetriever = DataRetriever(vvmPath, "mjo_std_mg")
    thData = dataRetriever.getThermoDynamic(0)
    xc, yc, zc = np.array(thData['xc']), np.array(thData['yc']), np.array(thData['zc'])
    historyTrackID = np.load("historyTrackArr.npy")
    finalTrack = np.load("finalTrack.npy")

    rrTrackRetriever = RRtrackRetriever()
    trackOutputData = rrTrackRetriever.readTrackFile(irctPath +  "irt_tracks_output.txt")
    
    timeIdxArange = np.arange(initTimeStep, endTimeStep, 1)
    minPerTimeIdx = 10
    
    for tIdx in timeIdxArange:
        print("========== {:06d} ==========".format(tIdx))
        qcObject = np.array(nc.Dataset(qcObjectPath + "cloudLabel-{:06d}.nc".format(tIdx))["cloudLabel"][0])
        cloudObject = np.array(nc.Dataset(cloudObjectPath + "cloudLabel-{:06d}.nc".format(tIdx))["cloudLabel"][0])
        lwpTrackField = np.array(nc.Dataset(reIdxLWP+"lwp-{:06d}.nc".format(tIdx+1))["lwp"][0])
        qcProject = qcObject#[0:np.argmin(np.abs(zc - 10000)), :, :]#np.max(cloudObject, axis=0)
        qcProject = np.max(qcProject, axis=0)
        qcProject = np.where(qcProject<=0, qcProject, finalTrack[qcProject-1])

        lwpTrackMetaData = trackOutputData[trackOutputData['timeStep'] == tIdx+1]

        figure(figsize=(12, 12))
        #pcolormesh(xc/1e3, yc/1e3, np.ma.masked_array(cloudLevels, cloudLevels<=0), 
        #           shading="nearest", cmap="rainbow", vmin=1, vmax=5)
        pcolormesh(xc/1e3, yc/1e3, np.ma.masked_array(qcProject%23, qcProject<=0), cmap='rainbow', vmin=0, vmax=23, shading="nearest")
        scatter(lwpTrackMetaData["weightCenX"]*1e2/1e3, lwpTrackMetaData["weightCenY"]*1e2/1e3, c="black", cmap="rainbow", edgecolor="black", s=5)
        for _, i in lwpTrackMetaData.iterrows():
            text(i["weightCenX"]*1e2/1e3, i["weightCenY"]*1e2/1e3, s=finalTrack[int(i["trackID"])-1], color='black', fontsize=10)
        contour(xc/1e3, yc/1e3, cloudObject.any(axis=0)>0, colors='blue', linewidths=0.1)
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
