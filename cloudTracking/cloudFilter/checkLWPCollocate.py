import sys
import numpy as np
import xarray as xr
import netCDF4 as nc
from scipy import ndimage
from utils import DataProcessor, DataRetriever, RRtrackRetriever
from matplotlib.pyplot import *

class Collocator:
    def __init__(self, deltaX, deltaY):
        self.deltaX = deltaX
        self.deltaY = deltaY

    def lwpCollocate(self, lwpTrackField, lwpTrackMetaData, findGridSize=2):
        """
        INPUT: 
            lwpTrackField: (y, x)
            lwpTrackMetaData: pandas DataFrame
        OUTPUT:
            cloudObject: (z, y, x)
        """
        mappedTrackField = np.copy(lwpTrackField)
        for _, eachObject in lwpTrackMetaData.iterrows():
            #print("{} -> {}".format(eachObject["objectID1"], eachObject["trackID"]))
            mappedTrackField[lwpTrackField==eachObject["objectID1"]] = eachObject["trackID"]
        return mappedTrackField


if __name__ == "__main__":
    inputConfig = sys.argv
    initTimeStep, endTimeStep = int(inputConfig[1]), int(inputConfig[2])
    caseName = "mjo"
    homePath = "/home/atmenu10246/"
    vvmPath = homePath + "transition/dat/{CN}/archive/".format(CN=caseName)
    cloudObjectPath = homePath + "transition/dat/cloudLabel/filter/"
    irctPath = homePath + "iterativeRainCellTracking/RRtrack/mjo-cwp-1e-1/"
    outputDir = homePath + "transition/dat/reindexLWP/"
    minPerTimeIdx = 10

    dataRetriever = DataRetriever(vvmPath, "mjo_std_mg")
    thData = dataRetriever.getThermoDynamic(0)
    xc, yc, zc = np.array(thData['xc']), np.array(thData['yc']), np.array(thData['zc'])
    deltaX, deltaY = xc[1] - xc[0], yc[1] - yc[0]

    lwpTrackData = nc.Dataset(irctPath + "irt_objects_mask_mjo_.nc")["var1"]

    rrTrackRetriever = RRtrackRetriever()
    trackOutputData = rrTrackRetriever.readTrackFile(irctPath +  "irt_tracks_output.txt")
    objectsOutputData = rrTrackRetriever.readObjectFile(irctPath +  "irt_objects_output.txt")

    collocator = Collocator(deltaX, deltaY)

    timeIdxArange = np.arange(initTimeStep, endTimeStep, 1)
    #timeIdxArange = np.arange(0, 150)

    for tIdx in timeIdxArange:
        print("========== {:06d} ==========".format(tIdx))
        cloudObject = np.array(nc.Dataset(cloudObjectPath+"cloudLabel-{:06d}.nc".format(tIdx-1))["cloudLabel"][0])
        lwpTrackField = lwpTrackData[tIdx, :, :][0].astype(int)
        lwpTrackMetaData = trackOutputData[trackOutputData['timeStep'] == tIdx]
        lwpObjectMetaData = objectsOutputData[objectsOutputData['timeStep'] == tIdx]

        lwpTrackField = collocator.lwpCollocate(lwpTrackField, lwpTrackMetaData)

        #xrLWP = xr.DataArray(lwpTrackField[np.newaxis, :, :], 
        #                     coords = {'time': np.ones(shape=(1,)), 'yc': yc, 'xc': xc}, 
        #                     dims = ["time", "yc", "xc"], 
        #                     name = "lwp")
        #xrLWP.to_netcdf(outputDir + "lwp-{:06d}.nc".format(tIdx))

        figure(figsize=(12, 12))
        pcolormesh(xc/1e3, yc/1e3, np.ma.masked_array(lwpTrackField%23, lwpTrackField<=0), cmap="rainbow", shading="nearest", vmin=0, vmax=23)
        scatter(lwpTrackMetaData["weightCenX"]*1e2/1e3, lwpTrackMetaData["weightCenY"]*1e2/1e3, c=lwpTrackMetaData["trackID"]%23, cmap="rainbow", edgecolor="black")
        for _, i in lwpTrackMetaData.iterrows():
            text(i["weightCenX"]*1e2/1e3, i["weightCenY"]*1e2/1e3, s=int(i["trackID"]), color='black', fontsize=15)
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
        savefig("test-{:06d}.jpg".format(tIdx), dpi=300)
        clf()

