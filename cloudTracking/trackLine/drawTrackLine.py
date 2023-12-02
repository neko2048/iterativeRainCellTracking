import sys
import numpy as np
import pandas as pd
from netCDF4 import Dataset
import matplotlib as mpl
import matplotlib.cm as cm
import matplotlib.pyplot as plt
from utils import DataProcessor, DataRetriever, RRtrackRetriever


if __name__ == "__main__":
    config = {
        "caseName": "mjo",
        "pBarDir": "./datTXT/", 
        "irctTXTDir": "/home/atmenu10246/transition/dat/irct-mjo-cwp-1e-1-221125/", 
        "irctNCDir": "/home/atmenu10246/iterativeRainCellTracking/RRtrack/mjo-cwp-1e-1/", 
        "oriCWPDir": "/home/atmenu10246/transition/src/dat/cwpTrackMask/"
    }
    caseName = config["caseName"]
    dataDir = "/home/atmenu10246/transition/dat/{CN}/archive/".format(CN=caseName)#"/home/atmenu10246/VVM/DATA/diurnal_prescribed/archive/"
    pBar = np.loadtxt("./datTXT/pBar-{CN}.txt".format(CN=caseName))
    inputConfig = sys.argv
    initTimeStep, endTimeStep = int(inputConfig[1]), int(inputConfig[2])

    dataRetriever = DataRetriever(dataDir, "mjo_std_mg")
    rrTrackRetriever = RRtrackRetriever()
    cloudTrackDir = config["irctTXTDir"] + "irt_tracks_output.txt"
    cloudTrackdata = rrTrackRetriever.readTrackFile(cloudTrackDir)

    cloudTrackLinkDir = config["irctTXTDir"] + "irt_tracklinks_output.txt"
    cloudTrackLinkdata = rrTrackRetriever.readTrackFile(cloudTrackLinkDir)

    minPerTimeIdx = 10

    thData = dataRetriever.getThermoDynamic(0)
    xc, yc, zc = np.array(thData['xc']), np.array(thData['yc']), np.array(thData['zc'])
    dataProcessor = DataProcessor(xc, yc, zc)
    tIdxArange = np.arange(initTimeStep, endTimeStep, 1)
    #tIdxArange = np.arange(0, 628, 100)

    objectData = Dataset(config["irctNCDir"] + "irt_objects_mask_mjo_.nc")["var1"]

    cmap = mpl.colors.ListedColormap(['white', 'grey',
                                      'yellow', 'red'])

    for tIdx in tIdxArange:
        print("========== {:06d} ==========".format(tIdx))
        plt.figure(figsize=(12, 12), dpi=200)
        cwp = np.array(Dataset(config["oriCWPDir"] + "cwp-{:06d}.nc".format(tIdx))["cwp"][0])
        cwpMask = cwp >= 0.1
        map = np.zeros_like(cwp)
        map[cwpMask] = 1

        subObjectData = objectData[tIdx+1, 0, :, :]
        subObjectDataMask = subObjectData >= 1
        map[subObjectDataMask] = 2

        map[np.logical_and(cwpMask, subObjectDataMask)] = 3
        plt.pcolormesh(xc/1e3, yc/1e3, map, cmap=cmap)
        subCloudTrackData = cloudTrackdata[cloudTrackdata['timeStep'] == tIdx+1]
        for _, i in subCloudTrackData.iterrows():
            trackID = i['trackID']

            plt.text(x=100 * (i['weightCenX'])/1e3, y=100 * (i['weightCenY'])/1e3, s=str(int(trackID)), 
                        c="green")
            indTrackLinkData = (cloudTrackdata[cloudTrackdata["trackID"] == trackID])

            if indTrackLinkData.shape[0]:
                modWeightCenX, modWeightCenY = rrTrackRetriever.modCenXY(100*np.array(indTrackLinkData["weightCenX"])/1e3, 100*np.array(indTrackLinkData["weightCenY"])/1e3)
                for j in range(len(modWeightCenX)):
                    plt.plot(modWeightCenX[j], modWeightCenY[j], c=cm.turbo(trackID%83/83))
        plt.scatter(100 * (subCloudTrackData['weightCenX'])/1e3, 100 * (subCloudTrackData['weightCenY'])/1e3, 
                    c=[cm.turbo(int(x)%83/83) for x in subCloudTrackData['trackID']], s=10)
        plt.xlim(0, 102.4)
        plt.ylim(0, 102.4)
        plt.xticks(fontsize=20)
        plt.yticks(fontsize=20)
        plt.xlabel("X [km]", fontsize=20)
        plt.ylabel("Y [km]", fontsize=20)
        plt.title("Time: {:04d} hr {:02d} min".format(tIdx*minPerTimeIdx//60, tIdx*minPerTimeIdx%60), fontsize=20, loc="right")
        plt.savefig("compareTrack-{:06d}.jpg".format(tIdx))
        plt.clf()