import numpy as np
import pandas as pd
from copy import copy
import matplotlib.cm as cm
from netCDF4 import Dataset
import matplotlib.pyplot as plt
from utils import DataProcessor, DataRetriever

def readObjectFile(objectFileDir):
    cloudTrackFile = open(objectFileDir, 'r')
    data = []
    for l in cloudTrackFile.readlines():
        singleLine = [x for x in l.split(" ") if x != ""]
        data.append([float(x) for x in singleLine[:17]])
    cloudTrackFile.close()

    data = pd.DataFrame(data)
    columns=["timeStep", "objectID1", "objectID2", "age1", "age2", 
             "area", "meanArea", "minArea", "maxArea", 
             "minXCor", "maxXCor", "minYCor", "maxYCor", "weightCenX", "weightCenY", 
             "estVelX", "estVelY"]#, "-", "-", "-", "-", "-", "-", "-", "-"})
    for i in range(len(columns)):
        data.rename(columns={i: columns[i]}, inplace=True)

    return data

def readTrackFile(trackFileDir):
    cloudTrackFile = open(trackFileDir, 'r')
    data = []
    for l in cloudTrackFile.readlines():
        if "*" in l: continue
        singleLine = [x for x in l.split(" ") if x != ""]
        if (len(singleLine) != 5):
            data.append([float(x) for x in singleLine][:18])
    cloudTrackFile.close()

    data = pd.DataFrame(data)
    columns=["trackID", "timeStep", "objectID1", "objectID2", "age1", "age2", 
             "gridArea", "meanArea", "minArea", "maxArea", 
             "minXCor", "maxXCor", "minYCor", "maxYCor", "weightCenX", "weightCenY", 
             "estVelX", "estVelY"]#, "-", "-", "-", "-", "-", "-", "-", "-"})
    for i in range(len(columns)):
        data.rename(columns={i: columns[i]}, inplace=True)

    return data

if __name__ == "__main__":
    caseName = "mjo"
    dataDir = "/home/atmenu10246/transition/dat/{CN}/archive/".format(CN=caseName)#"/home/atmenu10246/VVM/DATA/diurnal_prescribed/archive/"
    pBar = np.loadtxt("./datTXT/pBar-{CN}.txt".format(CN=caseName))
    dataRetriever = DataRetriever(dataDir, "mjo_std_mg")

    cloudTrackDir = "/home/atmenu10246/transition/dat/irct-mjo-cwp1lim/RRtrack/irt_tracks_output.txt"
    cloudTrackdata = readTrackFile(cloudTrackDir)

    minPerTimeIdx = 10

    thData = dataRetriever.getThermoDynamic(0)
    xc, yc, zc = np.array(thData['xc']), np.array(thData['yc']), np.array(thData['zc'])
    dataProcessor = DataProcessor(xc, yc, zc)
    tIdxArange = np.arange(np.min(cloudTrackdata['timeStep']), np.max(cloudTrackdata['timeStep'])).astype(int)

    for tIdx in tIdxArange[402:410]:

        print("========== {:06d} ==========".format(tIdx))
        cwp = np.array(Dataset("/home/atmenu10246/transition/src/dat/cwpTrackMask/cwp-{:06d}.nc".format(tIdx))["cwp"][0])
        #cwp = np.where(cwp>=1e-3, cwp, np.nan)
        cwp = cwp>=1e-3
        subCloudTrackData = cloudTrackdata[cloudTrackdata['timeStep'] == tIdx]

        plt.figure(figsize=(15, 12), dpi=300)
        cmap = copy(plt.get_cmap("jet"))
        plt.title("Height of TOC [km]", fontsize=30, y=1.05)
        plt.title("Time: {hr:04d} hr {minute:02d} min".format(
                hr = tIdx * minPerTimeIdx // 60, 
                minute = tIdx * minPerTimeIdx % 60), 
                loc='right', fontsize=20)
        plt.pcolormesh(xc/1e3, yc/1e3, cwp*1e3, cmap=cmap)#, vmin=0, vmax=50)
        cbar = plt.colorbar(extend='max')
        cbar.ax.tick_params(labelsize=20)
        cbar.ax.get_yaxis().labelpad = 15
        #plt.contour(xc/1e3, yc/1e3, rain * 3600 > 0.1, colors='white', width=10) # 1mm/hr

        for _, i in subCloudTrackData.iterrows():
            print("{}'s location: {}, {}".format(i['trackID'], i['weightCenX'], i['weightCenY']))
            plt.scatter(100 * (i['weightCenX'])/1e3, 100 * (i['weightCenY'])/1e3, 
                        c=i['trackID'], cmap='rainbow')

        plt.savefig("tracking{:06d}.jpg".format(tIdx))
        plt.clf()


