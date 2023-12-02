import numpy as np
from netCDF4 import Dataset
from matplotlib.pyplot import *
from utils import DataProcessor, DataRetriever, RRtrackRetriever

def readTXT(dataDir):
    singleTimeTrackID = []
    with open(historyTrackPath + "historyTrackPath-{:06d}.txt".format(tIdx)) as f:
        data = f.readlines()
        for i in range(len(data)):
            oneLineData = data[i].split()
            singleTimeTrackID.extend(oneLineData)
    return singleTimeTrackID


if __name__ == "__main__":
    inputConfig = sys.argv
    initTimeStep, endTimeStep = int(inputConfig[1]), int(inputConfig[2])
    caseName = "mjo"
    homePath = "/home/atmenu10246/"
    vvmPath = homePath + "transition/dat/{CN}/archive/".format(CN=caseName)
    qcObjectPath = homePath + "transition/dat/cloudLabel/qcFilter/"
    cloudObjectPath = homePath + "transition/dat/cloudLabel/filter/"
    irctPath = homePath + "iterativeRainCellTracking/RRtrack/mjo-cwp-1e-1/"
    reIdxLWP = homePath + "transition/dat/reindexLWP/"
    zzPath = homePath + "transition/src/datTXT/"
    collocateQcOutputPath = homePath + "transition/dat/collocateCloud/qcCollocate/"
    historyTrackPath = homePath + "transition/dat/collocateCloud/collocateCloudByQc/"

    minPerTimeIdx = 10

    #dataRetriever = DataRetriever(vvmPath, "mjo_std_mg")
    #thData = dataRetriever.getThermoDynamic(0)
    #xc, yc, zc = np.array(thData['xc']), np.array(thData['yc']), np.array(thData['zc'])
    #zz = np.loadtxt(zzPath + "zz-{}.txt".format(caseName))
    #zc3D = np.tile(zc[:, np.newaxis, np.newaxis], reps=(1, len(yc), len(xc)))

    rrTrackRetriever = RRtrackRetriever()
    trackOutputData = rrTrackRetriever.readTrackFile(irctPath +  "irt_tracks_output.txt")
    
    maxTrackID = int(np.max(trackOutputData["trackID"]))
    trackIDList = np.arange(1, maxTrackID+1, 1).astype(int)
    historyTrackID = np.zeros(shape=(len(trackIDList), 628))
    timeIdxArange = np.arange(initTimeStep, endTimeStep, 1)

    for tIdx in timeIdxArange:
        if tIdx % 50 == 0: print(tIdx)
        singleTimeTrackID = readTXT(historyTrackPath + "historyTrackPath-{:06d}.txt".format(tIdx))
        historyTrackID[:, tIdx] = singleTimeTrackID


    #for tIdx in timeIdxArange[::-1][:-1]:
    #    trackIDsInOneTime = historyTrackID[:, tIdx]
    #    trackIDsInPreTime = historyTrackID[:, tIdx-1]
    #    #uniqueTrackID, counts = np.unique(trackIDsInOneTime, return_counts=True)
    #    #counts = counts[uniqueTrackID>0]
    #    #uniqueTrackID = uniqueTrackID[uniqueTrackID>0]
    #    for i, trackID in enumerate(trackIDsInOneTime):
    #        if trackIDsInOneTime[i] != 0 and trackIDsInPreTime[i] != 0:
    #            



        #eachTrackID = historyTrackID[trackIdx, :]
        #uniqueTrackID, counts = np.unique(eachTrackID, return_counts=True)
        #counts = counts[uniqueTrackID>0]
        #uniqueTrackID = uniqueTrackID[uniqueTrackID>0]
        #if len(uniqueTrackID) >= 2:
        #    updateTrackID = np.zeros(shape=eachTrackID.shape)
        #    updateTrackID[eachTrackID>0] = np.max(uniqueTrackID)
        #    historyTrackID[trackIdx, :]
        #condition = np.logical_and(counts>=1, uniqueTrackID!=0)
        #if np.sum(condition) > 1:
        #    print(uniqueTrackID[condition])
        #else:
        #    continue
    figure(figsize=(15, 12))
    pcolormesh(np.ma.masked_array(historyTrackID%17, historyTrackID==0), cmap="jet")
    xticks(np.arange(0, 628, 60), np.arange(0, 626*minPerTimeIdx/60, 60*minPerTimeIdx/60))
    #xlim(300, 400)
    #ylim(3300, 4500)
    savefig("test.jpg", dpi=500)