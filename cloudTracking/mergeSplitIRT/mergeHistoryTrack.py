import sys
import numpy as np
import xarray as xr
import netCDF4 as nc
from scipy.ndimage.measurements import label
from matplotlib.pyplot import *
from utils import DataProcessor, DataRetriever, RRtrackRetriever

if __name__ == "__main__":
    inputConfig = sys.argv
    initTimeStep, endTimeStep = int(inputConfig[1]), int(inputConfig[2])
    caseName = "mjo"
    homePath = "/home/atmenu10246/"
    vvmPath = homePath + "transition/dat/{CN}/archive/".format(CN=caseName)
    qcObjectPath = homePath + "transition/dat/cloudLabel/qc2LayerFilterInSameCloud/"
    cloudObjectPath = homePath + "transition/dat/cloudLabel/cloud2LayerFilter/"
    irctPath = homePath + "iterativeRainCellTracking/RRtrack/mjo-cwp-25e-3/"
    reIdxLWP = homePath + "transition/dat/reindexLWP/reindexLWP-25e-3/"
    trackLinkID_Path = homePath + "transition/src/cloudTracking/mergeSplitIRT/"
    zzPath = homePath + "transition/src/datTXT/"
    trackIDListQc2LayerPath = homePath + "transition/dat/collocateCloud/qcCollocate2Layer/"
    trackOutputPath = homePath + "transition/src/cloudTracking/mergeSplitIRT/"
    minPerTimeIdx = 10

    #dataRetriever = DataRetriever(vvmPath, "mjo_std_mg")
    #thData = dataRetriever.getThermoDynamic(0)
    #xc, yc, zc = np.array(thData['xc']), np.array(thData['yc']), np.array(thData['zc'])
    #zz = np.loadtxt(zzPath + "zz-{}.txt".format(caseName))
    #zc3D = np.tile(zc[:, np.newaxis, np.newaxis], reps=(1, len(yc), len(xc)))

    trackLinkList = np.loadtxt(trackLinkID_Path + "trackLinkID-25e-3.txt")
    timeIdxArange = np.arange(initTimeStep, endTimeStep, 1)
    #historyTrackArr = np.zeros(shape=(len(trackLinkList), 628))
    finalTrack = np.zeros(shape=(len(trackLinkList), ))
    #f = open("errorQcCollocate{:03d}{:03d}.txt".format(initTimeStep, endTimeStep), "w")


    for tIdx in timeIdxArange:
        print("========== {:06d} ==========".format(tIdx))
        singleTimeTrackID = np.loadtxt(trackIDListQc2LayerPath + "trackIDListQc2Layer-25e-3-{:06d}.txt".format(tIdx))
        uniqueID, countsID = np.unique(singleTimeTrackID, return_counts=True)
        condition = np.logical_and(countsID>=2, uniqueID>0)
        if np.sum(condition) > 0:
            for uID, cID in zip(uniqueID[condition], countsID[condition]):
                print("{} / {}".format(uID, uniqueID[-1]))
                correspondTrackLink = trackLinkList[singleTimeTrackID==uID]
                # correspondTrackLink means one qc object overlaps with multiple LWP regions
                correspondTrackLink = np.unique(correspondTrackLink)
                maxID = np.max(correspondTrackLink)
                for i in range(len(finalTrack)):
                    if finalTrack[i] == 0 and trackLinkList[i] in correspondTrackLink:
                    # if this position is unflagged and its trackLinks are in correspondTrackLink
                        finalTrack[i] = maxID
                    elif finalTrack[i] in correspondTrackLink:
                    # if this position is flagged and in correspondTrackLink
                        finalTrack[finalTrack == finalTrack[i]] = maxID
                #for i in correspondTrackLink:
                #    if i in finalTrack:
                #        finalTrack[finalTrack==i] = np.max(correspondTrackLink)
                #    else:
                #        finalTrack[trackLinkIDs==i] = np.max(correspondTrackLink)

    for i in range(len(finalTrack)):
        if finalTrack[i] == 0:
            finalTrack[i] = trackLinkList[i]

    np.savetxt(trackOutputPath + "finalTrack-25e-3.txt", finalTrack)







