import sys
import numpy as np
import xarray as xr
import netCDF4 as nc
from scipy import ndimage
from matplotlib.pyplot import *
from utils import DataProcessor, DataRetriever, RRtrackRetriever



if __name__ == "__main__":
    #inputConfig = sys.argv
    #initTimeStep, endTimeStep = int(inputConfig[1]), int(inputConfig[2])
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

    rrTrackRetriever = RRtrackRetriever()
    trackOutputData = rrTrackRetriever.readTrackHeader(irctPath +  "irt_tracklinks_output.txt")
    listTrackID = np.arange(1, max(trackOutputData["trackID"])+1)
    dataTrackID = np.array(trackOutputData["trackID"]).astype(int)
    dataParentTrack = np.array(trackOutputData["parentTrack"]).astype(int)
    dataParentTrackSec = np.array(trackOutputData["parentTrackSec"]).astype(int)
    dataChildTrack = np.array(trackOutputData["childTrack"]).astype(int)
    dataChildTrackSec = np.array(trackOutputData["childTrackSec"]).astype(int)

    for trackID in listTrackID[::-1]:
        if trackID % 1000 == 0:
            print("---------- {} / {} ----------".format(trackID, listTrackID[-1]))
        if len(dataParentTrack == trackID) != 0:
            dataParentTrackSec[dataParentTrack == trackID] = trackID
            dataChildTrack[dataParentTrack == trackID] = trackID
            dataChildTrackSec[dataParentTrack == trackID] = trackID
            dataTrackID[dataParentTrack == trackID] = trackID
        if len(dataParentTrackSec == trackID) != 0:
            dataParentTrack[dataParentTrackSec == trackID] = trackID
            dataChildTrack[dataParentTrackSec == trackID] = trackID
            dataChildTrackSec[dataParentTrackSec == trackID] = trackID
            dataTrackID[dataParentTrackSec == trackID] = trackID
        if len(dataChildTrack == trackID) != 0:
            dataParentTrack[dataChildTrack == trackID] = trackID
            dataParentTrackSec[dataChildTrack == trackID] = trackID
            dataChildTrackSec[dataChildTrack == trackID] = trackID
            dataTrackID[dataChildTrack == trackID] = trackID
        if len(dataChildTrackSec == trackID) != 0:
            dataParentTrack[dataChildTrackSec == trackID] = trackID
            dataParentTrackSec[dataChildTrackSec == trackID] = trackID
            dataChildTrack[dataChildTrackSec == trackID] = trackID
            dataTrackID[dataChildTrackSec == trackID] = trackID

    for i in np.arange(len(dataTrackID)):
        tracID = dataTrackID[i]
        parent = dataParentTrack[i]
        parentSec = dataParentTrackSec[i]
        child = dataChildTrack[i]
        childSec = dataChildTrackSec[i]
        if (len(np.unique([trackID, parent, parentSec, child, childSec])) != 1):
            print(trackID, parent, parentSec, child, childSec)

    np.savetxt("trackLinkID.txt", dataTrackID.astype(int))

