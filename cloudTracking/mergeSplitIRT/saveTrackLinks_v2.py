import sys
import numpy as np
import xarray as xr
import netCDF4 as nc
from scipy import ndimage
from matplotlib.pyplot import *
from utils import DataProcessor, DataRetriever, RRtrackRetriever

def findParentChild(dataTrackID, dataParentTrack, dataParentTrackSec, dataChildTrack, dataChildTrackSec, trackID):
    #if np.sum(dataTrackID == trackID) == 0: return None # track ID doesn't exist anymore in trackID list. (be overwritten)
    parent = dataParentTrack[dataTrackID==trackID][0]
    parentSec = dataParentTrackSec[dataTrackID==trackID][0]
    childTrack = dataChildTrack[dataTrackID == trackID][0]
    childTrackSec = dataChildTrackSec[dataTrackID == trackID][0]
    nonZeroFamaly = [x for x in [trackID, parent, parentSec, childTrack, childTrackSec] if x != 0]
    return nonZeroFamaly


if __name__ == "__main__":
    #inputConfig = sys.argv
    #initTimeStep, endTimeStep = int(inputConfig[1]), int(inputConfig[2])
    caseName = "mjo"
    homePath = "/home/atmenu10246/"
    vvmPath = homePath + "transition/dat/{CN}/archive/".format(CN=caseName)
    qcObjectPath = homePath + "transition/dat/cloudLabel/qcFilter/"
    cloudObjectPath = homePath + "transition/dat/cloudLabel/filter/"
    irctPath = homePath + "iterativeRainCellTracking/RRtrack/mjo-cwp-2.5e-2/"
    reIdxLWP = homePath + "transition/dat/reindexLWP/"
    zzPath = homePath + "transition/src/datTXT/"
    collocateQcOutputPath = homePath + "transition/dat/collocateCloud/qcCollocate/"
    historyTrackPath = homePath + "transition/dat/collocateCloud/collocateCloudByQc/"
    minPerTimeIdx = 10

    rrTrackRetriever = RRtrackRetriever()
    trackOutputData = rrTrackRetriever.readTrackHeader(irctPath +  "irt_tracklinks_output.txt")
    listTrackID = np.arange(1, max(trackOutputData["trackID"])+1).astype(int)
    dataTrackID = np.array(trackOutputData["trackID"]).astype(int)
    dataParentTrack = np.array(trackOutputData["parentTrack"]).astype(int)
    dataParentTrackSec = np.array(trackOutputData["parentTrackSec"]).astype(int)
    dataChildTrack = np.array(trackOutputData["childTrack"]).astype(int)
    dataChildTrackSec = np.array(trackOutputData["childTrackSec"]).astype(int)
    trackLinkID = np.zeros(shape=listTrackID.shape).astype(int)


    familyDict = {}
    for trackID in listTrackID:
        if trackID % 1000 == 0:
            print("---------- {} / {} ----------".format(trackID, listTrackID[-1]))
        familyDict[trackID] = findParentChild(dataTrackID, dataParentTrack, dataParentTrackSec, dataChildTrack, dataChildTrackSec, trackID)

    for trackID, family in familyDict.items():
        print(trackID, family)
        if len(family) == 1 and trackLinkID[listTrackID == trackID][0] == 0:
            print("Only {} itself".format(family))
            # if only trackID itself and unflagged yet, flag it self.
            trackLinkID[listTrackID == trackID] = trackID
        elif len(family) > 1:
            print("{} has family".format(family))
            for familyID in family:
                if trackLinkID[listTrackID == familyID][0] == 0:
                    # if this family ID is unflagged, flag it as trackID
                    print("    unflagged: {}".format(trackID))
                    trackLinkID[listTrackID == familyID] = trackID
                else:
                    # if this family ID is flagged, flag all grid with this flagged ID to new (trackID)
                    flaggedID = trackLinkID[listTrackID == familyID][0]
                    trackLinkID[trackLinkID == flaggedID] = trackID
                    print("    flagged {} -> {}".format(flaggedID, trackID))



    np.savetxt("trackLinkID.txt", trackLinkID.astype(int))

