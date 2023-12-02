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
    qcObjectPath = homePath + "transition/dat/collocateCloud/qcCollocate/"
    cloudObjectPath = homePath + "transition/dat/cloudLabel/filter/"
    irctPath = homePath + "iterativeRainCellTracking/RRtrack/mjo-cwp-1e-1/"
    reIdxLWP = homePath + "transition/dat/reindexLWP/"
    trackIdxPath = homePath + "transition/src/mergeSplitIRT/"
    zzPath = homePath + "transition/src/datTXT/"
    collocateCloudOutputPath = homePath + "transition/dat/collocateCloud/collocateCloudByQc/"
    minPerTimeIdx = 10

    #dataRetriever = DataRetriever(vvmPath, "mjo_std_mg")
    #thData = dataRetriever.getThermoDynamic(0)
    #xc, yc, zc = np.array(thData['xc']), np.array(thData['yc']), np.array(thData['zc'])
    #zz = np.loadtxt(zzPath + "zz-{}.txt".format(caseName))
    #zc3D = np.tile(zc[:, np.newaxis, np.newaxis], reps=(1, len(yc), len(xc)))

    rrTrackRetriever = RRtrackRetriever()
    trackOutputData = rrTrackRetriever.readTrackFile(irctPath +  "irt_tracks_output.txt")
    
    originTrackIDs = np.arange(1, np.max(trackOutputData["trackID"])+1, 1).astype(int)
    finalTrackIDs = np.loadtxt("./finalTrack.txt").astype(int)#trackIdxPath + "msTrack.txt").astype(int)
    #historyTrackID = np.loadtxt(trackIdxPath + "soTrack.txt").astype(int)
    timeIdxArange = np.arange(initTimeStep, endTimeStep, 1)


    #f = open("error-{:03d}-{:03d}.txt".format(initTimeStep, endTimeStep), "w")
    for tIdx in timeIdxArange:
        print("========== {:06d} ==========".format(tIdx))
        lwpTrackField = np.array(nc.Dataset(reIdxLWP+"lwp-{:06d}.nc".format(tIdx+1))["lwp"][0])
        lwpTrackIDCandicate = np.unique(lwpTrackField)
        lwpTrackIDCandicate = lwpTrackIDCandicate[lwpTrackIDCandicate > 0]
        
        if len(lwpTrackIDCandicate) >= 1:
            qcObject = np.array(nc.Dataset(qcObjectPath+"cloudLabel-{:06d}.nc".format(tIdx))["cloudLabel"][0])
            cloudObject = np.array(nc.Dataset(cloudObjectPath+"cloudLabel-{:06d}.nc".format(tIdx))["cloudLabel"][0])
            #collocateCloud = np.zeros(cloudObject.shape)
            cloudObjectCandicate = np.unique(cloudObject)
            cloudObjectCandicate = cloudObjectCandicate[cloudObjectCandicate > 0]
            
            for cloudIdx in cloudObjectCandicate:
                print("Cloud: {} / {}".format(cloudIdx, cloudObjectCandicate[-1]))
                correspondQcIdx = (np.unique((cloudObject==cloudIdx) * qcObject))
                correspondQcIdx = correspondQcIdx[correspondQcIdx > 0]
                if len(correspondQcIdx) != 1:
                    #f.write("Time {:06d}, QcID: {:06d}, lenLWP: {}\n".format(tIdx, cloudIdx, len(correspondQcIdx)))
                    print(correspondQcIdx)
                    print([finalTrackIDs[x-1] for x in correspondQcIdx])
                else:
                    #collocateCloud[cloudObject==cloudIdx] = finalTrackIDs[correspondQcIdx[0]-1]
                    print(correspondQcIdx[0], "-> ", int(finalTrackIDs[correspondQcIdx[0]-1]))

        #else:
        #    collocateCloud = np.zeros((len(zc), len(yc), len(xc)))
        
        #collocateCloud = collocateCloud[np.newaxis, :, :, :] # same dim with VVM.
        #xrCollocateCloudObject = xr.DataArray(collocateCloud.astype(int), 
        #                              coords = {'time': np.ones(shape=(1,)), 'zc': zc, 'yc': yc, 'xc': xc}, 
        #                              dims = ["time", "zc", "yc", "xc"], 
        #                              name = "cloudLabel")
        #xrCollocateCloudObject.to_netcdf(collocateCloudOutputPath + "cloudLabel-{:06d}.nc".format(tIdx))

    #f.close()


