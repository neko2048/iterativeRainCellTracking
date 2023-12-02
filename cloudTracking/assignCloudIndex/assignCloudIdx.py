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
    qcObjectPath = homePath + "transition/dat/collocateCloud/qcCollocate2Layer/"
    cloudObjectPath = homePath + "transition/dat/cloudLabel/cloud2LayerFilter/"
    irctPath = homePath + "iterativeRainCellTracking/RRtrack/mjo-cwp-25e-3/"
    reIdxLWP = homePath + "transition/dat/reindexLWP/reindexLWP-25e-3/"
    trackLinkID_Path = homePath + "transition/src/cloudTracking/mergeSplitIRT/"
    zzPath = homePath + "transition/src/datTXT/"
    collocateCloudOutputPath = homePath + "transition/dat/collocateCloud/collocateCloudByQc2Layer/"
    minPerTimeIdx = 10

    dataRetriever = DataRetriever(vvmPath, "mjo_std_mg")
    thData = dataRetriever.getThermoDynamic(0)
    xc, yc, zc = np.array(thData['xc']), np.array(thData['yc']), np.array(thData['zc'])
    zz = np.loadtxt(zzPath + "zz-{}.txt".format(caseName))
    zc3D = np.tile(zc[:, np.newaxis, np.newaxis], reps=(1, len(yc), len(xc)))

    #rrTrackRetriever = RRtrackRetriever()
    #trackOutputData = rrTrackRetriever.readTrackFile(irctPath +  "irt_tracks_output.txt")
    trackLinkList = np.loadtxt(trackLinkID_Path + "trackLinkID-25e-3.txt")
    trackIDOrigin = np.arange(1, len(trackLinkList)+1)
    finalTrackID = np.loadtxt(trackLinkID_Path + "finalTrack-25e-3.txt")
    timeIdxArange = np.arange(initTimeStep, endTimeStep, 1)

    f = open("error{:03d}{:03d}CollocateCloudtxt".format(initTimeStep, endTimeStep), "w")
    for tIdx in timeIdxArange:
        print("========== {:06d} ==========".format(tIdx))

        qcObject = np.array(nc.Dataset(qcObjectPath+"cloudLabel-{:06d}.nc".format(tIdx-1))["cloudLabel"][0])
        newCloudObject = np.zeros(qcObject.shape)
        #collocateQc = np.zeros(shape=(len(zc), len(yc), len(xc)))
        if np.max(qcObject) > 0:
            cloudObject = np.array(nc.Dataset(cloudObjectPath+"cloudLabel-{:06d}.nc".format(tIdx-1))["cloudLabel"][0])
            lwpTrackField = np.array(nc.Dataset(reIdxLWP+"lwp-{:06d}.nc".format(tIdx))["lwp"][0])
            lwpTrackField3D = np.tile(lwpTrackField[np.newaxis, :, :], reps=(len(zc), 1, 1))
            cloudObjectCandicate = np.unique(cloudObject * (lwpTrackField3D > 0))
            cloudObjectCandicate = cloudObjectCandicate[cloudObjectCandicate > 0]
            totalCount = len(cloudObjectCandicate)
            count = 0
            for cloudIdx in cloudObjectCandicate:
                count += 1
                print("Cloud: {} / {} | {} / {}".format(cloudIdx, cloudObjectCandicate[-1], count, totalCount))
                correspondQc = np.unique((cloudObject == cloudIdx) * qcObject)
                correspondQc = correspondQc[correspondQc > 0]
                if len(correspondQc) == 0: 
                    print("No correspond Qc")
                    continue
                correspondQc = np.unique([finalTrackID[trackIDOrigin == x][0] for x in correspondQc])
                if len(correspondQc) == 1:
                    print("    To ", int(correspondQc[0]))
                    newCloudObject[cloudObject == cloudIdx] = correspondQc[0]
                else:
                    print("Error: Unsupposed Qc Situation - Multiple Qc")
                    f.write("Time {:06d}, CloudID: {:06d} has multiple Qc.".format(tIdx, cloudIdx))
                    for i in correspondQc:
                        f.write(str(i) + "\n")
        #data = np.max(cloudObject, axis=0)
        #figure(figsize=(12, 12))
        #pcolormesh(xc/1e3, yc/1e3, np.ma.masked_array(data, data==0), cmap="jet")
        #contour(xc/1e3, yc/1e3, np.max(qcObject, axis=0) > 0, colors='black')
        #contour(xc/1e3, yc/1e3, lwpTrackField, colors='white')
        #savefig("test.jpg", dpi=300)
        #print(tIdx)
        newCloudObject = newCloudObject[np.newaxis, :, :, :] # same dim with VVM.
        xrCollocateQcObject = xr.DataArray(newCloudObject.astype(np.int16), 
                                      coords = {'time': np.ones(shape=(1,)), 'zc': zc, 'yc': yc, 'xc': xc}, 
                                      dims = ["time", "zc", "yc", "xc"], 
                                      name = "cloudLabel")
        xrCollocateQcObject.to_netcdf(collocateCloudOutputPath + "cloudLabel-{:06d}.nc".format(tIdx))




