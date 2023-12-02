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
    collocateQcOutputPath = homePath + "transition/dat/collocateCloud/qcCollocate2Layer/"
    minPerTimeIdx = 10

    dataRetriever = DataRetriever(vvmPath, "mjo_std_mg")
    thData = dataRetriever.getThermoDynamic(0)
    xc, yc, zc = np.array(thData['xc']), np.array(thData['yc']), np.array(thData['zc'])
    zz = np.loadtxt(zzPath + "zz-{}.txt".format(caseName))
    zc3D = np.tile(zc[:, np.newaxis, np.newaxis], reps=(1, len(yc), len(xc)))

    rrTrackRetriever = RRtrackRetriever()
    trackOutputData = rrTrackRetriever.readTrackFile(irctPath +  "irt_tracks_output.txt")
    trackLinkList = np.loadtxt(trackLinkID_Path + "trackLinkID-25e-3.txt")
    trackIDRecord = np.arange(1, len(trackLinkList)+1)
    newTrackIDList = np.zeros(shape=trackLinkList.shape)
    #historyTrackID = np.tile(trackIDList[:, np.newaxis], reps=(1, endTimeStep - initTimeStep))
    timeIdxArange = np.arange(initTimeStep, endTimeStep, 1)

    f = open("errorQcCollocate{:03d}{:03d}.txt".format(initTimeStep, endTimeStep), "w")
    for tIdx in timeIdxArange:
        print("========== {:06d} ==========".format(tIdx))
        lwpTrackField = np.array(nc.Dataset(reIdxLWP+"lwp-{:06d}.nc".format(tIdx+1))["lwp"][0])
        lwpTrackIDCandicate = np.unique(lwpTrackField)
        lwpTrackIDCandicate = lwpTrackIDCandicate[lwpTrackIDCandicate > 0]

        collocateQc = np.zeros(shape=(len(zc), len(yc), len(xc)))
        newTrackIDList = np.zeros(shape=trackLinkList.shape)
        if len(lwpTrackIDCandicate) >= 1:
            qcObject = np.array(nc.Dataset(qcObjectPath+"cloudLabel-{:06d}.nc".format(tIdx))["cloudLabel"][0])
            cloudObject = np.array(nc.Dataset(cloudObjectPath+"cloudLabel-{:06d}.nc".format(tIdx))["cloudLabel"][0])
            lwpTrackField3D = np.tile(lwpTrackField[np.newaxis, :, :], reps=(len(zc), 1, 1))
            qcObjectCandicate = np.unique(qcObject * (lwpTrackField3D > 0))
            qcObjectCandicate = qcObjectCandicate[qcObjectCandicate > 0]

            for qcIdx in qcObjectCandicate:
                print("Qc: {} / {}".format(qcIdx, qcObjectCandicate[-1]))
                correspondLWP, area = np.unique((qcObject==qcIdx).any(axis=0) * lwpTrackField, return_counts=True)
                area = area[correspondLWP > 0]
                correspondLWP = correspondLWP[correspondLWP > 0]
                if len(correspondLWP) == 0: 
                    print("No correspond LWP")
                    continue
                #correspondCloud = np.unique((qcObject==qcIdx).any(axis=0) * cloudObject)
                #if len(correspondCloud) > 1:
                #    f.write("Time {:06d}, QcID: {:06d} shared with another cloud.".format(tIdx, qcIdx, len(correspondLWP)))
                #    print("It has another qc object connected")
                #    elseQc = np.unique((cloudObject == correspondCloud) * qcObject)
                #    elseQc = elseQc[elseQc > 0]
                #    for i in elseQc:
                #        isQc = 
                correspondLWP, area = np.unique((qcObject==qcIdx).any(axis=0) * lwpTrackField, return_counts=True)
                area = area[correspondLWP > 0]
                correspondLWP = correspondLWP[correspondLWP > 0]
                correspondLWPid = correspondLWP[area == np.max(area)][0]
                collocateQc[qcObject == qcIdx] = correspondLWPid
                if (newTrackIDList[trackLinkList==correspondLWPid][0] == 0):
                    newTrackIDList[trackLinkList==correspondLWPid] = correspondLWPid
                    print("To {}".format(correspondLWPid))
                else:
                    print("Flagged")
                if len(correspondLWP) != 1:
                    f.write("Time {:06d}, QcID: {:06d}, lenLWP: {}\n".format(tIdx, qcIdx, len(correspondLWP)))
                    for i in correspondLWP:
                        f.write(str(i) + "\n")
                        print("{} To {}".format(i, correspondLWPid))
                        newTrackIDList[trackLinkList==i] = correspondLWPid
        #data = np.max(qcObject, axis=0)
        #figure(figsize=(12, 12))
        #pcolormesh(xc/1e3, yc/1e3, np.ma.masked_array(data, data==0)%23, shading="nearest", cmap="rainbow", vmin=0, vmax=23)
        #pcolormesh(xc/1e3, yc/1e3, np.ma.masked_array(lwpTrackField, lwpTrackField<=0)%23, cmap="jet", vmin=0, vmax=23)#, linewidths=0.1)
        #contour(xc/1e3, yc/1e3, (cloudObject>0).any(axis=0), colors="blue", linewidths=0.1)
        #savefig("test-{:06d}.jpg".format(tIdx), dpi=300)
        #clf()

        collocateQc = collocateQc[np.newaxis, :, :, :] # same dim with VVM.
        xrCollocateQcObject = xr.DataArray(collocateQc.astype(np.int16), 
                                      coords = {'time': np.ones(shape=(1,)), 'zc': zc, 'yc': yc, 'xc': xc},
                                      dims = ["time", "zc", "yc", "xc"],
                                      name = "cloudLabel")
        xrCollocateQcObject.to_netcdf(collocateQcOutputPath + "cloudLabel-{:06d}.nc".format(tIdx))
        np.savetxt(collocateQcOutputPath + "trackIDListQc2Layer-25e-3-{:06d}.txt".format(tIdx), newTrackIDList)

