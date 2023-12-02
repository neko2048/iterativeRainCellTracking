import sys
import numpy as np
import xarray as xr
import netCDF4 as nc
from scipy import ndimage
from matplotlib.pyplot import *
from utils import DataProcessor, DataRetriever, RRtrackRetriever


if __name__ == "__main__":
    caseName = "mjo"
    homePath = "/home/atmenu10246/"
    vvmPath = homePath + "transition/dat/{CN}/archive/".format(CN=caseName)
    cloudObjectPath = homePath + "transition/dat/cloudLabel/filter/"
    irctPath = homePath + "iterativeRainCellTracking/RRtrack/mjo-cwp-1e-1/"
    reIdxLWP = homePath + "transition/dat/reindexLWP/"
    zzPath = homePath + "transition/src/datTXT/"
    collocateCloudOutputPath = homePath + "transition/dat/collocateCloudTest2/"
    collocateCloudFinalOutputPath = homePath + "transition/dat/collocateCloudFinalTest2/"
    indexMapOutput = homePath + "transition/dat/indexMap/"
    minPerTimeIdx = 10

    dataRetriever = DataRetriever(vvmPath, "mjo_std_mg")
    thData = dataRetriever.getThermoDynamic(0)
    xc, yc, zc = np.array(thData['xc']), np.array(thData['yc']), np.array(thData['zc'])
    zz = np.load(zzPath + "zz-{}.txt".format(caseName))
    zc3D = np.tile(zc[:, np.newaxis, np.newaxis], reps=(1, len(yc), len(xc)))
    deltaX, deltaY = xc[1] - xc[0], yc[1] - yc[0]

    rrTrackRetriever = RRtrackRetriever()
    trackOutputData = rrTrackRetriever.readTrackFile(irctPath +  "irt_tracks_output.txt")

    trackRecorder = np.arange(1, int(np.max(trackOutputData["trackID"]))+1)

    timeIdxArange = np.arange(400, 410)

    for tIdx in timeIdxArange:
        print("========== {:06d} ==========".format(tIdx))
        cloudBaseDict = {}
        cloudObject = np.array(nc.Dataset(cloudObjectPath+"cloudLabel-{:06d}.nc".format(tIdx-1))["cloudLabel"][0])
        lowBaseCloudObject = np.zeros(cloudObject.shape)
        collocateCloudObject = np.zeros(cloudObject.shape)
        lwpTrackField = np.array(nc.Dataset(reIdxLWP+"lwp-{:06d}.nc".format(tIdx))["lwp"][0])
        
        lowBaseCloudObjectCandicate = np.unique(cloudObject[zc<=5e3, :, :])
        lowBaseCloudObjectCandicate = lowBaseCloudObjectCandicate[lowBaseCloudObjectCandicate!=0]
        if lowBaseCloudObjectCandicate != []:
            maxIndex = np.max(lowBaseCloudObjectCandicate)
            depLowCloudObjectCandicate = np.zeros(lowBaseCloudObjectCandicate.shape)
            for lowCloudIdx in lowBaseCloudObjectCandicate:
                print("{} / {} Cloud Filter @ {}".format(lowCloudIdx, maxIndex, tIdx))
                cloudTop = (cloudObject==lowCloudIdx).any(axis=(1, 2)) * zz[1:]
                cloudTop = np.max(cloudTop)
                cloudBase = (cloudObject==lowCloudIdx).any(axis=(1, 2)) * zz[:-1]
                cloudBase = np.min(cloudBase[cloudBase != 0])
                if (cloudTop - cloudBase > 2e2): # over than 200 m, from Yi-Chang
                    cloudBaseDict[lowCloudIdx] = cloudBase
                    lowBaseCloudObject[cloudObject==lowCloudIdx] = lowCloudIdx
                    depLowCloudObjectCandicate[lowBaseCloudObjectCandicate==lowCloudIdx] = lowCloudIdx
            depLowCloudObjectCandicate = depLowCloudObjectCandicate[depLowCloudObjectCandicate!=0]

            for lowCloudIdx in depLowCloudObjectCandicate:
                cloudProjection = (lowBaseCloudObject == lowCloudIdx).any(axis=0)
                incloudLWPidx = np.unique(cloudProjection * lwpTrackField)
                incloudLWPidx = incloudLWPidx[incloudLWPidx > 0]
                # if len(incloudLWPidx) == 0: no correspond cloud
                if len(incloudLWPidx) == 1:
                    print("{} / {}: 1 LWP <-> 1 Cloud Cluster @ {}".format(int(lowCloudIdx), np.max(lowBaseCloudObjectCandicate), tIdx))
                    collocateCloudObject[lowBaseCloudObject == lowCloudIdx] = incloudLWPidx[0]
                elif len(incloudLWPidx) > 1:
                    print("{} / {}: >2 LWP <-> 1 Cloud Cluster @ {}".format(int(lowCloudIdx), np.max(lowBaseCloudObjectCandicate), tIdx))
                    collocateCloudObject[lowBaseCloudObject == lowCloudIdx] = -lowCloudIdx
                    for i in incloudLWPidx:
                        trackRecorder[trackRecorder == i] = max(incloudLWPidx)


        collocateCloudObject = collocateCloudObject[np.newaxis, :, :, :] # same dim with VVM.
        xrCollocateCloudObject = xr.DataArray(collocateCloudObject.astype(int), 
                                      coords = {'time': np.ones(shape=(1,)), 'zc': zc, 'yc': yc, 'xc': xc}, 
                                      dims = ["time", "zc", "yc", "xc"], 
                                      name = "cloudLabel")
        xrCollocateCloudObject.to_netcdf(collocateCloudOutputPath + "cloudLabel-{:06d}.nc".format(tIdx))
        np.save(indexMapOutput + "trackRecorder-{:06d}.npy".format(tIdx), trackRecorder)

    for tIdx in timeIdxArange:
        print("========== {:06d} ==========".format(tIdx))
        cloudObject = np.array(nc.Dataset(collocateCloudOutputPath + "cloudLabel-{:06d}.nc".format(tIdx))["cloudLabel"][0])
        lwpTrackField = np.array(nc.Dataset(reIdxLWP+"lwp-{:06d}.nc".format(tIdx))["lwp"][0])
        #val, count = np.unique(trackRecorder, return_counts=True)
        cloudCandicate = np.unique(cloudObject)
        cloudCandicate = cloudCandicate[cloudCandicate < 0]
        if len(cloudCandicate) != 1:
            for cloudIdx in cloudCandicate:
                cloudProjection = (cloudObject == cloudIdx).any(axis=0)
                incloudLWPidx = np.unique(cloudProjection * lwpTrackField)
                incloudLWPidx = incloudLWPidx[incloudLWPidx > 0]
                cloudObject[cloudObject == cloudIdx] = trackRecorder[incloudLWPidx[0] - 1]


        cloudObject = cloudObject[np.newaxis, :, :, :] # same dim with VVM.
        xrCloudObject = xr.DataArray(cloudObject.astype(int), 
                                      coords = {'time': np.ones(shape=(1,)), 'zc': zc, 'yc': yc, 'xc': xc}, 
                                      dims = ["time", "zc", "yc", "xc"], 
                                      name = "cloudLabel")
        xrCloudObject.to_netcdf(collocateCloudFinalOutputPath + "cloudLabel-{:06d}.nc".format(tIdx))





