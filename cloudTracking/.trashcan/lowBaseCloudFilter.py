import sys
import numpy as np
import xarray as xr
import netCDF4 as nc
from scipy import ndimage
from matplotlib.pyplot import *
from utils import DataProcessor, DataRetriever, RRtrackRetriever


if __name__ == "__main__":
    inputConfig = sys.argv
    initTimeStep, endTimeStep = int(inputConfig[1]), int(inputConfig[2])
    caseName = "mjo"
    homePath = "/home/atmenu10246/"
    vvmPath = homePath + "transition/dat/{CN}/archive/".format(CN=caseName)
    cloudObjectPath = homePath + "transition/dat/cloudLabel/filter/"
    irctPath = homePath + "iterativeRainCellTracking/RRtrack/mjo-cwp-1e-1/"
    reIdxLWP = homePath + "transition/dat/reindexLWP/"
    zzPath = homePath + "transition/src/datTXT/"
    lowBaseCloudOutputPath = homePath + "transition/dat/cloudLabel/lowBaseCloud1000/"
    minPerTimeIdx = 10

    dataRetriever = DataRetriever(vvmPath, "mjo_std_mg")
    thData = dataRetriever.getThermoDynamic(0)
    xc, yc, zc = np.array(thData['xc']), np.array(thData['yc']), np.array(thData['zc'])
    zz = np.loadtxt(zzPath + "zz-{}.txt".format(caseName))
    zc3D = np.tile(zc[:, np.newaxis, np.newaxis], reps=(1, len(yc), len(xc)))
    
    rrTrackRetriever = RRtrackRetriever()
    trackOutputData = rrTrackRetriever.readTrackFile(irctPath +  "irt_tracks_output.txt")
    
    trackRecorder = np.arange(1, int(np.max(trackOutputData["trackID"]))+1)

    timeIdxArange = np.arange(initTimeStep, endTimeStep, 1)
    #timeIdxArange = np.arange(400, 401)

    for tIdx in timeIdxArange:
        print("========== {:06d} ==========".format(tIdx))
        cloudObject = np.array(nc.Dataset(cloudObjectPath+"cloudLabel-{:06d}.nc".format(tIdx-1))["cloudLabel"][0])
        lowBaseCloudObject = np.zeros(cloudObject.shape)
        lwpTrackField = np.array(nc.Dataset(reIdxLWP+"lwp-{:06d}.nc".format(tIdx))["lwp"][0])
        lwpTrackIDCandicate = np.unique(lwpTrackField)
        lwpTrackIDCandicate = lwpTrackIDCandicate[lwpTrackIDCandicate > 0]
        if len(lwpTrackIDCandicate) != 0:
            maxLWPIdx = np.max(lwpTrackIDCandicate)

            lowBaseCloudObjectCandicate = np.unique(cloudObject[zc<=1e3, :, :])
            lowBaseCloudObjectCandicate = lowBaseCloudObjectCandicate[lowBaseCloudObjectCandicate!=0]
            if lowBaseCloudObjectCandicate != []:
                maxIndex = np.max(lowBaseCloudObjectCandicate)
                #depLowCloudObjectCandicate = np.zeros(lowBaseCloudObjectCandicate.shape)
                for lowCloudIdx in lowBaseCloudObjectCandicate:
                    print("{} / {} Cloud Filter @ {}".format(lowCloudIdx, maxIndex, tIdx))
                    cloudTop = (cloudObject==lowCloudIdx).any(axis=(1, 2)) * zz[1:]
                    cloudTop = np.max(cloudTop)
                    cloudBase = (cloudObject==lowCloudIdx).any(axis=(1, 2)) * zz[:-1]
                    cloudBase = np.min(cloudBase[cloudBase != 0])
                    if (cloudTop - cloudBase > 2e2): # over than 200 m, from Yi-Chang
                        lowBaseCloudObject[cloudObject==lowCloudIdx] = lowCloudIdx
                        #depLowCloudObjectCandicate[lowBaseCloudObjectCandicate==lowCloudIdx] = lowCloudIdx
                #depLowCloudObjectCandicate = depLowCloudObjectCandicate[depLowCloudObjectCandicate!=0]

        lowBaseCloudObject = lowBaseCloudObject[np.newaxis, :, :, :] # same dim with VVM.
        xrLowBaseCloudObject = xr.DataArray(lowBaseCloudObject.astype(int), 
                                      coords = {'time': np.ones(shape=(1,)), 'zc': zc, 'yc': yc, 'xc': xc}, 
                                      dims = ["time", "zc", "yc", "xc"], 
                                      name = "cloudLabel")
        xrLowBaseCloudObject.to_netcdf(lowBaseCloudOutputPath + "cloudLabel-{:06d}.nc".format(tIdx))



