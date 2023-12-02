import sys
import numpy as np
import xarray as xr
import netCDF4 as nc
from scipy import ndimage
from matplotlib.pyplot import *
from utils import DataProcessor, DataRetriever, RRtrackRetriever

class CloudIdxCollocator:
    def __init__(self, deltaX, deltaY):
        self.deltaX = deltaX
        self.deltaY = deltaY

    def projectIndex(self, cloudObject, lwpTrackField, lwpTrackMetaData, findGridSize=2):
        """
        INPUT: 
            cloudObject: (z, y, x)
            lwpTrackField: (y, x)
            lwpTrackMetaData: pandas DataFrame
        OUTPUT:
            cloudObject: (z, y, x)
        """
        return None

    def syncIndex(self):
        pass


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
    collocateCloudOutputPath = homePath + "transition/dat/collocateCloudTest2/"
    #collocateCloudFinalOutputPath = homePath + "transition/dat/collocateCloudFinalTest2/"
    indexMapOutput = homePath + "transition/dat/indexMap/"
    minPerTimeIdx = 10

    dataRetriever = DataRetriever(vvmPath, "mjo_std_mg")
    thData = dataRetriever.getThermoDynamic(0)
    xc, yc, zc = np.array(thData['xc']), np.array(thData['yc']), np.array(thData['zc'])
    zz = np.loadtxt(zzPath + "zz-{}.txt".format(caseName))
    zc3D = np.tile(zc[:, np.newaxis, np.newaxis], reps=(1, len(yc), len(xc)))

    rrTrackRetriever = RRtrackRetriever()
    trackOutputData = rrTrackRetriever.readTrackFile(irctPath +  "irt_tracks_output.txt")


    #timeIdxArange = np.arange(400, 410)
    timeIdxArange = np.arange(initTimeStep, endTimeStep, 1)

    for tIdx in timeIdxArange:
        print("========== {:06d} ==========".format(tIdx))
        cloudBaseDict = {}
        cloudObject = np.array(nc.Dataset(cloudObjectPath+"cloudLabel-{:06d}.nc".format(tIdx-1))["cloudLabel"][0])
        lowBaseCloudObject = np.zeros(cloudObject.shape)
        collocateCloudObject = np.zeros(cloudObject.shape)
        lwpTrackField = np.array(nc.Dataset(reIdxLWP+"lwp-{:06d}.nc".format(tIdx))["lwp"][0])
        lwpTrackIDCandicate = np.unique(lwpTrackField)
        lwpTrackIDCandicate = lwpTrackIDCandicate[lwpTrackIDCandicate > 0]
        maxLWPIdx = np.max(lwpTrackIDCandicate)

        lowBaseCloudObjectCandicate = np.unique(cloudObject)#[zc<=2.5e3, :, :])
        lowBaseCloudObjectCandicate = lowBaseCloudObjectCandicate[lowBaseCloudObjectCandicate!=0]
        if lowBaseCloudObjectCandicate != []:
            maxIndex = np.max(lowBaseCloudObjectCandicate)
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

            #for lwpIdx in lwpTrackIDCandicate:
            #    print("check LWP {} / {}".format(lwpIdx, maxLWPIdx))
            #    selectColumn = np.tile((lwpTrackField==lwpIdx)[np.newaxis, :, :], reps=(len(zc), 1, 1))
            #    selectCloud = np.unique(selectColumn * lowBaseCloudObject)
            #    selectCloud = selectCloud[selectCloud > 0]
            #    for cloudIdx in selectCloud:
            #        cloudZc = (lowBaseCloudObject == cloudIdx).any(axis=(1, 2)) * zc
            #        cloudBaseInColumn = np.min(cloudZc[cloudZc > 0])
            #        if cloudBaseInColumn < 2.5e3:
            #            print("cloud {} base in selected column is below 2500 m -> {}".format(cloudIdx, lwpIdx))
            #            collocateCloudObject[lowBaseCloudObject == cloudIdx] = lwpIdx


        collocateCloudObject = collocateCloudObject[np.newaxis, :, :, :] # same dim with VVM.
        xrCollocateCloudObject = xr.DataArray(collocateCloudObject.astype(int), 
                                      coords = {'time': np.ones(shape=(1,)), 'zc': zc, 'yc': yc, 'xc': xc}, 
                                      dims = ["time", "zc", "yc", "xc"], 
                                      name = "cloudLabel")
        xrCollocateCloudObject.to_netcdf(collocateCloudOutputPath + "cloudLabel-{:06d}.nc".format(tIdx))


