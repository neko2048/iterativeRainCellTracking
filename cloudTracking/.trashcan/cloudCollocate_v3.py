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
    cloudObjectPath = homePath + "transition/dat/cloudLabel/lowBaseCloud/"
    irctPath = homePath + "iterativeRainCellTracking/RRtrack/mjo-cwp-1e-1/"
    reIdxLWP = homePath + "transition/dat/reindexLWP/"
    zzPath = homePath + "transition/src/datTXT/"
    collocateCloudOutputPath = homePath + "transition/dat/collocateCloudMax/"
    indexMapOutput = homePath + "transition/dat/indexMap/"
    minPerTimeIdx = 10

    dataRetriever = DataRetriever(vvmPath, "mjo_std_mg")
    thData = dataRetriever.getThermoDynamic(0)
    xc, yc, zc = np.array(thData['xc']), np.array(thData['yc']), np.array(thData['zc'])
    zz = np.loadtxt(zzPath + "zz-{}.txt".format(caseName))
    zc3D = np.tile(zc[:, np.newaxis, np.newaxis], reps=(1, len(yc), len(xc)))
    zz3D = np.tile(zz[:, np.newaxis, np.newaxis], reps=(1, len(yc), len(xc)))
    rrTrackRetriever = RRtrackRetriever()
    trackOutputData = rrTrackRetriever.readTrackFile(irctPath +  "irt_tracks_output.txt")

    trackRecorder = np.arange(1, int(np.max(trackOutputData["trackID"]))+1)
    timeIdxArange = np.arange(initTimeStep, endTimeStep, 1)
    #timeIdxArange = np.arange(451, 460)

    for tIdx in timeIdxArange:
        print("========== {:06d} ==========".format(tIdx))
        lowBaseCloudObject = np.array(nc.Dataset(cloudObjectPath+"cloudLabel-{:06d}.nc".format(tIdx))["cloudLabel"][0])
        collocateCloudObject = np.zeros(lowBaseCloudObject.shape)
        lwpTrackField = np.array(nc.Dataset(reIdxLWP+"lwp-{:06d}.nc".format(tIdx))["lwp"][0])

        #pcolormesh(xc/1e3, yc/1e3, lowBaseCloudObject.any(axis=0))
        #contour(xc/1e3, yc/1e3, lwpTrackField)
        #savefig("test.jpg")
        lowBaseCloudObjectCandicate = np.unique(lowBaseCloudObject)
        lowBaseCloudObjectCandicate = lowBaseCloudObjectCandicate[lowBaseCloudObjectCandicate!=0]
        if len(lowBaseCloudObjectCandicate) != 0:
            for lowCloudIdx in lowBaseCloudObjectCandicate:
                cloudProjection = (lowBaseCloudObject == lowCloudIdx).any(axis=0)
                incloudLWPidx, incloudLWPcounts = np.unique(cloudProjection * lwpTrackField, return_counts=True)
                incloudLWPcounts = incloudLWPcounts[incloudLWPidx > 0]
                incloudLWPidx = incloudLWPidx[incloudLWPidx > 0]
                #if len(incloudLWPidx) == 0: no correspond cloud
                if len(incloudLWPidx) == 1:
                    print("{} / {}: 1 LWP <-> 1 Cloud Cluster @ {}".format(int(lowCloudIdx), np.max(lowBaseCloudObjectCandicate), tIdx))
                    collocateCloudObject[lowBaseCloudObject == lowCloudIdx] = incloudLWPidx[0]
                elif len(incloudLWPidx) > 1:
                    print("{} / {}: >2 ({}) LWP <-> 1 Cloud Cluster @ {}".format(int(lowCloudIdx), np.max(lowBaseCloudObjectCandicate), len(incloudLWPidx), tIdx))
                    maxCount = np.max(incloudLWPcounts)
                    if np.sum(incloudLWPcounts == maxCount) == 1:
                        collocateCloudObject[lowBaseCloudObject == lowCloudIdx] = incloudLWPidx[incloudLWPcounts == maxCount][0]
                    else:
                        collocateCloudObject[lowBaseCloudObject == lowCloudIdx] = incloudLWPidx[incloudLWPcounts == maxCount][0]

        collocateCloudObject = collocateCloudObject[np.newaxis, :, :, :] # same dim with VVM.
        xrCollocateCloudObject = xr.DataArray(collocateCloudObject.astype(int), 
                                      coords = {'time': np.ones(shape=(1,)), 'zc': zc, 'yc': yc, 'xc': xc}, 
                                      dims = ["time", "zc", "yc", "xc"], 
                                      name = "cloudLabel")
        xrCollocateCloudObject.to_netcdf(collocateCloudOutputPath + "cloudLabel-{:06d}.nc".format(tIdx))



