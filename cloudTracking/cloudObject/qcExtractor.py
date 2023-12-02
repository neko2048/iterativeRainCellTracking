import sys
import numpy as np
import xarray as xr
import netCDF4 as nc
from scipy.ndimage.measurements import label
from utils import DataProcessor, DataRetriever, RRtrackRetriever


if __name__ == "__main__":
    inputConfig = sys.argv
    initTimeStep, endTimeStep = int(inputConfig[1]), int(inputConfig[2])
    caseName = "mjo"
    homePath = "/home/atmenu10246/"
    vvmPath = homePath + "transition/dat/{CN}/archive/".format(CN=caseName)
    cloudObjectPath = homePath + "transition/dat/cloudLabel/cloud2LayerFilter/"
    filterQcOutputPath = homePath + "transition/dat/cloudLabel/qc2LayerFilter/"
    filterQc2D_OutputPath = homePath + "transition/dat/cloudLabel/qc2LayerFilter2D/"
    minPerTimeIdx = 10

    dataRetriever = DataRetriever(vvmPath, "mjo_std_mg")
    thData = dataRetriever.getThermoDynamic(0)
    xc, yc, zc = np.array(thData['xc']), np.array(thData['yc']), np.array(thData['zc'])
    #zz = np.load(zzPath + "zz-{}.txt".format(caseName))
    #zc3D = np.tile(zc[:, np.newaxis, np.newaxis], reps=(1, len(yc), len(xc)))
    labelStruct = np.array(
       [[[0, 0, 0],
         [0, 1, 0],
         [0, 0, 0]],

        [[0, 1, 0],
         [1, 1, 1],
         [0, 1, 0]],

         [[0, 0, 0],
          [0, 1, 0],
          [0, 0, 0]]], dtype='uint8')
    timeIdxArange = np.arange(initTimeStep, endTimeStep, 1)
    for tIdx in timeIdxArange:
        print("========== {:06d} ==========".format(tIdx))
        qc = np.array(dataRetriever.getThermoDynamic(tIdx)["qc"][0])
        cloudObject = np.array(nc.Dataset(cloudObjectPath+"cloudLabel-{:06d}.nc".format(tIdx))["cloudLabel"][0])
        isQc = np.logical_and(cloudObject > 0, qc > 0)
        labeledQc, _ = label(isQc, labelStruct)
        isQc2D = np.any(isQc>0, axis=0)
        saveIsQc = labeledQc[np.newaxis, :, :, :] # same dim with VVM.
        xrSaveIsQc = xr.DataArray(saveIsQc.astype(int), 
                                  coords = {'time': np.ones(shape=(1,)), 'zc': zc, 'yc': yc, 'xc': xc}, 
                                  dims = ["time", "zc", "yc", "xc"], 
                                  name = "cloudLabel")
        xrSaveIsQc.to_netcdf(filterQcOutputPath + "cloudLabel-{:06d}.nc".format(tIdx))

        print("save 2d")
        saveIsQc2D = isQc2D[np.newaxis, :, :] # same dim with VVM.
        print(np.unique(saveIsQc2D))
        xrSaveIsQc2D = xr.DataArray(saveIsQc2D.astype(int), 
                                  coords = {'time': np.ones(shape=(1,)), 'yc': yc, 'xc': xc}, 
                                  dims = ["time", "yc", "xc"], 
                                  name = "cloudLabel")
        xrSaveIsQc2D.to_netcdf(filterQc2D_OutputPath + "cloudLabel-{:06d}.nc".format(tIdx))



