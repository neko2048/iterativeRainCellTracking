import sys
import numpy as np
import xarray as xr
from netCDF4 import Dataset
from matplotlib.pyplot import *
from scipy.ndimage.measurements import label
from utils import DataRetriever, DataProcessor

def save2NC(xc, yc, zc, tIdx, labeledCloud, outputDir):
    labeledCloud = labeledCloud[np.newaxis, :, :, :] # same dim with VVM.
    xrLabeledCloud = xr.DataArray(labeledCloud, 
                                  coords = {'time': np.ones(shape=(1,)), 'zc': zc, 'yc': yc, 'xc': xc}, 
                                  dims = ["time", "zc", "yc", "xc"], 
                                  name = "cloudLabel")

    xrLabeledCloud.to_netcdf(outputDir + "cloudLabel-{:06d}.nc".format(tIdx))

if __name__ == "__main__":
    caseName = "mjo"
    dataDir = "/home/atmenu10246/transition/dat/{}/archive/".format(caseName)
    datTxtPath = "/home/atmenu10246/transition/src/datTXT/"
    cloudOutputDir = "/data3/atmenu10246/cloudLabel/cloud2LayerOrigin/"
    qcOutputDir = "/data3/atmenu10246/cloudLabel/qc2LayerOrigin/"

    inputConfig = sys.argv
    initTimeStep, endTimeStep = int(inputConfig[1]), int(inputConfig[2])

    qlThres = 1e-4

    dataRetriever = DataRetriever(dataDir, "mjo_std_mg")
    thData = dataRetriever.getThermoDynamic(0)
    xc, yc, zc = np.array(thData['xc']), np.array(thData['yc']), np.array(thData['zc'])
    tIdxArange = np.arange(initTimeStep, endTimeStep, 1) # per 5 hour
    
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


    for t, tIdx in enumerate(tIdxArange):
        print("========== {:06d} ==========".format(tIdx))
        thData = dataRetriever.getThermoDynamic(tIdx)
        qc = np.array(thData['qc'][0])
        qi = np.array(thData['qi'][0])
        isQc = np.array(qc > 0).astype(np.int64)
        isQci = np.array((qc + qi) > qlThres).astype(np.int64)
        isCloud = np.logical_or(isQc, isQci)
        print("label cloud")
        labeledCloud, numCloud = label(isCloud, labelStruct)
        save2NC(xc, yc, zc, tIdx, labeledCloud, cloudOutputDir)

