import numpy as np
from netCDF4 import Dataset
from matplotlib.pyplot import *
import sys
from utils import DataRetriever, DataProcessor
from scipy.ndimage.measurements import label

if __name__ == "__main__":
    inputConfig = sys.argv
    initTimeStep, endTimeStep = int(inputConfig[1]), int(inputConfig[2])
    caseName = "mjo"
    dataDir = "/home/atmenu10246/transition/dat/{}/archive/".format(caseName)
    pBar = np.loadtxt("./datTXT/pBar-{}.txt".format(caseName))

    qlThres = 1e-6
    fontsize = 20
    titleSize = 30
    minPerTimeIdx = 10

    dataRetriever = DataRetriever(dataDir, "mjo_std_mg")
    thData = dataRetriever.getThermoDynamic(0)
    xc, yc, zc = np.array(thData['xc']), np.array(thData['yc']), np.array(thData['zc'])
    zc3D = np.tile(zc[:, np.newaxis, np.newaxis], reps=(1, len(yc), len(xc)))
    dataProcessor = DataProcessor(xc, yc, zc)

    tIdxArange = np.arange(initTimeStep, endTimeStep, 1) # per 5 hour

    figure(figsize=(15, 12))
    for t, tIdx in enumerate(tIdxArange):
        print("========== {:06d} ==========".format(tIdx))
        cloudLabel = np.load("cloudLabel/cloudLabel-{:06d}.npy".format(tIdx))
        cloudLabel = cloudLabel.astype(bool).astype(np.int64)
        cloudZc = zc3D * cloudLabel
        cloudZc = np.ma.masked_array(cloudZc, cloudZc == 0.)
        #cloudLevel = np.zeros(shape=(len(yc), len(xc)))
        cloudLevel = np.nanmin(cloudZc, axis=0)

        pcolormesh(xc, yc, cloudLevel)
        colorbar()
        title("Time: {:04d} hr {:02d} min".format(tIdx*minPerTimeIdx//60, tIdx*minPerTimeIdx%60), fontsize=20, loc="right")
        savefig("cloudLevel-{:06d}.jpg".format(tIdx), dpi=300)
        clf()



