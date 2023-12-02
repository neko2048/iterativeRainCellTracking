import sys
import numpy as np
from netCDF4 import Dataset
from matplotlib.pyplot import *
from utils import DataRetriever, DataProcessor


if __name__ == "__main__":
    caseName = "mjo"
    dataDir = "/data3/atmenu10246/cloudLabel/qc000000InCloud/"
    inputConfig = sys.argv
    initTimeStep, endTimeStep = int(inputConfig[1]), int(inputConfig[2])

    tIdxArange = np.arange(initTimeStep, endTimeStep, 1) # per 5 hour

    for tIdx in tIdxArange:
        print("========== {:06d} ==========".format(tIdx))
        data = np.array(Dataset(dataDir + "cloudLabel-{:06d}.nc".format(tIdx))["cloudLabel"][0])
        data = np.max(data, axis=0)
        pcolormesh(np.ma.masked_array(data, data==0))
        savefig("test-{:06d}.jpg".format(tIdx))
        clf()
