import numpy as np
from netCDF4 import Dataset
from matplotlib.pyplot import *

if __name__ == "__main__":
    tIdx = 460
    originData = Dataset("/data3/atmenu10246/cloudLabel/ncFormat/cloudLabel-{:06d}.nc".format(tIdx))
    data = Dataset("/data3/atmenu10246/cloudLabel/filter/cloudLabel-{:06d}.nc".format(tIdx))
    objectData = Dataset("/home/atmenu10246/iterativeRainCellTracking/RRtrack/mjo-cwp-1e-1/irt_objects_mask_mjo_.nc")["var1"][tIdx+1, 0, :, :]
    xc = np.array(data['xc'])
    yc = np.array(data['yc'])

    oriCloudLabel = np.array(originData['cloudLabel'][0])
    oriCloudLabel = np.ma.masked_array(oriCloudLabel ,oriCloudLabel==0)
    cloudLabel = np.array(data['cloudLabel'][0])
    cloudLabel = np.ma.masked_array(cloudLabel ,cloudLabel==0)
    pcolormesh(xc/1e3, yc/1e3, np.max(cloudLabel, axis=0), cmap='turbo')
    contour(xc/1e3, yc/1e3, (objectData>0), colors='red', linewidth=1)
    savefig("test2.jpg", dpi=300)
