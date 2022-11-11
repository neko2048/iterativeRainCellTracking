import netCDF4 as nc
import numpy as np
import os

class ParamRetriever:
    def __init__(self, dataDir):
        self.dataDir = dataDir
        self.data = nc.Dataset(self.dataDir)

    def printDomainSize(self):
        print("domainsize_x = {}".format(len(self.data["xc"])))
        print("domainsize_y = {}".format(len(self.data["yc"])))
        print("! domainsize_z = {}".format(len(self.data["zc"])))
    def printlLonLatGrid(self):
        try:
            self.data["lon"]
            print("llonlatgrid = .TRUE. ! suggest .FALSE. when using VVM")
        except IndexError:
            print("llonlatgrid = .FALSE.")

    def printUnitArea(self):
        x0, x1 = self.data["xc"][0], self.data["xc"][1]
        y0, y1 = self.data["yc"][0], self.data["yc"][1]
        xyArea = (x1 - x0) * (y1 - y0) / 1e6 # km ^ 2
        print("unit_area = {}".format(xyArea))

    def printTimeSteps(self):
        dirPath = "/".join(self.dataDir.split("/")[:-1])
        files = [entry for entry in os.listdir(dirPath) if os.path.isfile(os.path.join(dirPath, entry))]
        print("time_steps = {}".format(len([file for file in files if "Thermodynamic" in file])))

    def printLatFirst(self):
        print("lat_first = {}".format(np.min(self.data["lat"])))

    def printLatIncAndLonInc(self):
        try: 
            print("lat_inc = {}".format((self.data["lat"][1:] - self.data["lat"][:-1])[:3]))
            print("lon_inc = {}".format((self.data["lon"][1:] - self.data["lon"][:-1])[:3]))
        except IndexError:
            print("lat_inc and lon_inc: NO LAT/LON info.")

    def printParams(self):
        self.printDomainSize()
        self.printlLonLatGrid()
        self.printUnitArea()
        self.printTimeSteps()
        self.printLatFirst()
        self.printLatIncAndLonInc()

if __name__ == "__main__":
    #dataDir = "/home/atmenu10246/transition/dat/diurnal_prescribed/archive/exp.L.Thermodynamic-000000.nc"
    dataDir = "/home/atmenu10246/transition/dat/mjo/archive/mjo_std_mg.L.Thermodynamic-000000.nc"
    paramRetriever = ParamRetriever(dataDir)
    paramRetriever.printParams()
