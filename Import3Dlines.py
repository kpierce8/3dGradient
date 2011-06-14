import re
from UserDict import UserDict

lines = open(u"F:\\PASSPORT\\IMW\\seabeckHGeo\\pf.txt")

linesfile = lines.read()

lines_ind = re.split('END\n',linesfile)

           
class PolylineProfile2(UserDict):
    "store single reach raster profile"
    def __init__(self, raster=None):
        UserDict.__init__(self)
        self["rasterType"] = raster
        self.id = []
        self.xycoor = []
        self.ind = 0
    def ingestPolylines(self, linesCollection):       
        for polyline in linesCollection:
            print polyline[0:50]
            points = re.split('\n',polyline)
            print points[0]
            self.id = points[0]
            self.ind = 0
            for point in points[1:]:
                self.ind += 1
                self.xycoor.append([self.id,self.ind,re.split('\s',point)])
                

hopy = PolylineProfile2('Elevation')
hopy.ingestPolylines(lines_ind)

outstring = []
for iline in hopy.xycoor:
    outstring.append(re.sub('(\[)|(\])|(\')','',str(iline))+'\n')

output = open("F:\\PASSPORT\\IMW\\seabeckHGeo\\output.txt", 'w')
output.writelines(outstring)

output.close()
