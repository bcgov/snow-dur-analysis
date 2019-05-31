from qgis.core import *
import os
import numpy as np

# supply path to qgis install location
QgsApplication.setPrefixPath('C:/OSGEO4~1/apps/qgis', True)

# create a reference to the QgsApplication, setting the
# second argument to False disables the GUI
qgs = QgsApplication([], False)

# load providers
qgs.initQgis()

# Write your code here to load some layers, use processing
# algorithms, etc.

annual_dir = 'C:/Users/HGLEASON/Dropbox/Git_Snow_MODIS/Data/MODIS/Raw/Annual_Daily_MD10A1_NDSI_BC'

dirs = os.listdir(annual_dir)

dtype = {'names' : ('STATUS','ELEVATION','LCTN_NM','LCTN_ID' ,'SNWASWS_ID', 'Lon', 'Lat'), 'formats': ('U20','i4','U20','U20','i4','f4','f4')}

ASWS = np.loadtxt('C:/Users/HGLEASON/Dropbox/Git_Snow_MODIS/Data/ASWS/IN/ASWS_STNS.csv', delimiter=",", dtype=dtype, skiprows=1)


stnid_list = []
lat_list = []
lon_list = []


out_file = open('C:/Users/HGLEASON/Dropbox/Git_Snow_MODIS/Data/MODIS/Derived/MD10A1_ASWS_Sampls.csv','w') 

for row in range(len(ASWS)):
    
    stnid_list.append(ASWS[row][3])
    lat_list.append(ASWS[row][6])
    lon_list.append(ASWS[row][5])
    




for dir in dirs:
    
    tile_list = []
    
    tiffs = os.listdir(annual_dir+"/"+dir)
    
    year = str(dir)[11:]
    
    print("Working: ",dir)
    
    for tif in tiffs:
        
        path_to_tif = annual_dir+"/"+dir+"/"+tif
        
        name = str(tif)[:-4]
            
        tile_list.append(QgsRasterLayer(path_to_tif, name))
        
    print("Tiffs Loaded")
    for stn in range(len(stnid_list)):
        
        lat = lat_list[stn]
        lon = lon_list[stn]
        station = stnid_list[stn]
        
        for tile in tile_list:
            
            
            ndsi = tile.dataProvider().identify(QgsPointXY(lon, lat), QgsRaster.IdentifyFormatValue)
               
            line = station+","+year+","+str(lat)+","+str(lon)+","
            
            if ndsi.isValid():
                
                ndsi_dic = ndsi.results()
                
                for day in range(2,367):
                    
                    val = ndsi_dic[day]
                    
                    line = line+str(val)+","
                
                out_file.write(line+" \n")

out_file.close()

# When your script is complete, call exitQgis() to remove the
# provider and layer registries from memory

qgs.exitQgis()