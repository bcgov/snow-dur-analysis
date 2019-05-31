import os
import gdal_rast_funcs as grast

working_dir = "C:/Users/hgleason/Dropbox/Git_Snow_MODIS/Data/MODIS/Raw/Annual_Daily_MD10A1_NDSI_BC_np"
out_dir = "C:/Users/hgleason/Dropbox/Git_Snow_MODIS/Data/MODIS/Raw/Annual_Snow_Metrics"
gdal_pth = "C:/Users/hgleason/bin/"

dirs = os.listdir(working_dir)

for dir in dirs:

    path = working_dir+"/"+dir+"/"

    year = dir[10:]

    out_path = out_dir+"/MD10A1_SD"+year+".tif"

    grast.merge_gtiffs(path, out_path, gdal_pth, -9, -9)



    

    
