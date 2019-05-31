# -*- coding: utf-8 -*-
"""
Created on Tue May 14 10:03:40 2019

@author: hgleason


This method is specific to the Confluence manuscript. Using random lat-lon locations found in the 'EE_Random_Samples.csv', the snow duration metrics are sampled from the rasters present in the 'Annual_Snow_Metrics directory. The code then outputs the associated snow metrics annually for each unique random sample.

"""


#Import the required packages
import numpy as np
import gdal
import gdal_rast_funcs as grast
import glob

#Load the earth engine derived Random Sample locations CSV. 
samples = np.loadtxt(fname = "C:/Users/hgleason/Dropbox/Git_Snow_MODIS/Data/EE_Random_Sample.csv", dtype = "U10",delimiter=',',skiprows=1)

wrk_dir = "C:\\Users\\hgleason\\Dropbox\\Git_Snow_MODIS\\Data\\MODIS\\Derived\\Annual_Snow_Metrics\\"

path = wrk_dir+"*_clp.tif"
    
files = glob.glob(path)

#Path to output csv 
out = open("C:/Users/hgleason/Dropbox/Git_Snow_MODIS/Data/MODIS/Derived/SD_Random_Sampls.csv","w") 

#For each annual snow metric GeoTiff
for tile in files:
    
    # open dataset
    ds = gdal.Open(tile)
    
    #Get the Geotransform
    geoTrans = ds.GetGeoTransform()
    
    #Convert the GeoTiff to Numpy Array 
    img_cube = grast.bands_to_np_cube(ds,1)
    
    #Get the current year from image path string 
    year = str(tile)[90:94]
    
    print(year,str(tile))
    
    #For each uniqe EE sample 
    for samp in range(len(samples)):
        
        #Get sample details 
        uid = samples[samp,2]
        
        lat = samples[samp,0]
        
        lon = samples[samp,1]
        
        #Sample the annual raster at the UID lat-lon
        smpl_idxs = grast.world2Pixel(geoTrans, float(lon), float(lat))
        snow_mets = img_cube[0:,smpl_idxs[1],smpl_idxs[0]]
        
        sdon = snow_mets[0]
        sdoff = snow_mets[1]
        sddur = snow_mets[2]
        obs = snow_mets[3]
        
        #Print sample details to out_csv 
        line = uid+","+year+","+str(lat)+","+str(lon)+","+str(sdon)+","+str(sdoff)+","+str(sddur)+","+str(obs)+"\n"
        
        out.write(line)


out.close()