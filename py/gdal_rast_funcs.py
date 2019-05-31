# -*- coding: utf-8 -*-
"""
The following module is a compilation of GDAL based functions for reading, writing and manipulating geospatial raster data. Environment must have GDAL and Numpy packages available. 
"""

"""
Import required packages
"""
import gdal, ogr, osr, os
import numpy as np
import glob
import sys


####################################################################################################
"""
Method accepts a GDAL raster ('raster') and returns the specified band('bnd_num') as a NumPy array. 
"""
def raster2array(raster, bnd_num):
    #Get specified band 
    band = raster.GetRasterBand(bnd_num)

    #Return as NumPy array
    return band.ReadAsArray()
####################################################################################################

####################################################################################################
"""
Method will merge all GeoTiffs in a directory provided the in directory, out path, path to the 'gdal_merge.py' script, the raster’s no data value, an initialization value, and a glob pattern. (Windows)
"""
def merge_gtiffs(in_dir, out_path, gdal_path, nodata_val, init_val, glob_ptrn):
    
    #Get path to geotiffs with specified glob pattern 
    path = in_dir+glob_ptrn
    
    files_to_mosaic = glob.glob(path)
    
    #For each GeoTiff, edit the paths with '/' seperator 
    for file in range(len(files_to_mosaic)):
        files_to_mosaic[file] = files_to_mosaic[file].replace(os.sep, '/')
    
    #Create a string of files paths to mosiac to pass to the command line 
    files_string = " ".join(files_to_mosaic)
    
    #Final os command as a string
    command = "python "+gdal_path+"gdal_merge.py --config GDAL_CACHEMAX 6000 -v -n "+str(nodata_val)+" -init "+str(init_val)+" -o "+out_path+" -of gtiff -co BIGTIFF=IF_NEEDED "+ files_string

    #Pass the command string to the command line 
    os.system(command)
    
####################################################################################################   

####################################################################################################
"""
Method accepts a GeoTiff as a path ('gtiff_img_pth') and a start band index ('strt_idx') and returns a time-space cube as a 3D NumPy array. !!Input bands are assumes to be ordered chronologically by band number order !! Only bands after the 'strt_idx' will be converted. Band indexing starts at 1.
"""  
def bands_to_np_cube(src_ds, strt_idx):

    #Get number of bands
    bands = src_ds.RasterCount
    
    
    #Get raster dimensions from first band 
    first = raster2array(src_ds, 1)
    rows = len(first)
    cols = len(first[0])
    
    
    bnd_shp = bands-(strt_idx-1)
    
    #Initialize a 3D NumPy array with proper dimensions
    data_cube = np.full((bnd_shp*rows*cols),0).reshape(bnd_shp,rows,cols)
    

    #For each band in the range start_idx -> total bands
    for band in range(strt_idx, bands+1):

        #Convert the band to 2D NumPy array 
        band_as_np = raster2array(src_ds, band)

        #Stack the 2D array into the time cube 
        data_cube[band-strt_idx] = band_as_np
    
    #Return the spatial time series as 3D NumPy array      
    return data_cube

###################################################################################################

###################################################################################################
"""
Method reads in a 2D or 3D Numpy array and translates to a GeoTiff format provided the 'x-y' coordinates of the lower left raster origin, the pixel width and height in map units, the array to translate and the desired output EPSG.
"""
def array2gtiff(newRasterfn,LowrLft_Origin,pixelWidth,pixelHeight,array,EPSG):

    #Update users
    print("Starting: Write "+newRasterfn+" to GeoTiff")

    #Get number of bands
    bands = array.shape[0]

    #Get number of columns and rows
    cols = array.shape[2]
    rows = array.shape[1]

    #Get raster origin 
    originX = LowrLft_Origin[0]
    originY = LowrLft_Origin[1]
    
    #Get a GeoTiff driver 
    driver = gdal.GetDriverByName('GTiff')

    #Initialize a output raster with GeoTransform parameters from input tile and array shape  
    outRaster = driver.Create(newRasterfn, cols, rows, bands, gdal.GDT_Int16)
    outRaster.SetGeoTransform((originX, pixelWidth, 0, originY, 0, pixelHeight))
    outRasterSRS = osr.SpatialReference()
    outRasterSRS.ImportFromEPSG(EPSG)
    outRaster.SetProjection(outRasterSRS.ExportToWkt())
	
	
    #Write each band to the output raster, starting with band 1
    for b in range(1, bands+1):
        #Get the output band @ b
        outband = outRaster.GetRasterBand(b)
        
        #Slice the 3D array at b-1 (2D)
        ary = array[b-1]
        
        #Write the 2D NumPy array to the band 
        outband.WriteArray(ary)
        
        #Flush 
        outband.FlushCache()

    #Update user 
    print("Finished: Write "+newRasterfn+" to GeoTiff")
    
###################################################################################################

###################################################################################################
"""
Function for mapping a function along the 'time-axis' of a 3D Numpy array. (IN PROGRESS)
"""    
def map_timeser_func(np_img_cube):
    
    def avg(ary):
        return np.std(ary)
    
    return np.apply_along_axis(avg,0,np_img_cube)
###################################################################################################   

###################################################################################################  
"""
Uses gdal geomatrix (gdal.GetGeoTransform()) info to calculate
the pixel location of a geospatial coordinate.
"""   
def world2Pixel(geoMatrix, x, y):
    #Get Transform details 
  ulX = geoMatrix[0]
  ulY = geoMatrix[3]
  xDist = geoMatrix[1]
  yDist = geoMatrix[5]
  
  #Get array indexes from trasform calcs 
  pixel = int((x - ulX) / xDist)
  line = int((ulY - y) / yDist)
  
  #Return the numpy index's at the x-y location 
  return (pixel, line)
################################################################################################### 
  
    
