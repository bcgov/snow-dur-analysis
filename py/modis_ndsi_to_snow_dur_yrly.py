# -*- coding: utf-8 -*-
"""
Created on Wed Apr 24 09:27:05 2019

@Author: Hunter Gleason & Alex Bevington
@Organization:FLNRORD
@Location: Prince George, BC
@Contact: Alexandre.Bevington@gov.bc.ca
@Project:MODIS Snow Duration Over BC 

The following script carries out computation of snow duration metrics including the start, end and
duration of the longest continuous snow covered period derived from an image time series of NDSI
values from the MODIS M*D10A1 archives. See associated Earth Engine (EE) script for the source of
these data. Snow duration is computed on a pixel by pixel basis. At each pixel location for a given
snow year defined as 1-Sep to 1-Sep, a NDSI time series in extracted from the M*D10A1 images output
from the EE script. Using the NDSI time series, a LOWESS trend is fit to the series and used to
interpolate missing NDSI data. From the smoothed NDSI time series the start and end of the longest
snow covered period is determined by asserting a start NDSI threshold and a end NDSI threshold.
The algorithm outputs for each input tile a GeoTiff with the same extent with four bands representing
the snow start (in days after 1-Sep), end and duration with an additional band representing the total
number of observations that were present in the time series. 

"""


import gdal, ogr, osr, os
import numpy as np
import statsmodels.api as sm
from multiprocessing import Pool
import time
import gdal_rast_funcs as grast
from scipy.interpolate import interp1d


###################################################################################################
"""
Method for converting a NDSI time series to snow on and off dates using a LOWESS interpolation of
the time series. Inputs include the NDSI time series 'ndsi_ser' which is assumed to have missing
data represented by the number '-9'.The fraction ('frac'), ndsi snow start threshold ('strt_ndsi')
and ndsi snow end threshold ('end_ndsi') parameters must also be specified. Returns a list as
[SDon,SDoff,SDdur,Obs] for the provided M*D10A1 time series.
"""    
def ndsi_lowess_interp(ndsi_ser, frac,ndsi_thresh, it,dlta):

    #Initialize a NumPy array representing days after 1-Sep
    day_ser = np.arange(len(ndsi_ser))


    #Filter missing (-9) data from the time series     
    y_filt = ndsi_ser[ndsi_ser>=0]
    x_filt = day_ser[ndsi_ser>=0]
    
    
    #Initialize continuous snow list [SDon,SDoff,SDdur,Obs]
    cont_snow_list = [-9,-9,0,-9]

    #Get the total number of non-missing observations 
    obs = len(y_filt)
    
    #Initialize instance of statsmodels LOWESS
    lowess = sm.nonparametric.lowess
    
    #Fit a default LOWESS smoother to the filtered NDSI data using specified bandwidth (frac)
    yout = lowess(y_filt,x_filt, frac= frac, is_sorted=True, it=it, delta = dlta, return_sorted=False)


    frst_dy = x_filt[0]
    lst_dy = x_filt[len(x_filt)-1]


    try:
        spline = interp1d(x_filt, yout, kind='cubic')
        
        xnew = np.arange(frst_dy,lst_dy, 1)
        ynew = spline(xnew)
        
        condition = ynew>=ndsi_thresh
        
        x_xtrct = np.extract(condition, xnew)
        
        stepsize = 1
        
        x_split = np.split(x_xtrct, np.where(np.diff(x_xtrct) != stepsize)[0]+1)
        
        if len(x_split) > 0:
        
            max_lng = 0
            max_idx = 0
            
            for i in range(len(x_split)):
                
                if len(x_split[i])>max_lng:
                    max_idx = i
                    max_lng = len(x_split[i])
            
            period = x_split[max_idx]     
            
            if period.size != 0:
            
                start = period[0]
                
                end = period[len(period)-1]
                
                dur = end-start
                
                return [start, end, dur, obs]
            
            else:
                
                return cont_snow_list
            
            
    except:
        
        return [-9,-9,-9,obs]
###################################################################################################

###################################################################################################
"""
Method accepts a tile path to a GeoTiff (See EE Script) representing a daily M*D10A1 derived NDSI
time series with each band representing a new day. Missing data is assumed to be represented by
the number '-9'. Method also requires the tile directory as a string. A start index ('strt_idx')
must be provided to indicate the first band with data representing NDSI on 1-Sep. Each following band
is assumed to be ordered chronologically. The method applies a LOWESS interpolation to each pixel
time series to derive the first and last day of the longest continuous snow covered period. Snow
presence is defined by both start ('ndsi_strt') and end ('ndsi_end') thresholds. The minimum number
of observations ('min_obs') must be specified to derive snow duration metrics. Finally, the start,
end, duration and number of observations are exported as a GeoTIff corresponding to the input
coverage. The 'ESPG' code of the output GeoTiff must be specified. GeoTiffs are output
to the 'tile_dir' path. 
"""
def process_tile(dir_pth, strt_idx, frac, thresh,it,dlta, EPSG):
    
    
    tile_dir = os.listdir(dir_pth)


    for t in tile_dir:


        tile_pth = dir_pth+"/"+t

        #Update user
        print("Starting: Process "+tile_pth+" @ "+str(time.time()))

        
        #Open the GeoTiff with gdal
        data = gdal.Open(tile_pth)
    
        #Get the geo-transformation 
        GeoTran = data.GetGeoTransform()
    
        #Get origin of raster
        RastOrigin = (GeoTran[0], GeoTran[3])
    
        #Get pixel dimensions 
        PixWidth = GeoTran[1]
        PixHeight = GeoTran[5]
    
    
        #Get name of fist tile without extension 
        tile_name = t[:-4]
    
        #Establish output path from directory and current tile 
        RastOut = dir_pth+"/"+tile_name+"_out.tif"
    
    
        print("Converting "+t+"to Numpy")
        #Commit the GeoTiff to memory as 3D NumPy array 
        time_cube = grast.bands_to_np_cube(data,strt_idx)
        print("Finished "+t+"to Numpy")
    
        #Get data dimensions 
        rows = len(time_cube[0])
        cols = len(time_cube[0][0])
    
        #Initialize output NumPy array with input dimensions and 4 bands 
        snow_stack = np.full((rows*cols*4),0).reshape(4,rows,cols)
    
    
        #Progress keeper 
        qrt = rows*0.25
        thsh = qrt
    
        #For each row
        for row in range(rows):
    	#Update user on progress 
            if row>=thsh:
                print("Finished "+str(round((float(row)/float(rows))*100))+"% of "+tile_pth)
                thsh = thsh+qrt
            #For each col 
            for col in range(cols):
    
    	    #Get the NDSI time series from the 'time_cube' @ row and col 
                ndsi_vect = time_cube[0:,row,col]
    
    	    #Check that all data is not missing (-9) or all (0) 
                if np.sum(ndsi_vect>0)>0:
    
                    #Get snow duration metrics 
                    metrics = ndsi_lowess_interp(ndsi_vect,frac,thresh,it,dlta)
    
    		#Update each array with snow metric results 
    
                    #Snow Start Band
                    snow_stack[0,row,col] = metrics[0]
    
                    #Snow End Band 
                    snow_stack[1,row,col] = metrics[1]
    
                    #Snow Dur Band
                    snow_stack[2,row,col] = metrics[2]
    
                    #Snow Obs Band 
                    snow_stack[3,row,col] = metrics[3]
    
    
                else:
    
                    #Populate arrays with missing data value 
                    snow_stack[0,row,col] = -9
                    snow_stack[1,row,col] = -9
                    snow_stack[2,row,col] = -9
                    snow_stack[3,row,col] = -9
    
    
        #Convert the 3D NumPy annual stack array to mulit-band GeoTiff 
        grast.array2gtiff(RastOut,RastOrigin,PixWidth,PixHeight,snow_stack, EPSG)
    
        #Update user 
        print("Finished: Process "+tile_pth+" @ "+str(time.time()))
###################################################################################################

###################################################################################################
"""
auxiliary function to make it work
"""
def process_helper(args):
	return process_tile(**args)
###################################################################################################

###################################################################################################
"""
Main method for implementing the 'process_tile' method. Provided the path to the directory holding
the output Earth Engine M*D10A1 NDSI tiles, this method uses a parallel pool to implement the
'process_tile' method on each tile (GeoTiff) present in the directory. 
"""
def main(top_dir_pth, strt_idx,frac,thresh,it,dlta,EPSG, pool_num):

    tile_dirs = os.listdir(top_dir_pth)

    params = []

    for d in tile_dirs:

        #Get the absolute path 
        pth = top_dir_pth+"/"+d

        #Establish parameters as Dict
        param = {'dir_pth':pth, 'strt_idx':strt_idx , 'frac': frac, 'thresh':thresh, 'it':it,'dlta':dlta ,'EPSG': EPSG}

        #Append to params
        params.append(param)

    #Set pool limit
    pool = Pool(pool_num)

    #Asynchronously apply the 'process_tiles' method
    p = pool.map_async(process_helper, params)
    p.get()



if __name__ == '__main__':

    """
    Specify variable values below:
    """

    #Path to directory with output Earth Engine NDSI tile directories 
    top_dir_pth = 'C:/Users/hgleason/Dropbox/Git_Snow_MODIS/Data/MODIS/Raw/Dummy_Dir'

    #Index stating which band marks the begining of the time series 
    strt_idx = 2

    #Specifies the smoothness of the LOWESS interpolation 
    frac = 0.2

    thresh = 30
    
    it = 1
    
    dlta = 3

    #Minimum number of NDSI observations to merit derving snow metrics 
    min_obs = 0

    #Desired output projection (EPSG) code 
    EPSG = 4326

    #Number of workers to use 
    pool_num = 8

    print("Script Started @: ",time.time())
    #Calling main method 
    main(top_dir_pth, strt_idx,frac,thresh,it,dlta,EPSG, pool_num)
    print("Script Ended @: ",time.time())

 ###################################################################################################   


                    
