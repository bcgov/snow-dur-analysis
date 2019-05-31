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
import glob
import statsmodels.api as sm
from multiprocessing import Pool
import time
import gdal_rast_funcs as grast



###################################################################################################
"""
Method for converting a NDSI time series to snow on and off dates using a LOWESS interpolation of
the time series. Inputs include the NDSI time series 'ndsi_ser' which is assumed to have missing
data represented by the number '-9'.The fraction ('frac'), ndsi snow start threshold ('strt_ndsi')
and ndsi snow end threshold ('end_ndsi') parameters must also be specified. Returns a list as
[SDon,SDoff,SDdur,Obs] for the provided M*D10A1 time series.
"""    
def ndsi_lowess_interp(ndsi_ser, frac,strt_ndsi,end_ndsi, min_obs):

    #Initialize a NumPy array representing days after 1-Sep
    day_ser = np.arange(len(ndsi_ser))


    #Filter missing (-9) data from the time series     
    y_filt = ndsi_ser[ndsi_ser>=0]
    x_filt = day_ser[ndsi_ser>=0]
    
    #Initialize instance of statsmodels LOWESS
    lowess = sm.nonparametric.lowess
    
    #Fit a default LOWESS smoother to the filtered NDSI data using specified bandwidth (frac)
    yout = lowess(y_filt,x_filt, frac= frac, is_sorted=True, return_sorted=False, it=1, delta=3)
    
    
    #Initialize continuous snow list [SDon,SDoff,SDdur,Obs]
    cont_snow_list = [-9,-9,0,-9]

    #Get the total number of non-missing observations 
    obs = len(ndsi_ser[ndsi_ser>-9])

    #Check that there are a suitable number of observations 
    if obs > min_obs:
        
        #Initialize a start day var
        start_day = -9;

        #Initialize a end day var
        end_day = -9;

        #Initialize a max duration var
        max_dur = 0;
        
        
        #Initialize a boolean continuous snow period start indicator 
        start_detected = False;

        #For each day in the smoothed NDSI series 
        for day in range(len(x_filt)):

            #Get the LOWESS smoothed NDSI value @ day
            ndsi_val = yout[day]

            #If the NDSI is greater than the start threshold and as not been detected and in not day 0
            if ndsi_val>=strt_ndsi and start_detected == False and day>0:

                #Update boolean start indicator 
                start_detected = True

                #Linearly interpolate start day @ start_ndsi threshold
                x_cur = x_filt[day]
                x_prior = x_filt[day-1]
                y_cur = ndsi_val
                y_prior = yout[day-1]
                slope = (y_cur - y_prior)/(x_cur - x_prior)
                beta = y_cur - (slope*x_cur)

                #Update start_day var 
                start_day = round((strt_ndsi-beta)/slope)

            #Check if there is snow cover on 1-Sep start, don't interp
            if day==0 and ndsi_val>=strt_ndsi:

                #Update start_day var 
                start_day = x_filt[day]

                #Update start indicator 
                start_detected = True

            #If NDSI is less then end NDSI threshold and start is true and not day 365
            if ndsi_val<end_ndsi and start_detected==True and day!=(len(x_filt)-1):

                #Update boolean start indicator 
                start_detected = False

                #Linearly interpolate end day @ end_ndsi threshold
                x_cur = x_filt[day]
                x_prior = x_filt[day-1]
                y_cur = ndsi_val
                y_prior = yout[day-1]
                slope = (y_cur - y_prior)/(x_cur - x_prior)
                beta = y_cur - (slope*x_cur)

                #Update end_day var
                end_day = round((end_ndsi-beta)/slope)

            #Check if last day of time series, don't interp
            if day==(len(x_filt)-1) and start_detected==True:

                end_day = x_filt[day]

            #Calculate the duration of the continuous snow period 
            temp_dur = round(end_day-start_day)

            #If the new snow covered period is longer than the longest observed period (or only snow covered period) 
            if temp_dur>max_dur:

                #Re-define the return snow metrics 
                cont_snow_list = [start_day,end_day,temp_dur,obs]

                #Update max duration var
                max_dur = temp_dur

	#Return snow metrics as list 
    return cont_snow_list
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
def process_tile(tile_pth,tile_dir,tile_name, strt_idx, frac, ndsi_strt, ndsi_end ,min_obs, EPSG):

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
    tile_name = tile_name[:-4]

    #Establish output path from directory and current tile 
    RastOut = tile_dir+"/"+tile_name+"_out.tif"

    #Commit the GeoTiff to memory as 3D NumPy array 
    time_cube = grast.bands_to_np_cube(tile_pth,strt_idx)

    #Get data dimensions 
    rows = len(time_cube[0])
    cols = len(time_cube[0][0])

    #Initialize output NumPy array with input dimensions and 4 bands 
    snow_stack = np.arange(rows*cols*4).reshape(4,rows,cols)

    #Progress keeper 
    qrt = rows*0.1
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
            ndsi_vect = time_cube[:,row,col]

	    #Check that all data is not missing (-9) or all (0) 
            if np.sum(ndsi_vect>0)>0:

                #Get snow duration metrics 
                metrics = ndsi_lowess_interp(ndsi_vect,frac,ndsi_strt,ndsi_end,min_obs)

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
def main(dir_pth, strt_idx,frac,ndsi_strt,ndsi_end,min_obs,EPSG, pool_num):

    tile_dirs = os.listdir(dir_pth)

    for d in tile_dirs:
    
        #List GeoTiff in target directory 
        gtiffs = os.listdir(dir_pth+"/"+d)

        #A parameter variable
        params = []

        #For each tiff in the directory
        for t in gtiffs:

            #Get the absolute path 
            pth = dir_pth+"/"+d+"/"+t

            #Establish parameters as Dict
            param = {'tile_pth':pth, 'tile_dir':dir_pth+"/"+d,'tile_name': t, 'strt_idx':strt_idx , 'frac': frac, 'ndsi_strt':ndsi_strt, 'ndsi_end':ndsi_end,'min_obs':min_obs ,'EPSG': EPSG}

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
    dir_pth = "C:/Users/HGLEASON/Dropbox/Git_Snow_MODIS/snow-dur-analysis/py/2004_test"

    #Index stating which band marks the begining of the time series 
    strt_idx = 2

    #Specifies the smoothness of the LOWESS interpolation 
    frac = 0.2

    #NDSI threshold to classify onset of continious snow cover
    ndsi_strt = 15

    #NDSI threshold to classify termination of continious snow cover
    ndsi_end = 10

    #Minimum number of NDSI observations to merit derving snow metrics 
    min_obs = 0

    #Desired output projection (EPSG) code 
    EPSG = 4326

    #Number of workers to use 
    pool_num = 8

    print("Script Started @: ",time.time())
    #Calling main method 
    main(dir_pth, strt_idx,frac,ndsi_strt,ndsi_end,min_obs,EPSG, pool_num)
    print("Script Ended @: ",time.time())

 ###################################################################################################   


                    
