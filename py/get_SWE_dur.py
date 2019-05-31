# -*- coding: utf-8 -*-
"""
Created on Wed Mar 27 10:32:12 2019

@author: HGLEASON
"""

import numpy as np
import datetime

swe_data_pth = "C:/Users/hgleason/Dropbox/Git_Snow_MODIS/Data/ASWS/IN/Input_SWE.csv"

swe_out = open("C:/Users/hgleason/Dropbox/Git_Snow_MODIS/Data/ASWS/OUT/Snow_Dur_from_SWE.csv","w") 



def stamp_to_date(stamp):
            year = int(str(stamp)[0:4])
            month = int(str(stamp)[5:7])
            day = int(str(stamp)[8:10])
            cur_date = datetime.datetime(year,month,day)
            return cur_date


#Load the data to a 2D NumPy array, skipping header
in_swe_data = np.genfromtxt(swe_data_pth, delimiter=',',dtype=[('name','U6'),('date','U17'),('swe','i')],skip_header=1, autostrip=True)


stations = in_swe_data['name']
dates_s = in_swe_data['date']
swes = in_swe_data['swe']
    
vfunc = np.vectorize(stamp_to_date)
    
dates = vfunc(dates_s)


station_list = np.unique(stations)


def get_swe_dur(year, station, swe_thresh, stations, dates, swes):
        
    
    start_date = datetime.datetime(year,9,1)
    end_date = datetime.datetime(year+1,9,1)
    day_strt = datetime.datetime(year,9,1)
    day_end =  datetime.datetime(year,9,2)

    one_day = day_end-day_strt
    
    filt = (stations==station) & (dates>=start_date) & (dates<end_date)
    
    swe_filt = swes[filt]
    dates_filt = dates[filt]

    max_gap = 0


    for i in range(len(dates_filt)-1):

        dif = dates_filt[i+1]-dates_filt[i]


        if dif.days>max_gap:
            max_gap = dif.days

    if ((len(dates_filt)>0) and (swe_filt[len(swe_filt)-1]<swe_thresh) and (swe_filt[0]<swe_thresh) and (max_gap<5)):
        
        delta = dates_filt[0] - start_date
        data_start = delta.days
        delta = dates_filt[len(dates_filt)-1] - start_date
        data_end = delta.days

        
        start_found = False
        
        start = -9
        end = -9
        max_dur = -9
        f_start_idx = 0

        snow_metrics = [start,end,max_dur]
        
        
        for i in range(len(dates_filt)):
            
            swe = swe_filt[i]
            
            delta = dates_filt[i] - start_date
            days = delta.days
            

            
            if ((swe>=swe_thresh) and (start_found==False)):
                
                start = days
                start_found = True
                start_idx = i


            if ((swe<=swe_thresh) and (start_found==True)):
                
                end = days
                
                dur = end-start
                
                start_found=False
                
                
                if dur>max_dur:
                
                    f_start_idx = start_idx

                    end_idx = i
                    
                    snow_metrics = [start,end,dur]
                    
                    max_dur = dur

        start_chk = dates_filt[f_start_idx]-dates_filt[f_start_idx-1]

        try:
            end_chk = dates_filt[end_idx+1]-dates_filt[end_idx]

        except:
            return [-9,-9,-9,-9,-9]
            
        
        if (start_chk.days <= 1) and (end_chk.days <= 1):
                    
            return [snow_metrics[0],snow_metrics[1],snow_metrics[2],data_start,data_end]

        else:

            return [-9,-9,-9,-9,-9]
    
    else:
        
        return [-9,-9,-9,-9,-9]
        

for stat in station_list:
    for yr in range(2002,2017):
 
        snow_metrics = get_swe_dur(yr, stat, 15, stations, dates, swes)
        
        line = stat+","+str(yr)+","+str(snow_metrics[0])+","+str(snow_metrics[1])+","+str(snow_metrics[2])+","+str(snow_metrics[3])+","+str(snow_metrics[4])+"\n"
        
        swe_out.write(line) 
            
        
swe_out.close()  
    

