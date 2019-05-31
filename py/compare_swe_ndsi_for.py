# -*- coding: utf-8 -*-
"""
Created on Wed May  8 10:43:58 2019

@author: hgleason
"""

import numpy as np
import modis_ndsi_to_snow_dur_yrly_for_spln as lowess_src
import time

def get_ndsi_ser(stn, yr, ndsi):
    
    filt = ndsi[(ndsi[0:,0]==stn) & (ndsi[0:,1]==yr)]

    if len(filt.flatten())>0:
    
        ndsi_str = filt[0,4:]
    
        ndsi_int = ndsi_str.astype(int)
    
        return ndsi_int
    
    else:

        return filt.flatten()


def get_swe_dur(stn, yr, swe_in):

    filt = swe_in[(swe_in[0:,0]==stn) & (swe_in[0:,1]==yr)]

    return filt



if __name__ == '__main__':

    swe_dur_pth = "C:/Users/hgleason/Dropbox/Git_Snow_MODIS/Data/ASWS/OUT/Snow_Dur_from_SWE.csv"

    ndsi_series_pth = "C:/Users/hgleason/Dropbox/Git_Snow_MODIS/Data/MODIS/Derived/MD10A1_ASWS_Sampls.csv"

    swe_in = np.loadtxt(swe_dur_pth, delimiter=",", dtype='U20', skiprows=1)

    ndsi = np.loadtxt(ndsi_series_pth, delimiter=",", skiprows=1, dtype='U20')


    bndwths = [0.1,0.15,0.2,0.25,0.3,0.35,.4]
    strt_ndsi = [10,15,20,25,30,35,40,45,50]
    end_ndsi = [10,15,20,25,30,35,40,45,50]

    stns = swe_in[0:,0]



    years = swe_in[0:,1]


    out_file = open("C:/Users/hgleason/Dropbox/Git_Snow_MODIS/Data/ASWS/OUT/Snow_Dur_Comp_it1_dlta3_org.csv","w+") 

     
    print("Start: ",time.time())
    for i in range(len(years)):

        stn = stns[i]

        yr = years[i]

        print("Working...",stn,yr)
        
        for bw in bndwths:
            for strt in strt_ndsi:
                for end in end_ndsi:
                    
                    ndsi_in = get_ndsi_ser(stn,yr,ndsi)
    
                    if len(ndsi_in) > 0:
                        
                        snow_mets = lowess_src.ndsi_lowess_interp(ndsi_in,bw,strt,end,0)
                            
                        swe_mets = get_swe_dur(stn,yr,swe_in)
    
                        line = str(swe_mets[0][0])+","
    
                        for ob in range(1,len(swe_mets[0])):
                            line = line+str(swe_mets[0][ob])+","
    
                        for ob in range(len(snow_mets)):
                            line = line+str(snow_mets[ob])+","
    
                        line = line+str(bw)+","+str(strt)+","+str(end)
    
                        out_file.write(line+"\n")


    out_file.close()  
    print("End: ",time.time())
                    
