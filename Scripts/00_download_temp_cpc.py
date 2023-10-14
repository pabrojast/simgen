# -*- coding: utf-8 -*-
"""
Created on Tue Jul 25 23:06:09 2023
from https://www.datasciencewithraghav.com/2022/04/07/how-to-get-historical-weather-data-min-temp-max-temp-and-precipitation-directly-from-noaa-national-oceanic-and-atmospheric-agency-using-python-part-1/
data: https://psl.noaa.gov/data/gridded/data.cpc.globaltemp.html
@author: pablo
"""
#%%
import wget
import os
#%%
os.chdir('D:\\tmp')

base_url = 'https://downloads.psl.noaa.gov/Datasets/'
#precip_url = 'cpc_global_precip'
temp_url = 'cpc_global_temp'
latest_year = 2023
first_year = 1979
output_dir = './'
for var in ['tmin','tmax']:
    for y in range(first_year, latest_year, 1):
        file_name = var+'.'+ str(y)+'.nc'
        output_file_path_name = output_dir+file_name
        print(f'Downloading: {file_name} to {output_file_path_name}')
        download_url = base_url+'cpc_global_temp/'+var+'.'+str(y)+'.nc'
        print(download_url)
        wget.download(download_url,output_file_path_name)