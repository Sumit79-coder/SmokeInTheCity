# -*- coding: utf-8 -*-
"""
Created on Wed Feb 24 16:39:45 2016

@author: sbrey
"""

from mpl_toolkits.basemap import Basemap
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.cm as cm
import pylab as pl


# First load the ozone effect data from the shared summary data output by
# R/ozoneDistributionsAnalysis.R
baseDir = '/Users/sbrey/projects/smokeInTheCity/figures/'
#parameters = 'PM2.5_non_FRM_Mass_24hr_Arithmetic.Mean_allSummers_ecmwf_nDays=30_TSdFactor=-0.5/'
parameters = 'PM2.5_non_FRM_Mass_24hr_Arithmetic.Mean_allSummers_ecmwf_nDays=30_TSdFactor=-1/'
fileName = baseDir + parameters + 'summary_data.csv'
df = np.loadtxt(fileName,
                dtype = 'str', delimiter =",", 
                skiprows = 1, comments=None)

# Get rid of R NA becuase they freak python out
NAMask = df == 'NA'
df[NAMask] = 'nan'

# Get all the data relative to plotting 
effect = np.array(df[:,9], dtype='float')
significant = np.array(df[:,10], dtype='string')
lat = np.array(df[:,4], dtype='float')
lon = np.array(df[:,3], dtype='float')

# Reduce all these arrays to only those where the effect is significant
#effect = effect[significant == 'TRUE']
#lat    = lat[significant == 'TRUE']
#lon    = lon[significant == 'TRUE']

m = Basemap(projection='hammer',lon_0=180)
x, y = m(lon,lat)

# Now get the plot set up
#from mpl_toolkits.basemap import Basemap, cm
#%pylab inline
#pylab.rcParams['figure.figsize'] = 13, 9

# create figure and axes instances
pl.figure()

m = Basemap(projection='merc', lat_0=0, lon_0=0,
    resolution = 'l', area_thresh=10000,
    llcrnrlon=-125., llcrnrlat=25.,
    urcrnrlon=-60., urcrnrlat=50.)

# draw coastlines, state and country boundaries, edge of map.
m.drawcoastlines()
m.drawstates()
m.drawcountries()

m.scatter(x,y,marker='o', c=effect, s=100)
m.colorbar(label="$O_{3}$ ppbv ", size="5%")
pl.savefig('mapChangeSmokeColorbar_python.pdf')