/ecmwfData README

This data contains data downloaded from: 

http://apps.ecmwf.int/datasets/data/interim-full-daily/levtype=sfc/

The URL of the request stored in this project is below. 

http://apps.ecmwf.int/datasets/data/interim-full-daily/levtype=sfc/requests/netcdf/56c5096d7991bb4119d48cb9/

The datasets where chosen to get gridded temperature data for April - Oct for
the years 2005 - 2015. This gives more temporal coverage than needed for this
project.

The local copy of this project contains two .nc files and two .RData files that are too big to store on this free git repository. To run R/subsetECMWFData.R and R/runAnalysisPipeline you will need the following files:

ecmwfData/2m-T-00UTC_ecmwf_may-oct.nc
ecmwfData/cloudFrac_00Z_ecmwf_may-oct.nc
ecmwfData/t2m.RData
ecmwfData/tcc.RData


You can download them at the links listed above. I can also send them upon request. Email me at sjbrey at rams dot colostate dot edu. 