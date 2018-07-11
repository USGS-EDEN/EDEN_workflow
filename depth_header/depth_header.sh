#! /bin/ksh
#

# Calculate quarter for yesterday
echo `date`
mon=$(date -v -1d +'%m')
year=$(date -v -1d +'%Y')
if [ $mon -ge 1 -a $mon -le 3 ]; then
     qtr=1
elif [ $mon -ge 4 -a $mon -le 6 ]; then
     qtr=2
elif [ $mon -ge 7 -a $mon -le 9 ]; then
     qtr=3
elif [ $mon -ge 10 -a $mon -le 12 ]; then
     qtr=4
fi

# Move to working directory; clean up old files
cd ~/Desktop/EDEN_workflow/depth_header/
rm *.nc
rm wl/*.nc

# Retrieve current WL and depth netCDFs
ftp -in ftpint.usgs.gov << e_o_f
user anonymous bmccloskey@usgs.gov
binary
cd /pub/er/fl/st.petersburg/eden-data/netcdf
get d${year}_q$qtr.nc d${year}_q$qtr.nc
get ${year}_q$qtr.nc wl/${year}_q$qtr.nc
bye
e_o_f

file=`ls d*.nc`
# Rename stage variable to “depth” rename file to “_fixed.nc”
/opt/local/bin/ncrename -v stage,depth -h d*.nc ${file%.*}_fixed.nc
# Rename long_name attribute to “depth”
/opt/local/bin/ncatted -a long_name,depth,o,c,depth -h d*_fixed.nc
# Option to set <0 depths to 0
#/opt/local/bin/ncap2 -S depth.nco -h d*_fixed.nc d*_fixed_zeroed.nc
# R script to add min, max, and mapping attributes to header
/usr/local/bin/R --no-save --no-site-file < ~/Desktop/EDEN_workflow/depth_header/ncdf_header_min_max.R

# Push new versions back to FTP
ftp -in ftpint.usgs.gov << e_o_f
user anonymous bmccloskey@usgs.gov
binary
cd /pub/er/fl/st.petersburg/eden-data/netcdf
mput d*_fixed.nc
lcd ~/Desktop/EDEN_workflow/depth_header/wl/
mput *.nc
bye
e_o_f
