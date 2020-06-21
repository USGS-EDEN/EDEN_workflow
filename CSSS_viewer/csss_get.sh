#! /bin/ksh
#

umask 002
cd /var/www/eden/csss/imgs4
/usr/kerberos/bin/ftp -in ftpint.usgs.gov << e_o_f
user anonymous sunshine@usgs.gov
cd /pub/er/fl/st.petersburg/eden/csss/images
binary
mget 2020/*
mget 2020_nest/*
ascii
cd /pub/er/fl/st.petersburg/eden/csss
lcd ..
mget por_stats.js
binary
mget csss_yr_cmp_report.pdf
mget recent_year_subpop_mean_water_depth.png
mget recent_month_subpop_mean_water_depth.png
mget recent_wet_seas_subpop_mean_water_depth.png
mget recent_dry_seas_subpop_mean_water_depth.png
mget recent_wk_subpop_mean_water_depth.png
mget CSSS_subarea_stats.csv.zip
lcd ../wadem/wading_bird_depths
cd /pub/er/fl/st.petersburg/eden/wadem/depth/
mget depth_2020/*
cd ../rr
mget rr_2020/*
bye
e_o_f
