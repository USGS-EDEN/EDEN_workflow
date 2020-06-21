#! /bin/ksh
#

cd /var/www/eden/coastal
/usr/kerberos/bin/ftp -in ftpint.usgs.gov << e_o_f
user anonymous sunshine@usgs.gov
cd /pub/er/fl/st.petersburg/eden/coastal_eden/
binary
mget full/*
mget thumbnails/*
mget csi_stacked/*
mget csi_values/*
mget salinity_duration_hydrographs/*
cd /pub/er/fl/st.petersburg/eden/coastal_eden_scga/
lcd ../coastal_eden_scga/
mget csi_plot/*
mget csi_stacked/*
mget csi_values/*
mget salinity_7day/*
mget salinity_30day/*
mget stage_7day/*
mget stage_30day/*
mget temperature_7day/*
mget temperature_30day/*
mget csi_values.csv
bye
e_o_f
cd /var/www/eden/coastal
/usr/bin/zip csi_values.zip csi_values/*
