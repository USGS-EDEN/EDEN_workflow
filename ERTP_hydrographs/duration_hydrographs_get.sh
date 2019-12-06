#! /bin/ksh
#

cd /export1/htdocs/eden
/usr/kerberos/bin/ftp -in ftpint.usgs.gov << e_o_f
user anonymous sunshine@usgs.gov
cd /pub/er/fl/st.petersburg/eden/ertp_hydrographs/
binary
mget hydrographs/*
mget thumbnails/*
ascii
mget table/*
bye
e_o_f
