#! /bin/ksh
#

umask 002
cd /export1/htdocs/eden/data/jpgs
/usr/kerberos/bin/ftp -in ftpint.usgs.gov << e_o_f
user anonymous sunshine@usgs.gov
cd /pub/er/fl/st.petersburg/eden-data/jpg/
binary
mget *
bye
e_o_f
