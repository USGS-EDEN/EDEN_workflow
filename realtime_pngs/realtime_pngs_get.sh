#! /bin/ksh
#

umask 002
cd /var/www/eden/data/pngs
/usr/kerberos/bin/ftp -in ftpint.usgs.gov << e_o_f
user anonymous sunshine@usgs.gov
cd /pub/er/fl/st.petersburg/eden-data/png/
binary
mget *
bye
e_o_f
