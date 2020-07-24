library (dplyr)
library (RMySQL)
library (CSI)

setwd("./coastal_EDEN")
source ("../admin_pwd.R")
# Connect to database, list of gages for which to acquire data
con <- dbConnect(MySQL(), user = usr, password = pword, dbname = "csi", host = "igsafpesgsz03.er.usgs.gov")

g <- c("acespwq", "apaebwq", "cbmocwq", "cbmrrwq", "cbvtcwq", "delslwq", "gndbhwq", "gndblwq", "grblrwq", "grborwq", "grbsqwq", "gtmpcwq", "hudscwq", "hudtnwq", "hudtswq", "jacb6wq", "jacnewq", "job09wq", "job20wq", "marscwq", "nartbwq", "niwdcwq", "niwolwq", "nocrcwq", "noczbwq", "rkblhwq", "sapldwq", "welinwq", "welsmwq", "wkbfrwq", "wqbmhwq")
for (i in g) {
  print(i)
  sd <- format(Sys.Date() - 14, "%Y%m%d"); ed <- format(Sys.Date() - 1, "%Y%m%d")
  url <- paste0("https://sofia.usgs.gov/eden/programs/nerrs.php?gage=", i, "&sd=", sd, "&ed=", ed)
  g <- read.table(url, header = T, sep = "\t", colClasses = c("character", "numeric", "NULL", "NULL"))
  if (dim(g)[1]) {
    g$datetimestamp <- as.Date(g$datetimestamp, tz = "EST", format = "%m/%d/%Y %H:%M")
    g <- g %>% group_by(datetimestamp) %>% summarise(sal = mean(sal, na.rm = T))
    for (j in 1:dim(g)[1]) {
      iu <- dbGetQuery(con, paste0("select date from nerrs_salinity where date = '", g$datetimestamp[j], "'"))
      if (dim(iu)[1]) {
        q <- paste0("update nerrs_salinity set ", i, "_salinity = ")
        if (is.na(g$sal[j])) q <- paste0(q, "NULL") else q <- paste0(q, g$sal[j], "")
        q <- paste0(q, " where date = '", g$datetimestamp[j], "'")
      } else {
        q <- paste0("insert into nerrs_salinity (date, ", i, "_salinity) values ('", g$datetimestamp[j], "', ")
        if (is.na(g$sal[j])) q <- paste0(q, "NULL)") else q <- paste0(q, g$sal[j], ")")
      }
      dbSendQuery(con, q)
    }
  }
}

db <- dbGetQuery(con, "SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_SCHEMA = 'csi'
AND TABLE_NAME = 'nerrs_salinity'")
query <- "select date_format(date, '%Y') as Year, date_format(date, '%m') as Month"
for (j in 2:dim(db)[1])
  query <- paste0(query, ", avg(`", db$COLUMN_NAME[j], "`) as `", db$COLUMN_NAME[j], "`")
query <- paste(query, "from nerrs_salinity group by Year, Month")
sal <- dbGetQuery(con, query)
csi <- CSIcalc(sal)

for (l in 1:dim(csi)[1]) {
  q <- paste0("insert into nerrs_csi values ('", rownames(csi)[l], "-01'")
  for (k in c(1, 2, 3, 6, 9, 12, 18, 24))
    for (i in 1:dim(csi)[3]) {
      c <- if(is.na(csi[l, k, i])) "NULL" else csi[l, k, i]
      q <- paste0(q, ", `", strsplit(dimnames(csi)[[3]][i], "_")[[1]][1], "_csi", k, "` = ", c)
    }
  query <- paste0(q, ") on duplicate key update date = date")
  dbSendQuery(con, query)
}
setwd("..")
