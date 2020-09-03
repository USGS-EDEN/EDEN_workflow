library (dplyr)
library (RMySQL)
library (CSI)

setwd("./coastal_EDEN")
source ("../admin_pwd.R")
# Connect to database, list of gages for which to acquire data
con <- dbConnect(MySQL(), user = usr, password = pword, dbname = "csi", host = "igsafpesgsz03.er.usgs.gov")

ga <- c("acespwq", "apaebwq", "cbmocwq", "cbmrrwq", "cbvtcwq", "delslwq", "gndbhwq", "gndblwq", "grblrwq", "grborwq", "grbsqwq", "gtmpcwq", "hudscwq", "hudtnwq", "hudtswq", "jacb6wq", "jacnewq", "job09wq", "job20wq", "marscwq", "nartbwq", "niwdcwq", "niwolwq", "nocrcwq", "noczbwq", "rkblhwq", "sapldwq", "welinwq", "welsmwq", "wkbfrwq", "wqbmhwq")
sd <- format(Sys.Date() - 14, "%Y%m%d"); ed <- format(Sys.Date() - 1, "%Y%m%d")
for (i in ga) {
  print(i)
  url <- paste0("https://sofia.usgs.gov/eden/programs/nerrs.php?gage=", i, "&sd=", sd, "&ed=", ed)
  g <- read.table(url, header = T, sep = "\t", colClasses = c("character", "numeric", "NULL", "NULL"))
  if (dim(g)[1]) {
    g$datetimestamp <- as.Date(g$datetimestamp, tz = "EST", format = "%m/%d/%Y %H:%M")
    g <- g %>% group_by(datetimestamp) %>% summarise(sal = mean(sal, na.rm = T))
    g$sal[g$sal < 0] <- NA
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
  date_check <- dbGetQuery(con, paste0("select count(date) as ct from nerrs_csi where date = '", rownames(csi)[l], "-01'"))
  in_up <- if (date_check$ct == 1) "update" else "insert into"
  q <- paste0(in_up, " nerrs_csi set date = '", rownames(csi)[l], "-01'")
  for (k in c(1, 2, 3, 6, 9, 12, 18, 24))
    for (i in 1:dim(csi)[3]) {
      c <- if(is.na(csi[l, k, i])) "NULL" else csi[l, k, i]
      q <- paste0(q, ", `", strsplit(dimnames(csi)[[3]][i], "_")[[1]][1], "_csi", k, "` = ", c)
    }
  if (date_check$ct == 1)
    q <- paste0(q, " where date = '", rownames(csi)[l], "-01'")
  dbSendQuery(con, q)
}
write.csv(sal, "./csi/nerrs_CSI_calculation_data.csv", quote = F, row.names = F)
err <- try (ftpUpload("./csi/nerrs_CSI_calculation_data.csv", "ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/nerrs_csi/csi_values/CSI_calculation_data.csv"))
query <- "select date_format(date, '%Y') as Year, date_format(date, '%m') as Month"
for (j in 2:dim(db)[1]) {
  q <- paste0("select date, `", db$COLUMN_NAME[j], "` from nerrs_salinity order by date")
  sal <- dbGetQuery(con, q)
  write.csv(sal, paste0("./csi/", db$COLUMN_NAME[j], "_input.csv"), quote = F, row.names = F)
  err <- try (ftpUpload(paste0("./csi/", db$COLUMN_NAME[j], "_input.csv"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/nerrs_csi/csi_values/", strsplit(db$COLUMN_NAME[j], "_")[[1]][1], "_input.csv")))
  q <- paste0(query, ", avg(`", db$COLUMN_NAME[j], "`) as `", db$COLUMN_NAME[j], "` from nerrs_salinity group by Year, Month")
  sal <- dbGetQuery(con, q)
  csi <- CSIcalc(sal)
  CSIwrite(csi, "./csi")
  err <- try (ftpUpload(paste0("./csi/", db$COLUMN_NAME[j], ".csv"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/nerrs_csi/csi_values/", strsplit(db$COLUMN_NAME[j], "_")[[1]][1], ".csv"), .opts = list(forbid.reuse = 1)))
}
setwd("..")
