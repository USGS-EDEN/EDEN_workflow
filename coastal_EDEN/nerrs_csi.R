library (dplyr)
library (RMySQL)
library (CSI)
library (RCurl)

setwd("./coastal_EDEN")
source ("../admin_pwd.R")
# Connect to database, list of gages for which to acquire data
con <- dbConnect(MySQL(), user = usr, password = pword, dbname = "csi", host = "igsafpesgsz03.er.usgs.gov")

ga <- c("acespwq", "apaebwq", "cbmocwq", "cbmrrwq", "cbvtcwq", "delslwq", "gndbhwq", "gndblwq", "grblrwq", "grborwq", "grbsqwq", "gtmpcwq", "hudscwq", "hudtnwq", "hudtswq", "jacb6wq", "jacnewq", "job09wq", "job20wq", "marscwq", "nartbwq", "niwdcwq", "niwolwq", "nocrcwq", "noczbwq", "rkblhwq", "sapldwq", "welinwq", "welsmwq", "wkbfrwq", "wqbmhwq")
# 10 days is the maximum full days before hitting 1000 record limit
sd <- format(Sys.Date() - 10, "%Y%m%d"); ed <- format(Sys.Date() - 1, "%Y%m%d")
for (k in c("salinity", "temperature", "stage")) {
  for (i in ga) {
    print(paste(i, k))
    url <- paste0("https://sofia.usgs.gov/eden/programs/nerrs.php?gage=", i, "&sd=", sd, "&ed=", ed)
    g <- read.table(url, header = T, sep = "\t", colClasses = c("character", rep("numeric", 4)))
    if (dim(g)[1]) {
      g$datetimestamp <- as.Date(g$datetimestamp, tz = "EST", format = "%m/%d/%Y %H:%M")
      if (i == "welinwq" | i == "delslwq") { g$cdepth <- g$clevel; g$f_cdepth <- g$f_clevel }
      g <- g[, 1:7]
      names(g)[c(2, 4, 6)] <- c("salinity", "temperature", "stage")
      g$salinity[g$salinity < 0 | g$f_sal < 0] <- NA
      g$temperature[g$temperature < -99.99 | g$f_temp <0] <- NA
      g$stage[g$f_cdepth < 0] <- NA
      g <- g %>% group_by(datetimestamp) %>% summarise(!!k := mean(get(k), na.rm = T))
      for (j in 1:dim(g)[1]) {
        iu <- dbGetQuery(con, paste0("select date from nerrs_", k, " where date = '", g$datetimestamp[j], "'"))
        if (dim(iu)[1]) {
          q <- paste0("update nerrs_", k, " set ", i, "_", k, " = ")
          if (is.na(g[j, k]) | g[j, k] > 99.99) q <- paste0(q, "NULL") else q <- paste0(q, g[j, k])
          q <- paste0(q, " where date = '", g$datetimestamp[j], "'")
        } else {
          q <- paste0("insert into nerrs_", k, " (date, ", i, "_", k, ") values ('", g$datetimestamp[j], "', ")
          if (is.na(g[j, k]) | g[j, k] > 99.99) q <- paste0(q, "NULL)") else q <- paste0(q, g[j, k], ")")
        }
        dbSendQuery(con, q)
      }
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
  for (k in 1:24)
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

#t <- "create table nerrs_stage_per (date date NOT NULL primary key"
#for (i in ga)
#  for (j in c(7, 14, 30, 60, 90))
#    t <- paste0(t, ", ", i, "_stage_", j, "day tinyint unsigned default NULL")
#t <- paste0(t, ")")
#dbSendQuery(con, t)
q2 <- paste0("update nerrs_stage_per set date = '", Sys.Date() - 1, "'")
for (j in c(7, 14, 30, 60, 90)) {
  q <- "SELECT date"
  for (i in ga)
    q <- paste0(q, ", AVG(", i, "_stage) AS avg_", i, "_stage")
  q <- paste0(q, " FROM nerrs_stage WHERE DAYOFYEAR(date) >= DAYOFYEAR('", Sys.Date() - j, "') AND DAYOFYEAR(date) <= DAYOFYEAR('", Sys.Date() - 1, "') GROUP BY YEAR(date)")
  st <- dbGetQuery(con, q)
  for (i in 2:dim(st)[2]) {
    p <- quantile(st[, i] ,c(0, .1, .25, .75, .9, 1), na.rm = T)
    m <- ifelse(st[dim(st)[1], i] == p[1], 1, ifelse(st[dim(st)[1], i] == p[6], 7, which.max(p > st[dim(st)[1], i])))
    m <- ifelse(is.na(m), "NULL", m)
    q2 <- paste0(q2, ", ", ga[i - 1], "_stage_", j, "day = ", m)
  }
}
dbSendQuery(con, q2)
q2 <- paste0("update nerrs_stage_change set date = '", Sys.Date() - 1, "'")
for (j in c(7, 14, 30, 60, 90)) {
  q <- "SELECT date"
  for (i in ga)
    q <- paste0(q, ", AVG(", i, "_stage) AS avg_", i, "_stage")
  q <- paste0(q, " FROM nerrs_stage WHERE date >= '", Sys.Date() - j, "' AND date <= '", Sys.Date() - 1, "' GROUP BY YEAR(date)")
  st <- dbGetQuery(con, q)
  r <- "SELECT date"
  for (i in ga)
    r <- paste0(r, ", AVG(", i, "_stage) AS avg_", i, "_stage")
  r <- paste0(r, " FROM nerrs_stage WHERE date >= '", Sys.Date() - j * 2, "' AND date <= '", Sys.Date() - j + 1, "' GROUP BY YEAR(date)")
  st2 <- dbGetQuery(con, r)
  p <- st[, 2:dim(st)[2]] - st2[, 2:dim(st)[2]] * 3.28
  for (i in 1:length(p)) {
    m <- ifelse(p[i] <= -2, 1, ifelse(p[i] <= -1, 2, ifelse(p[i] <= 0, 3, ifelse(p[i] <= 1, 4, ifelse(p[i] <= 1, 4, ifelse(p[i] <= 2, 5, ifelse(p[i] <= 3, 6, 7)))))))
    m <- ifelse(is.na(m), "NULL", m)
    q2 <- paste0(q2, ", ", ga[i], "_stage_", j, "day = ", m)
  }
}
dbSendQuery(con, q2)
setwd("..")
