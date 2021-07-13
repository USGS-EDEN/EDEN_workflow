library (RMySQL)
library (CSI)
library (RCurl)

setwd("./coastal_EDEN")
source ("../admin_pwd.R")
# Connect to database, list of gages for which to acquire data
con <- dbConnect(MySQL(), user = usr, password = pword, dbname = "csi", host = "igsafpesgsz03.er.usgs.gov")

n <- read.csv("ftp://ftpint.usgs.gov/from_pub/er/enp/ENP_daily_coastal_combined_file.csv", 
                     colClasses = c("character", "character", "character", "numeric", "NULL", "NULL"), na.strings = "null")
n$type[n$type %in% c("bottom_temperature", "temperature_surfacewater")] <- "temperature"
gages <- unique(n$stn)
tbl <- NULL
report <- ""
for (j in c("salinity", "stage", "temperature")) {
  for (i in 1:length(gages)) {
    gage <- n[n$stn == gages[i] & n$type == j, ]
    print(head(gage))
    gage$date <- as.Date(gage$date)
    names(gage)[4] <- paste0(gages[i], "_", j)
    if (is.null(tbl)) {
      tbl <- gage[, 3:4]
    } else {
      tbl <- merge(tbl, gage[, 3:4], all = T)
    }
  }

  db <- dbGetQuery(con, paste0("SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_SCHEMA = 'csi'
AND TABLE_NAME = 'nps_", j, "'"))
  tbl <- tbl[, which(names(tbl) %in% db$COLUMN_NAME)]
  for (i in 1:dim(tbl)[1]) {
    date_check <- dbGetQuery(con, paste0("select count(date) as ct from nps_", j, " where date = '", tbl$date[i], "'"))
    in_up <- if (date_check$ct == 1) "update" else "insert into"
    q <- paste0(in_up, " nps_", j, " set date = '", tbl$date[i], "'")
    for (k in 2:dim(tbl)[2]) {
      if (is.na(tbl[i, k]) | tbl[i, k] < -99.99 | tbl[i, k] > 99.99) tmp <- "NULL" else tmp <- paste0("'", tbl[i, k], "'")
      q <- paste0(q, ", ", names(tbl)[k], " = ", tmp)
    }
    if (date_check$ct == 1)
      q <- paste0(q, " where date = '", tbl$date[i], "'")
    dbSendQuery(con, q)
  }
  report <- paste0(report, dim(tbl)[2] - 1, " NPS ", j, " gages loaded to EDENdb\n\n")
  tbl <- NULL
}

db <- dbGetQuery(con, "SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_SCHEMA = 'csi'
AND TABLE_NAME = 'nps_salinity' and COLUMN_NAME like '%_salinity'")
query <- "select date_format(date, '%Y') as Year, date_format(date, '%m') as Month"
for (j in 1:dim(db)[1])
  query <- paste0(query, ", avg(", db$COLUMN_NAME[j], ") as ", db$COLUMN_NAME[j])
query <- paste(query, "from nps_salinity group by Year, Month")
sal <- dbGetQuery(con, query)
csi <- CSIcalc(sal)
for (l in dim(csi)[1]:(dim(csi)[1] - 100)) {
  date_check <- dbGetQuery(con, paste0("select count(date) as ct from nps_csi where date = '", rownames(csi)[l], "-01'"))
  in_up <- if (date_check$ct == 1) "update" else "insert into"
  query <- paste0(in_up, " nps_csi set date = '", rownames(csi)[l], "-01'")
  for (k in 1:24)
    for (i in 1:dim(csi)[3]) {
      c <- if(is.na(csi[l, k, i])) "NULL" else csi[l, k, i]
      query <- paste0(query, ", ", substr(dimnames(csi)[[3]][i], 1, nchar(dimnames(csi)[[3]][i]) - 9), "_csi", k, " = ", c)
    }
  if (date_check$ct == 1)
    query <- paste0(query, " where date = '", rownames(csi)[l], "-01'")
  dbSendQuery(con, query)
}
report <- paste0(report, dim(csi)[3], " NPS CSI gages loaded to EDENdb\n\n")
names(sal)[3:dim(sal)[2]] <- substr(names(sal)[3:dim(sal)[2]], 1, nchar(names(sal)[3:dim(sal)[2]]) - 9)
for (i in 3:dim(sal)[2])
  sal[, i] <- ifelse(sal[, i] < 10, sprintf("%.1f", round(sal[, i], 1)), as.character(round(sal[, i])))
write.csv(sal, "./csi/nps_CSI_calculation_data.csv", quote = F, row.names = F)
err <- try (ftpUpload("./csi/nps_CSI_calculation_data.csv", "ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/nps_csi/csi_values/CSI_calculation_data.csv"))
query <- "select date_format(date, '%Y') as Year, date_format(date, '%m') as Month"
for (j in 2:dim(db)[1]) {
  q <- paste0("select date, ", db$COLUMN_NAME[j], " from nps_salinity order by date")
  sal <- dbGetQuery(con, q)
  names(sal)[2] <- substr(names(sal)[2], 1, nchar(names(sal)[2]) - 9)
  sal[, 2] <- ifelse(sal[, 2] < 10, sprintf("%.1f", round(sal[, 2], 1)), as.character(round(sal[, 2])))
  write.csv(sal, paste0("./salinity/", names(sal)[2], "_input.csv"), row.names = F)
  zip(paste0("./salinity/", names(sal)[2], "_input.zip"), c(paste0("./salinity/", names(sal)[2], "_input.csv"), "./metadata_input_data.txt"))
  err <- try (ftpUpload(paste0("./salinity/", names(sal)[2], "_input.zip"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/nps_csi/csi_values/", names(sal)[2], "_input.zip")))
  q <- paste0(query, ", avg(", db$COLUMN_NAME[j], ") as ", db$COLUMN_NAME[j], " from nps_salinity group by Year, Month")
  sal <- dbGetQuery(con, q)
  names(sal)[3] <- substr(names(sal)[3], 1, nchar(names(sal)[3]) - 9)
  csi <- CSIcalc(sal)
  csi <- round(csi, 2)
  CSIwrite(csi, "./csi")
  zip(paste0("./csi/", names(sal)[3], ".zip"), c(paste0("./csi/", names(sal)[3], ".csv"), "./metadata_CSI_data.txt"))
  err <- try (ftpUpload(paste0("./csi/", names(sal)[3], ".zip"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/nps_csi/csi_values/", names(sal)[3], ".zip"), .opts = list(forbid.reuse = 1)))
  if (inherits(err, "try-error")) report <- paste0(report, names(sal)[3], " CSI data NOT transferred\n") else report <- paste0(report, "\n", names(sal)[3], " CSI data transferred\n")
}

#t <- "create table nps_stage_per (date date NOT NULL primary key"
#for (i in gages)
#  for (j in c(7, 14, 30, 60, 90))
#    t <- paste0(t, ", ", i, "_stage_", j, "day tinyint unsigned default NULL")
#t <- paste0(t, ")")
#dbSendQuery(con, t)
q2 <- paste0("update nps_stage_per set date = '", Sys.Date() - 1, "'")
for (j in c(7, 14, 30, 60, 90)) {
  dt <- dbGetQuery(con, paste0("SELECT DAYOFYEAR('", Sys.Date() - j, "'), DAYOFYEAR('", Sys.Date() - 1, "')"))
  ao <- if(dt[1] > dt[2]) c("OR", paste("YEAR(DATE_ADD(date, INTERVAL", j, "DAY))")) else c("AND", "YEAR(date)")
  q <- "SELECT date"
  for (i in gages)
    q <- paste0(q, ", AVG(", i, "_stage) AS avg_", i, "_stage")
  q <- paste0(q, " FROM nps_stage WHERE DAYOFYEAR(date) >= DAYOFYEAR('", Sys.Date() - j, "') ", ao[1], " DAYOFYEAR(date) <= DAYOFYEAR('", Sys.Date() - 1, "') GROUP BY ", ao[2])
  st <- dbGetQuery(con, q)
  for (i in 2:dim(st)[2]) {
    p <- quantile(st[, i] ,c(0, .1, .25, .75, .9, 1), na.rm = T)
    m <- ifelse(st[dim(st)[1], i] == p[1], 1, ifelse(st[dim(st)[1], i] == p[6], 7, which.max(p > st[dim(st)[1], i])))
    m <- ifelse(is.na(m), "NULL", m)
    q2 <- paste0(q2, ", ", gages[i - 1], "_stage_", j, "day = ", m)
  }
}
dbSendQuery(con, q2)
q2 <- paste0("update nps_stage_change set date = '", Sys.Date() - 1, "'")
for (j in c(7, 14, 30, 60, 90)) {
  q <- "SELECT date"
  for (i in gages)
    q <- paste0(q, ", AVG(", i, "_stage) AS avg_", i, "_stage")
  q <- paste0(q, " FROM nps_stage WHERE date >= '", Sys.Date() - j, "' AND date <= '", Sys.Date() - 1, "'")
  st <- dbGetQuery(con, q)
  r <- "SELECT date"
  for (i in gages)
    r <- paste0(r, ", AVG(", i, "_stage) AS avg_", i, "_stage")
  r <- paste0(r, " FROM nps_stage WHERE date >= '", Sys.Date() - j * 2, "' AND date <= '", Sys.Date() - (j + 1), "'")
  st2 <- dbGetQuery(con, r)
  p <- (st[, 2:dim(st)[2]] - st2[, 2:dim(st)[2]]) * 3.28
  for (i in 1:length(p)) {
    m <- ifelse(p[i] <= -2, 1, ifelse(p[i] <= -1, 2, ifelse(p[i] <= 0, 3, ifelse(p[i] <= 1, 4, ifelse(p[i] <= 1, 4, ifelse(p[i] <= 2, 5, ifelse(p[i] <= 3, 6, 7)))))))
    m <- ifelse(is.na(m), "NULL", m)
    q2 <- paste0(q2, ", ", gages[i], "_stage_", j, "day = ", m)
  }
}
dbSendQuery(con, q2)
### System level commands may not work if local environment does not have sendmail installed!!
to <- "bmccloskey@usgs.gov,rsyoung@usgs.gov,mdpetkewich@usgs.gov,amedenblik@usgs.gov,bhuffman@usgs.gov"
system(paste0("echo 'Subject: NPS CSI upload report
", report, "' | /usr/sbin/sendmail ", to))
setwd("..")
