library (RMySQL)
library (CSI)
library (RCurl)

setwd("./coastal_EDEN")
source ("../admin_pwd.R")
# Connect to database, list of gages for which to acquire data
con <- dbConnect(MySQL(), user = usr, password = pword, dbname = "csi", host = "igsafpesgsz03.er.usgs.gov")

gages <- read.csv("usgs_gages.csv", header = T, colClasses = "character")
sd <- Sys.Date() - 14; ed <- Sys.Date() - 1
for (i in 1:dim(gages)[1]) {
  url <- paste0("https://waterservices.usgs.gov/nwis/dv/?format=rdb&sites=", gages$gage[i], "&startDT=", sd, "&endDT=", ed, "&parameterCd=", gages$param[i], "&statCd=00003&access=3")
  gage <- read.table(url, header = T, sep = "\t", colClasses = "character", check.names = F)
  gage <- gage[2:dim(gage)[1], ]
  print(head(gage))
  if (dim(gage)[1] != 1) {
    names(gage)[3] <- "date"
    gage$date <- as.Date(gage$date)
    gage[, 4] <- gsub("[^0-9.-]", "", gage[, 4])
    gage[, 4] <- as.numeric(gage[, 4])
    if (gages$param[i] == "00095") {
      k1 <- 0.0120    # Wagner et al., 2006
      k2 <- -0.2174
      k3 <- 25.3283
      k4 <- 13.7714
      k5 <- -6.4788
      k6 <- 2.5842
      r <- gage[, 4] / 53087
      gage[, 4] <- k1 + k2 * r ^ 0.5 + k3 * r + k4 * r ^ 1.5 + k5 * r ^ 2 + k6 * r ^ 2.5
    }
    gage[, 4] <- round(gage[, 4], 2)
    gage[, 6] <- names(gage[4])
    names(gage)[4] <- paste0(gage$site_no[1], "_salinity")
    names(gage)[5] <- paste0(gage$site_no[1], "_code")
    names(gage)[6] <- paste0(gage$site_no[1], "_param")
    if (i == 1) {
      tbl <- gage[, 3:6]
    } else {
      tbl <- merge(tbl, gage[, 3:6], all = T)
    }
  }
}

for (i in 1:dim(tbl)[1]) {
  date_check <- dbGetQuery(con, paste0("select count(date) as ct from usgs_salinity where date = '", tbl$date[i], "'"))
  in_up <- if (date_check$ct == 1) "update" else "insert into"
  q <- paste0(in_up, " usgs_salinity set date = '", tbl$date[i], "'")
  for (j in 2:dim(tbl)[2]) {
    if (is.na(tbl[i, j])) tmp <- "NULL" else tmp <- paste0("'", tbl[i, j], "'")
    q <- paste0(q, ", `", names(tbl)[j], "` = ", tmp)
  }
  if (date_check$ct == 1)
    q <- paste0(q, " where date = '", tbl$date[i], "'")
  dbSendQuery(con, q)
}

db <- dbGetQuery(con, "SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_SCHEMA = 'csi'
AND TABLE_NAME = 'usgs_salinity' and COLUMN_NAME like '%_salinity'")
query <- "select date_format(date, '%Y') as Year, date_format(date, '%m') as Month"
for (j in 1:dim(db)[1])
  query <- paste0(query, ", avg(`", db$COLUMN_NAME[j], "`) as `", db$COLUMN_NAME[j], "`")
query <- paste(query, "from usgs_salinity group by Year, Month")
sal <- dbGetQuery(con, query)
csi <- CSIcalc(sal)
for (l in dim(csi)[1]:(dim(csi)[1] - 100)) {
  date_check <- dbGetQuery(con, paste0("select count(date) as ct from usgs_csi where date = '", rownames(csi)[l], "-01'"))
  in_up <- if (date_check$ct == 1) "update" else "insert into"
  query <- paste0(in_up, " usgs_csi set date = '", rownames(csi)[l], "-01'")
  for (k in c(1, 2, 3, 6, 9, 12, 18, 24))
    for (i in 1:dim(csi)[3]) {
      c <- if(is.na(csi[l, k, i])) "NULL" else csi[l, k, i]
      query <- paste0(query, ", `", substr(dimnames(csi)[[3]][i], 1, nchar(dimnames(csi)[[3]][i]) - 9), "_csi", k, "` = ", c)
    }
  if (date_check$ct == 1)
    query <- paste0(query, " where date = '", rownames(csi)[l], "-01'")
  dbSendQuery(con, query)
}
write.csv(sal, "./csi/usgs_CSI_calculation_data.csv", quote = F, row.names = F)
err <- try (ftpUpload("./csi/usgs_CSI_calculation_data.csv", "ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/usgs_csi/csi_values/CSI_calculation_data.csv"))
query <- "select date_format(date, '%Y') as Year, date_format(date, '%m') as Month"
for (j in 2:dim(db)[1]) {
  q <- paste0("select date, `", db$COLUMN_NAME[j], "`, `", strsplit(db$COLUMN_NAME[j], "_")[[1]][1], "_code`, `", strsplit(db$COLUMN_NAME[j], "_")[[1]][1], "_param` from usgs_salinity order by date")
  sal <- dbGetQuery(con, q)
  write.csv(sal, paste0("./csi/", db$COLUMN_NAME[j], "_input.csv"), row.names = F)
  err <- try (ftpUpload(paste0("./csi/", db$COLUMN_NAME[j], "_input.csv"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/usgs_csi/csi_values/", db$COLUMN_NAME[j], "_input.csv")))
  q <- paste0(query, ", avg(`", db$COLUMN_NAME[j], "`) as `", db$COLUMN_NAME[j], "` from usgs_salinity group by Year, Month")
  sal <- dbGetQuery(con, q)
  csi <- CSIcalc(sal)
  CSIwrite(csi, "./csi")
  err <- try (ftpUpload(paste0("./csi/", db$COLUMN_NAME[j], ".csv"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/usgs_csi/csi_values/", strsplit(db$COLUMN_NAME[j], "_")[[1]][1], ".csv"), .opts = list(forbid.reuse = 1)))
}
setwd("..")
