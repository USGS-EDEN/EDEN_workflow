library (RMySQL)
library (CSI)
library (RCurl)

setwd("./coastal_EDEN")
source ("../admin_pwd.R")
# Connect to database, list of gages for which to acquire data
con <- dbConnect(MySQL(), user = usr, password = pword, dbname = "csi", host = "igsafpesgsz03.er.usgs.gov")

all_gages <- read.csv("usgs_gages.csv", header = T, colClasses = "character")
sd <- Sys.Date() - 90; ed <- Sys.Date() - 1
tbl <- NULL
for (k in c("salinity", "temperature", "stage")) {
  gages <- switch (k, salinity = all_gages[all_gages$param == "00095" | all_gages$param == "00480", ], temperature = all_gages[all_gages$param == "00010", ], stage = all_gages[all_gages$param == "00065", ])
  for (i in 1:dim(gages)[1]) {
    url <- paste0("https://waterservices.usgs.gov/nwis/dv/?format=rdb&sites=", gages$gage[i], "&startDT=", sd, "&endDT=", ed, "&parameterCd=", gages$param[i], "&statCd=00003&access=3")
    gage <- read.table(url, header = T, sep = "\t", colClasses = "character", check.names = F)
    gage <- gage[2:dim(gage)[1], ]
    print(paste(gages$gage[i], k))
    print(head(gage))
    if (dim(gage)[1] != 1) {
      names(gage)[3] <- "date"
      gage$date <- as.Date(gage$date)
      ts_col <- which(names(gage) == paste0(gages$tsid[i], "_", gages$param[i], "_00003"))
      if (length(ts_col)) {
        gage[, ts_col] <- gsub("[^0-9.-]", "", gage[, ts_col])
        gage[, ts_col] <- as.numeric(gage[, ts_col])
        if (gages$param[i] == "00095") {
          k1 <- 0.0120    # Wagner et al., 2006
          k2 <- -0.2174
          k3 <- 25.3283
          k4 <- 13.7714
          k5 <- -6.4788
          k6 <- 2.5842
          r <- gage[, ts_col] / 53087
          gage[, ts_col] <- k1 + k2 * r ^ 0.5 + k3 * r + k4 * r ^ 1.5 + k5 * r ^ 2 + k6 * r ^ 2.5
        }
        gage[, ts_col] <- round(gage[, ts_col], 2)
        gage[, ts_col + 2] <- names(gage[ts_col])
        names(gage)[ts_col] <- paste0(gage$site_no[1], "_", k)
        names(gage)[ts_col + 1] <- paste0(gage$site_no[1], "_code")
        names(gage)[ts_col + 2] <- paste0(gage$site_no[1], "_param")
        if (is.null(tbl)) {
          tbl <- gage[, c(3, ts_col:(ts_col + 2))]
        } else {
          tbl <- merge(tbl, gage[, c(3, ts_col:(ts_col + 2))], all = T)
        }
      }
    }
  }

  for (i in 1:dim(tbl)[1]) {
    date_check <- dbGetQuery(con, paste0("select count(date) as ct from usgs_", k, " where date = '", tbl$date[i], "'"))
    in_up <- if (date_check$ct == 1) "update" else "insert into"
    q <- paste0(in_up, " usgs_", k, " set date = '", tbl$date[i], "'")
    for (j in 2:dim(tbl)[2]) {
      if (is.na(tbl[i, j])) tmp <- "NULL" else tmp <- paste0("'", tbl[i, j], "'")
      q <- paste0(q, ", `", names(tbl)[j], "` = ", tmp)
    }
    if (date_check$ct == 1)
      q <- paste0(q, " where date = '", tbl$date[i], "'")
    dbSendQuery(con, q)
  }
  tbl <- NULL
}

db <- dbGetQuery(con, "SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_SCHEMA = 'csi'
AND TABLE_NAME = 'usgs_salinity' and COLUMN_NAME like '%_salinity'")
query <- "select date_format(date, '%Y') as Year, date_format(date, '%m') as Month"
for (j in 1:dim(db)[1])
  query <- paste0(query, ", avg(`", db$COLUMN_NAME[j], "`) as `", db$COLUMN_NAME[j], "`")
query <- paste(query, "from usgs_salinity group by Year, Month")
sal <- dbGetQuery(con, query)
r <- NULL; for (i in 1:dim(sal)[2]) if(length(which(!is.na(sal[,i]))) <= 60) r <- c(r, i)
sal <- sal[, -r]
csi <- CSIcalc(sal)
for (l in dim(csi)[1]:(dim(csi)[1] - 100)) {
  date_check <- dbGetQuery(con, paste0("select count(date) as ct from usgs_csi where date = '", rownames(csi)[l], "-01'"))
  in_up <- if (date_check$ct == 1) "update" else "insert into"
  query <- paste0(in_up, " usgs_csi set date = '", rownames(csi)[l], "-01'")
  for (k in 1:24)
    for (i in 1:dim(csi)[3]) {
      c <- if(is.na(csi[l, k, i])) "NULL" else csi[l, k, i]
      query <- paste0(query, ", `", substr(dimnames(csi)[[3]][i], 1, nchar(dimnames(csi)[[3]][i]) - 9), "_csi", k, "` = ", c)
    }
  if (date_check$ct == 1)
    query <- paste0(query, " where date = '", rownames(csi)[l], "-01'")
  dbSendQuery(con, query)
}
names(sal)[3:dim(sal)[2]] <- substr(names(sal)[3:dim(sal)[2]], 1, nchar(names(sal)[3:dim(sal)[2]]) - 9)
for (i in 3:dim(sal)[2])
  sal[, i] <- ifelse(sal[, i] < 10, sprintf("%.1f", round(sal[, i], 1)), as.character(round(sal[, i])))
write.csv(sal, "./csi/usgs_CSI_calculation_data.csv", quote = F, row.names = F)
err <- try (ftpUpload("./csi/usgs_CSI_calculation_data.csv", "ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/usgs_csi/csi_values/CSI_calculation_data.csv"))
query <- "select date_format(date, '%Y') as Year, date_format(date, '%m') as Month"
for (j in 1:dim(db)[1]) {
  q <- paste0(query, ", avg(`", db$COLUMN_NAME[j], "`) as `", db$COLUMN_NAME[j], "` from usgs_salinity group by Year, Month")
  sal <- dbGetQuery(con, q)
  sal <- sal[which(!is.na(sal[, 3]))[1]:dim(sal)[1], ]
  names(sal)[3] <- substr(names(sal)[3], 1, nchar(names(sal)[3]) - 9)
  if(length(which(!is.na(sal[, 3]))) > 60) {
    csi <- CSIcalc(sal)
    csi <- round(csi, 2)
    sal[, 3] <- ifelse(sal[, 3] < 10, sprintf("%.1f", round(sal[, 3], 1)), as.character(round(sal[, 3])))
    write.csv(sal, paste0("./salinity/", names(sal)[3], "_input.csv"), row.names = F)
    zip(paste0("./salinity/", names(sal)[3], "_input.zip"), c(paste0("./salinity/", names(sal)[3], "_input.csv"), "./metadata_input_data.txt"))
    err <- try (ftpUpload(paste0("./salinity/", names(sal)[3], "_input.zip"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/usgs_csi/csi_values/", names(sal)[3], "_input.zip")))
    CSIwrite(csi, "./csi")
    zip(paste0("./csi/", names(sal)[3], ".zip"), c(paste0("./csi/", names(sal)[3], ".csv"), "./metadata_CSI_data.txt"))
    err <- try (ftpUpload(paste0("./csi/", names(sal)[3], ".zip"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/usgs_csi/csi_values/", names(sal)[3], ".zip"), .opts = list(forbid.reuse = 1)))
    CSIplot(csi, "./csi_plots", "bottom")
    for (i in 1:24)
      err <- try (ftpUpload(paste0("./csi_plots/", names(sal)[3], "_interval", i, ".png"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/usgs_csi/csi_plots/", names(sal)[3], "_interval", i, ".png"), .opts = list(forbid.reuse = 1)))
    CSIstack(csi, "./csi_plots", leg = "bottom")
    err <- try (ftpUpload(paste0("./csi_plots/", names(sal)[3], "_stacked.png"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/usgs_csi/csi_plots/", names(sal)[3], "_stacked.png"), .opts = list(forbid.reuse = 1)))
  }
}

#t <- "create table usgs_stage_per (date date NOT NULL primary key"
#for (i in gages)
#  for (j in c(7, 14, 30, 60, 90))
#    t <- paste0(t, ", ", i, "_stage_", j, "day tinyint unsigned default NULL")
#t <- paste0(t, ")")
#dbSendQuery(con, t)
gages <- dbGetQuery(con, "describe usgs_stage")
gages <- unlist(lapply(strsplit(gages[seq(2, dim(gages)[1], 3), "Field"], "_"), "[[", 1))
q2 <- paste0("update usgs_stage_per set date = '", Sys.Date() - 1, "'")
for (j in c(7, 14, 30, 60, 90)) {
  dt <- dbGetQuery(con, paste0("SELECT DAYOFYEAR('", Sys.Date() - j, "'), DAYOFYEAR('", Sys.Date() - 1, "')"))
  ao <- if(dt[1] > dt[2]) c("OR", paste("YEAR(DATE_ADD(date, INTERVAL", j, "DAY))")) else c("AND", "YEAR(date)")
  q <- "SELECT date"
  for (i in gages)
    q <- paste0(q, ", AVG(", i, "_stage) AS avg_", i, "_stage")
  q <- paste0(q, " FROM usgs_stage WHERE DAYOFYEAR(date) >= DAYOFYEAR('", Sys.Date() - j, "') ", ao[1], " DAYOFYEAR(date) <= DAYOFYEAR('", Sys.Date() - 1, "') GROUP BY ", ao[2])
  st <- dbGetQuery(con, q)
  for (i in 2:dim(st)[2]) {
    p <- quantile(st[, i] ,c(0, .1, .25, .75, .9, 1), na.rm = T)
    m <- ifelse(st[dim(st)[1], i] == p[1], 1, ifelse(st[dim(st)[1], i] == p[6], 7, which.max(p > st[dim(st)[1], i])))
    m <- ifelse(is.na(m), "NULL", m)
    q2 <- paste0(q2, ", ", gages[i - 1], "_stage_", j, "day = ", m)
  }
}
dbSendQuery(con, q2)
q2 <- paste0("update usgs_stage_change set date = '", Sys.Date() - 1, "'")
for (j in c(7, 14, 30, 60, 90)) {
  q <- "SELECT date"
  for (i in gages)
    q <- paste0(q, ", AVG(", i, "_stage) AS avg_", i, "_stage")
  q <- paste0(q, " FROM usgs_stage WHERE date >= '", Sys.Date() - j, "' AND date <= '", Sys.Date() - 1, "'")
  st <- dbGetQuery(con, q)
  r <- "SELECT date"
  for (i in gages)
    r <- paste0(r, ", AVG(", i, "_stage) AS avg_", i, "_stage")
  r <- paste0(r, " FROM usgs_stage WHERE date >= '", Sys.Date() - j * 2, "' AND date <= '", Sys.Date() - (j + 1), "'")
  st2 <- dbGetQuery(con, r)
  p <- (st[, 2:dim(st)[2]] - st2[, 2:dim(st)[2]]) * 3.28
  for (i in 1:length(p)) {
    m <- ifelse(p[i] <= -2, 1, ifelse(p[i] <= -1, 2, ifelse(p[i] <= 0, 3, ifelse(p[i] <= 1, 4, ifelse(p[i] <= 1, 4, ifelse(p[i] <= 2, 5, ifelse(p[i] <= 3, 6, 7)))))))
    m <- ifelse(is.na(m), "NULL", m)
    q2 <- paste0(q2, ", ", gages[i], "_stage_", j, "day = ", m)
  }
}
dbSendQuery(con, q2)
setwd("..")
