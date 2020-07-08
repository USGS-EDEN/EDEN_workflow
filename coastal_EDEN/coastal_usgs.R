library (stringr)
library (RMySQL)
library (CSI)

setwd("./coastal_EDEN")
source ("../admin_pwd.R")
# Connect to database, list of gages for which to acquire data
con <- dbConnect(MySQL(), user = usr, password = pword, dbname = "csi", host = "igsafpesgsz03.er.usgs.gov")

gages <- read.csv("usgs_gages.csv", header = T, colClasses = "character")
for (i in 1:dim(gages)[1]) {
  sd <- Sys.Date() - 7; ed <- Sys.Date() - 1
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
  r <- "insert into usgs_salinity (date"
  q <- paste0("values ('", tbl$date[i], "'")
  for (j in 2:dim(tbl)[2]) {
    r <- paste0(r, ", `", names(tbl)[j], "`")
    if (is.na(tbl[i, j])) q <- paste0(q, ", NULL") else q <- paste0(q, ", '", tbl[i, j], "'")
  }
  q <- paste0(r, ")", q, ") on duplicate key update date = date")
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
  d1 <- d2 <- as.Date(paste0(rownames(csi)[l], "-01"))
  m <- format(d2, format = "%m")
  while (format(d2, format = "%m") == m) d2 <- d2 + 1
  d2 <- as.integer(format(d2 - 1, format = "%d"))
  query <- paste0("insert into usgs_csi values ('", rownames(csi)[l], "-01'")
  for (k in c(1, 2, 3, 6, 9, 12, 18, 24))
    for (i in 1:dim(csi)[3]) {
      c <- if(is.na(csi[l, k, i])) "NULL" else csi[l, k, i]
      query <- paste0(query, ", `", strsplit(dimnames(csi)[[3]][i], "_")[[1]][1], "_csi", k, "` = ", c)
    }
  query <- paste0(substring(query, 1, nchar(query) - 2), ") on duplicate key update date = date")
  dbSendQuery(con, query)
}
setwd("..")