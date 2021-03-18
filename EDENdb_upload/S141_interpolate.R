library (RMySQL)
library (zoo)

try (setwd("./EDENdb_upload"), silent = T)
source ("../admin_pwd.R")
# Connect to database and download data file
con <- dbConnect(MySQL(), user = usr, password = pword, dbname = "eden", host = "igsafpesgsz03.er.usgs.gov")
# Expects one extra day on both ends to use for interpolation and truncate
data_file <- "S141s_2020_2021.csv"
file_columns <- c("character", "character", "numeric", "character")
err <- try (z <- read.csv(data_file, colClasses = file_columns))
print(head(z))
names(z) <- c("date_tm", "site", "value", "flag")

# Format timestamps
z$date_tm <- paste(z$date_tm, "12:00")
z$date_tm <- as.POSIXct(z$date_tm, tz = "EST", format = "%m/%d/%Y %H:%M")
for (i in 1:length(unique(z$site))) {
  z2 <- z[z$site == unique(z$site)[i], c(1, 3, 4)]
  first <- sort(unique(z2$date_tm))[1]
  last <- rev(sort(unique(z2$date_tm)))[1]
  range <- data.frame(date_tm = seq.POSIXt(first, last, "hour"))
  z2 <- merge(range, z2, all = T)
  z2$value <- na.approx(z2$value)
  # truncate ends
  z2 <- z2[-which(as.Date(z2$date_tm, tz = "EST") == as.Date(first, tz = "EST")), ]
  z2 <- z2[-which(as.Date(z2$date_tm, tz = "EST") == as.Date(last, tz = "EST")), ]
  z2$flag[as.POSIXlt(z2$date_tm, tz = "EST")$hour != 12] <- "G"
  z2$flag[z2$flag == "NULL"] <- NA
  
  # Upload matrix to EDENdb
  for (j in 1:dim(z2)[1]) {
    if (i %% 1000 == 0) print(i)
    query <- paste0("update stage set `stage_", unique(z$site)[i], "` = ", z2$value[j], ", `flag_", unique(z$site)[i], "` = ")
    if (is.na(z2$flag[j])) tmp <- "NULL" else tmp <- paste0("'", z2$flag[j], "'")
    query <- paste0(query, tmp, " where datetime = '", z2$date_tm[j], "'")
    # Upload timestamp row to database
    err <- try (dbSendQuery(con, query))
  }
}
