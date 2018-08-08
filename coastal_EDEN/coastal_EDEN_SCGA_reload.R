library (RMySQL)
source ("../admin_pwd.R")
# Connect to database and download data file
con <- dbConnect(MySQL(), user = usr, password = pword, dbname = "eden_new", host = "stpweb1-dmz.er.usgs.gov")
# This will work for local or FTP ADAM-format files, as needed
data_file <- "~/Downloads/CSI_DB_Data.csv"
file_columns <- c("character", "character", "numeric", "character", "character")
err <- try (z <- read.csv(data_file, colClasses = file_columns, na.strings = "ND"))
z$Date <- as.Date(z$Date, format = "%m/%d/%Y")
z$Approval_Code[z$Approval_Code == "A [0]"] <- "A"
z$Approval_Code[z$Approval_Code == "P [4]"] <- "P"
k1 <- 0.0120    # Wagner et al., 2006
k2 <- -0.2174
k3 <- 25.3283
k4 <- 13.7714
k5 <- -6.4788
k6 <- 2.5842
r <- z$Value / 53087
z$Sal <- k1 + k2 * r ^ 0.5 + k3 * r + k4 * r ^ 1.5 + k5 * r ^ 2 + k6 * r ^ 2.5
z$name[z$SITE == "02110755"] <- "AIW_Briarcliffe"
z$name[z$SITE == "02110770"] <- "AIW_Grand_Strand"
z$name[z$SITE == "021720677"] <- "Cooper_River_Filbin_Creek"
z$name[z$SITE == "02198920"] <- "Savannah_River"
z$name[z$SITE == "021720709"] <- "Cooper_River_HWY_17"
z$name[z$SITE == "02110760"] <- "AIW_Golf_course"
z$name[z$SITE == "02110777"] <- "AIW_Highway_9"
z$name[z$SITE == "02110815"] <- "Waccamaw_River"
z$name[z$SITE == "02172020"] <- "Cooper_River_West_Branch"
z$name[z$SITE == "02172040"] <- "Back_River"
z$name[z$SITE == "02172050"] <- "Cooper_River_Goose_Creek"
z$name[z$SITE == "021720698"] <- "Wando_River"
z$name[z$SITE == "021720710"] <- "Cooper_River"
z$name[z$SITE == "021989784"] <- "Little_Back_River_above_Canal"
z$name[z$SITE == "021989791"] <- "Little_Back_River"

dbSendQuery(con, "update coastal_sc_ga set AIW_Briarcliffe_salinity = NULL, AIW_Grand_Strand_salinity = NULL, Cooper_River_Filbin_Creek_salinity = NULL, Savannah_River_salinity = NULL, Cooper_River_HWY_17_salinity = NULL, AIW_Golf_course_salinity = NULL, AIW_Highway_9_salinity = NULL, Waccamaw_River_salinity = NULL, Cooper_River_West_Branch_salinity = NULL, Back_River_salinity = NULL, Cooper_River_Goose_Creek_salinity = NULL, Wando_River_salinity = NULL, Cooper_River_salinity = NULL, Little_Back_River_above_Canal_salinity = NULL, Little_Back_River_salinity = NULL where date < 20180223")

for (i in 143603:dim(z)[1]) {
  tmp <- if (is.na(z$Sal[i])) c("NULL", "NULL", "NULL") else c(z$Sal[i], paste0("'", z$Approval_Code[i], "'"), paste0("'", z$Parameter_code[i], "'"))
  dbSendQuery(con, paste0("update coastal_sc_ga set ", z$name[i], "_salinity = ", tmp[1], ", ", z$name[i], "_code = ", tmp[2], ", ", z$name[i], "_param = ", tmp[3], " where date = '", z$Date[i], "'"))
}
