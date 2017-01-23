
setwd("U:/CityWide Performance/CovStat/CovStat Projects/Legal")

library("xlsx")
library("plyr")
library("dplyr")
library("tidyr")
library("reshape")
library("reshape2")
library("stringr")
library("zoo")
library("arcgisbinding")
library("sp")
library("spdep")
library("rgdal")
library("maptools")
library("ggmap")

#####################
##Read in for 2016 ##
#####################
#Read the contents of the worksheet into a data.frame
prm_230a16  <- read.xlsx2("230am permit holders.xlsx", sheetName="2016", as.data.frame=TRUE, header=TRUE)
prm_230a16$Year <- '2016'
prm_230a16$NOTES <- NULL
#####################
##Read in for 2015 ##
#####################
#Read the contents of the worksheet into a data.frame
prm_230a15  <- read.xlsx2("230am permit holders.xlsx", sheetName="2015", as.data.frame=TRUE, header=TRUE)
prm_230a15$Year <- '2015'
#####################
##Read in for 2014 ##
#####################
#Read the contents of the worksheet into a data.frame
prm_230a14  <- read.xlsx2("230am permit holders.xlsx", sheetName="2014", as.data.frame=TRUE, header=TRUE)
prm_230a14$Year <- '2014'

#####join 2014-2016 230 am permit holders
prm_230a14_16 <- do.call("rbind", list(prm_230a14, prm_230a15, prm_230a16))
drop <- names(prm_230a14_16) %in% c("Mailing.St", "Mailing.City", "Mailing.State", "Mailing.Zip")
prm_230a14_16 <-prm_230a14_16[!drop]
#####################
##Read in for 2013 ##
#####################
#Read the contents of the worksheet into a data.frame
prm_230a13  <- read.xlsx2("230am permit holders.xlsx", sheetName="2013", as.data.frame=TRUE, header=TRUE)
prm_230a13$Year <- '2013'
prm_230a13 <- plyr::rename(prm_230a13, c("Address"="Business.Address"))

###join 2014-16 with 2013
prm_230a <- do.call("rbind", list(prm_230a13, prm_230a14_16))


#### Add a column for geocoding
prm_230a <- within(prm_230a, {
  Location <- paste(Business.Address, City, State, Zip, sep = " ")})

## Keep only active status
prm_230aActive <- subset(prm_230a, Status == "ACTIVE")

#### Geocode against Google Maps API
coordinates <- geocode(prm_230aActive$Location)

#### Bind coordinates back to data.frame
prm_230aActive <- cbind(prm_230aActive, coordinates)

#### Create Spatial Points Data.Frame from Lat/Long Coordinates
prm_230aSP <- prm_230aActive
coordinates(prm_230aSP) <- ~lon+lat

### Define Coordinate system for spatial points data.frame
reference <- CRS("+init=epsg:4326")
proj4string(prm_230aSP) <- reference

### Permits by year ###
prm_230aSP_2016 <- subset(prm_230aSP, Year == '2016')
prm_230aSP_2015 <- subset(prm_230aSP, Year == '2015')
prm_230aSP_2014 <- subset(prm_230aSP, Year == '2014')
prm_230aSP_2013 <- subset(prm_230aSP, Year == '2013')

#### Write spatial points data.frame to a shapefile
writeOGR(obj = prm_230aSP_2016, dsn ="C:/Users/tsink/Mapping/Geocoding/Legal", layer = "prm_230AM16", driver = 'ESRI Shapefile', overwrite_layer = TRUE)
writeOGR(obj = prm_230aSP_2015, dsn ="C:/Users/tsink/Mapping/Geocoding/Legal", layer = "prm_230AM15", driver = 'ESRI Shapefile', overwrite_layer = TRUE)
writeOGR(obj = prm_230aSP_2014, dsn ="C:/Users/tsink/Mapping/Geocoding/Legal", layer = "prm_230AM14", driver = 'ESRI Shapefile', overwrite_layer = TRUE)
writeOGR(obj = prm_230aSP_2013, dsn ="C:/Users/tsink/Mapping/Geocoding/Legal", layer = "prm_230AM13", driver = 'ESRI Shapefile', overwrite_layer = TRUE)

########################################################
#### Read in Police Runs Data from SQlite database #####
########################################################
library("RSQLite")

#### connect to db
cons.police <- dbConnect(drv=RSQLite::SQLite(), dbname="O:/AllUsers/CovStat/Data Portal/repository/Data/Database Files/Police.db")

#### list all tables
tables <- dbListTables(cons.police)

#### get Calls for Services from 2016
police.runs <- dbGetQuery(cons.police, 'select * from PoliceRuns where Lat >0')
police.runs <- subset(police.runs, Category == "Calls for Service")


#### Create Spatial Points Data.Frame from Lat/Long Coordinates in Police Runs
prunsSP <- police.runs
coordinates(prunsSP) <- ~Long+Lat

### Define Coordinate system for spatial points data.frame
reference_police <- CRS("+init=epsg:4326")
proj4string(prunsSP) <- reference_police

### Calls for Service by year ###
prunsSP_2016 <- subset(prunsSP, Year == '2016')
prunsSP_2015 <- subset(prunsSP, Year == '2015')
prunsSP_2014 <- subset(prunsSP, Year == '2014')
prunsSP_2013 <- subset(prunsSP, Year == '2013')

#### Write spatial points data.frame to a shapefile
writeOGR(obj = prunsSP_2016, dsn ="C:/Users/tsink/Mapping/Geocoding/Legal", layer = "PoliceRuns2016", driver = 'ESRI Shapefile', overwrite_layer = TRUE)
writeOGR(obj = prunsSP_2015, dsn ="C:/Users/tsink/Mapping/Geocoding/Legal", layer = "PoliceRuns2015", driver = 'ESRI Shapefile', overwrite_layer = TRUE)
writeOGR(obj = prunsSP_2014, dsn ="C:/Users/tsink/Mapping/Geocoding/Legal", layer = "PoliceRuns2014", driver = 'ESRI Shapefile', overwrite_layer = TRUE)
writeOGR(obj = prunsSP_2013, dsn ="C:/Users/tsink/Mapping/Geocoding/Legal", layer = "PoliceRuns2013", driver = 'ESRI Shapefile', overwrite_layer = TRUE)

#####################
##Connect to ArcGIS##
#####################

#### Initialize arcgisbinding ####
arc.check_product()


#### Read GIS Features ####
PoliceCFS_230A16 <- arc.open("C:/Users/tsink/Mapping/Geocoding/Legal/prm_230AM_PoliceJoin16.shp")
PoliceCFS_230A15 <- arc.open("C:/Users/tsink/Mapping/Geocoding/Legal/prm_230AM_PoliceJoin15.shp")
PoliceCFS_230A14 <- arc.open("C:/Users/tsink/Mapping/Geocoding/Legal/prm_230AM_PoliceJoin14.shp")
PoliceCFS_230A13 <- arc.open("C:/Users/tsink/Mapping/Geocoding/Legal/prm_230AM_PoliceJoin13.shp")

#### Create Data.Frame ####
PoliceCFS_230A16 <- arc.select(PoliceCFS_230A16)
PoliceCFS_230A15 <- arc.select(PoliceCFS_230A15)
PoliceCFS_230A14 <- arc.select(PoliceCFS_230A14)
PoliceCFS_230A13 <- arc.select(PoliceCFS_230A13)


## Add name to missing field ##
## 2016
PoliceCFS_230A16$DBA_Nam[PoliceCFS_230A16$DBA_Nam == " "] <- NA
PoliceCFS_230A16$DBA_Nam <- ifelse(is.na(PoliceCFS_230A16$DBA_Nam), as.character(PoliceCFS_230A16$Bsnss_N),
                                   as.character(PoliceCFS_230A16$DBA_Nam))
## 2015
PoliceCFS_230A15$DBA_Nam[PoliceCFS_230A15$DBA_Nam == " "] <- NA
PoliceCFS_230A15$DBA_Nam <- ifelse(is.na(PoliceCFS_230A15$DBA_Nam), as.character(PoliceCFS_230A15$Bsnss_N),
                                   as.character(PoliceCFS_230A15$DBA_Nam))
## 2014
PoliceCFS_230A14$DBA_Nam[PoliceCFS_230A14$DBA_Nam == " "] <- NA
PoliceCFS_230A14$DBA_Nam <- ifelse(is.na(PoliceCFS_230A14$DBA_Nam), as.character(PoliceCFS_230A14$Bsnss_N),
                                   as.character(PoliceCFS_230A14$DBA_Nam))                                

## 2013
PoliceCFS_230A13$DBA_Nam[PoliceCFS_230A13$DBA_Nam == " "] <- NA
PoliceCFS_230A13$DBA_Nam <- ifelse(is.na(PoliceCFS_230A13$DBA_Nam), as.character(PoliceCFS_230A13$Bsnss_N),
                                   as.character(PoliceCFS_230A13$DBA_Nam)) 

### Join together ##
PoliceCFS_230A <- do.call("rbind", list(PoliceCFS_230A16, PoliceCFS_230A15, PoliceCFS_230A14, PoliceCFS_230A13))

## Aggregate based on count by business name
police_agg <- aggregate(Count ~ Year_1 + Bsnss_N + DBA_Nam, PoliceCFS_230A, sum)
police_agg <- police_agg[order(police_agg$Year_1 ,-police_agg$Count),]


##################################################################
#### Write CovStat Repository ####
write.csv(PoliceCFS_230A, "O:/AllUsers/CovStat/Data Portal/Repository/Data/Legal/230AMPermitHolders.csv")

#### Write to Tableau Dashboard File ####
write.csv(PoliceCFS_230A, "U:/CityWide Performance/CovStat/CovStat Projects/Legal/Tableau Files/230AM_PermitHolders.csv")

#### Write to SQLite

dbWriteTable(cons.police, "230AM Permit Holders", PoliceCFS_230A, overwrite = TRUE)

dbDisconnect(cons.police)





