# COMPUTE AVERAGE MONTHLY SPEI VALUES FOR EVERY NUTS2016 LEVEL
# Milos Popovic
# 05/03/2021

library(ncdf4, quietly=T) # package for netcdf manipulation
library(raster, quietly=T) # package for raster manipulation
library(rgdal, quietly=T) # package for geospatial analysis
library(ggplot2, quietly=T) # package for plotting
library(exactextractr, quietly=T) # package for zonal statistics
library(sf, quietly=T) # package for geospatial analysis
library(reshape2, quietly=T) # package for data manipulation

# set seed for reproducibility
set.seed(05032021)

#inspect nc file
nc_data <- nc_open('spei01.nc') # load spei locally
print(nc_data) # show file properties

lon <- ncvar_get(nc_data, "lon") # store longitude
lat <- ncvar_get(nc_data, "lat", verbose = F) # store latitude
t <- ncvar_get(nc_data, "time") # store time-series
spei.array <- ncvar_get(nc_data, "spei") # turn into array
fillvalue <- ncatt_get(nc_data, "spei", "_FillValue") # get NA values for this file

#use stack from raster
n <- stack('spei01.nc') # load spei file as stacked raster 
n[n == fillvalue$value] <- NA # fill NAs

#download all NUTS2016 shapefiles
temp1 <- tempfile(fileext = ".zip")
download.file("https://gisco-services.ec.europa.eu/distribution/v2/nuts/download/ref-nuts-2016-01m.shp.zip", 
    temp1)
unzip(temp1)

#unzip NUTS0, NUTS1, NUTS2 and NUTS3 shapefiles in WGS84 coordinate system 
unzip("NUTS_RG_01M_2016_4326_LEVL_0.shp.zip")
unzip("NUTS_RG_01M_2016_4326_LEVL_1.shp.zip")
unzip("NUTS_RG_01M_2016_4326_LEVL_2.shp.zip")
unzip("NUTS_RG_01M_2016_4326_LEVL_3.shp.zip")

# import
wd <- getwd()
shp_files <- list.files(wd, pattern = "\\.shp$")

# Batch shapefile loading function from ealfons1 #
rgdal_batch_shp <- function(shp_list) {
	layer_name <- as.character(gsub(".shp","",shp_list))
    shp_spdf <-readOGR(dsn = wd, stringsAsFactors = FALSE, verbose = TRUE, 
                         useC = TRUE, dropNULLGeometries = TRUE, addCommentsToPolygons = TRUE,
                         layer = layer_name, require_geomType = NULL,
                         p4s = NULL, encoding = 'ESRI Shapefile')
    }

batch_shp_list <- lapply(shp_files, rgdal_batch_shp)

#--Extract each shp in list into its own object
# name them nuts_0, nuts_1, nuts_2, nuts_3
    for(i in seq(batch_shp_list))
      assign(paste("nuts_", i-1, sep = ""), batch_shp_list[[i]])

#NUTS0
nuts_0$id <- 1:max(nrow(nuts_0)) # common id
nuts0 <- nuts_0 %>% st_as_sf()

nts0  <- exact_extract(n, nuts0 , "mean")
nts0$id <- 1:max(nrow(nts0)) # common id
nt0 <- melt(data = nts0, id.vars = "id")

# turn variable string into date string
nt0$date <- nt0$variable %>%
           gsub("mean.X", "", .) %>%
           gsub("\\.", "-", .) %>%
           as.Date()
nt0$year <- format(as.Date(nt0$date, format="%d/%m/%Y"),"%Y")
nt0$month <- format(as.Date(nt0$date, format="%d/%m/%Y"),"%m")
nt0$day <- format(as.Date(nt0$date, format="%d/%m/%Y"),"%d")

spei_n0 <- merge(nt0[,c(1, 3:7)], nuts_0, by="id")

spei_n0$indicator_code <- "spei01"
spei_n0$unit <- "spei"
colnames(spei_n0)[colnames(spei_n0) == "NUTS_ID"] <- "geo"
colnames(spei_n0)[colnames(spei_n0) == "date"] <- "time"
spei_n0$frequency <- "*M*onthly"
spei_n0$estimate <- spei_n0$value
spei_n0$db_source <- "average_spei_nuts0"

n0df <- spei_n0[,c("indicator_code", "unit", "geo", "time", "value",
				 "year", "month", "day", "frequency", "estimate", "db_source")]

write.csv(file="nuts0_spei01_average.csv", n0df, row.names=FALSE)

#NUTS1
nuts_1$id <- 1:max(nrow(nuts_1)) # common id
nuts1 <- nuts_1 %>% st_as_sf()

nts1  <- exact_extract(n, nuts1 , "mean")
nts1$id <- 1:max(nrow(nts1)) # common id
nt1 <- melt(data = nts1, id.vars = "id")

# turn variable string into date string
nt1$date <- nt1$variable %>%
           gsub("mean.X", "", .) %>%
           gsub("\\.", "-", .) %>%
           as.Date()
nt1$year <- format(as.Date(nt1$date, format="%d/%m/%Y"),"%Y")
nt1$month <- format(as.Date(nt1$date, format="%d/%m/%Y"),"%m")
nt1$day <- format(as.Date(nt1$date, format="%d/%m/%Y"),"%d")

spei_n1 <- merge(nt1[,c(1, 3:7)], nuts_1, by="id")

spei_n1$indicator_code <- "spei01"
spei_n1$unit <- "spei"
colnames(spei_n1)[colnames(spei_n1) == "NUTS_ID"] <- "geo"
colnames(spei_n1)[colnames(spei_n1) == "date"] <- "time"
spei_n1$frequency <- "*M*onthly"
spei_n1$estimate <- spei_n1$value
spei_n1$db_source <- "average_spei_nuts1"

n1df <- spei_n1[,c("indicator_code", "unit", "geo", "time", "value",
				 "year", "month", "day", "frequency", "estimate", "db_source")]

write.csv(file="nuts1_spei01_average.csv", n1df, row.names=FALSE)

#NUTS2
nuts_2$id <- 1:max(nrow(nuts_2)) # common id
nuts2 <- nuts_2 %>% st_as_sf()

nts2  <- exact_extract(n, nuts2 , "mean")
nts2$id <- 1:max(nrow(nts2)) # common id
nt2 <- melt(data = nts2, id.vars = "id")

# turn variable string into date string
nt2$date <- nt2$variable %>%
           gsub("mean.X", "", .) %>%
           gsub("\\.", "-", .) %>%
           as.Date()
nt2$year <- format(as.Date(nt2$date, format="%d/%m/%Y"),"%Y")
nt2$month <- format(as.Date(nt2$date, format="%d/%m/%Y"),"%m")
nt2$day <- format(as.Date(nt2$date, format="%d/%m/%Y"),"%d")

spei_n2 <- merge(nt2[,c(1, 3:7)], nuts_2, by="id")

spei_n2$indicator_code <- "spei01"
spei_n2$unit <- "spei"
colnames(spei_n2)[colnames(spei_n2) == "NUTS_ID"] <- "geo"
colnames(spei_n2)[colnames(spei_n2) == "date"] <- "time"
spei_n2$frequency <- "*M*onthly"
spei_n2$estimate <- spei_n2$value
spei_n2$db_source <- "average_spei_nuts2"

n2df <- spei_n2[,c("indicator_code", "unit", "geo", "time", "value",
				 "year", "month", "day", "frequency", "estimate", "db_source")]

write.csv(file="nuts2_spei01_average.csv", n2df, row.names=FALSE)

#NUTS2
nuts_3$id <- 1:max(nrow(nuts_3)) # common id
nuts3 <- nuts_3 %>% st_as_sf()

nts3  <- exact_extract(n, nuts3 , "mean")
nts3$id <- 1:max(nrow(nts3)) # common id
nt3 <- melt(data = nts3, id.vars = "id")

# turn variable string into date string
nt3$date <- nt3$variable %>%
           gsub("mean.X", "", .) %>%
           gsub("\\.", "-", .) %>%
           as.Date()
nt3$year <- format(as.Date(nt3$date, format="%d/%m/%Y"),"%Y")
nt3$month <- format(as.Date(nt3$date, format="%d/%m/%Y"),"%m")
nt3$day <- format(as.Date(nt3$date, format="%d/%m/%Y"),"%d")

spei_n3 <- merge(nt3[,c(1, 3:7)], nuts_3, by="id")

spei_n3$indicator_code <- "spei01"
spei_n3$unit <- "spei"
colnames(spei_n3)[colnames(spei_n3) == "NUTS_ID"] <- "geo"
colnames(spei_n3)[colnames(spei_n3) == "date"] <- "time"
spei_n3$frequency <- "*M*onthly"
spei_n3$estimate <- spei_n3$value
spei_n3$db_source <- "average_spei_nuts"

n3df <- spei_n3[,c("indicator_code", "unit", "geo", "time", "value",
				 "year", "month", "day", "frequency", "estimate", "db_source")]

write.csv(file="nuts3_spei01_average.csv", n3df, row.names=FALSE)