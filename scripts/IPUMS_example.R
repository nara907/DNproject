
### Processed IPUMS census microdata

setwd("/Users/nathansuberi/Desktop/WRI_Programming/Py_Scripts/IPUMS_Stats_By_GeoLev1/")
file_folder <- "/Users/nathansuberi/Desktop/WRI_Programming/Py_Scripts/IPUMS_Stats_By_GeoLev1/"

# Useful if importing raw IPUMS .dat file... don't know why would, as python script works
# https://stackoverflow.com/questions/11664075/import-dat-file-into-r

data_1970_mining_employment <- read.csv(paste(file_folder,"data_1970_mining_employment.csv", sep=""), header=T)
data_1980_mining_employment <- read.csv(paste(file_folder,"data_1980_mining_employment.csv", sep=""), header=T)
data_1991_mining_employment <- read.csv(paste(file_folder,"data_1991_mining_employment.csv", sep=""), header=T)
data_2001_mining_employment <- read.csv(paste(file_folder,"data_2001_mining_employment.csv", sep=""), header=T)
data_2010_mining_employment <- read.csv(paste(file_folder,"data_2010_mining_employment.csv", sep=""), header=T)

# Examine data
data_1970_mining_employment

# Load shapefiles
# Question for Sula: What does consistent boundaries mean in IPUMS shapefiles?

library("rgdal")
geo_lev <- "world_geolev1"
dest <- paste(file_folder, geo_lev,"/", sep="")
world_geolev1 <- readOGR(dest, geo_lev)

# inspect geo_levs on the world shapefile

str(world_geolev1, max.level = 2)
str(world_geolev1@data, max.level = 2)
world_geolev1@data$GEOLEVEL1

# strip GIS boundaries down to country
country = "Argentina"
argentine_geolev1s <- world_geolev1[world_geolev1@data$CNTRY_NAME == "Argentina",]

# There are 26, was expecting 24 from IPUMS census data
paste("Number of GeoLevs in IPUMS census data for ",country," is: ", nrow(data_1970_mining_employment), sep="")
paste("Number of GeoLevs in IPUMS GIS shapefile for ",country," is: ", length(argentine_geolev1s), sep="")

# GeoLev1 does not have the leading 0 in the census data
# account for this in merge
data_1970_mining_employment

plot(argentine_geolev1s)

# Bring in mining data, filter for Aluminum in Argentina
mineral_deposits <- readOGR('./mrds', 'mrds-trim')

str(mineral_deposits, max.level=2)
str(mineral_deposits@data, max.level=2)

mineral_deposits_ARG <- mineral_deposits[!is.na(over(mineral_deposits, as(argentine_geolev1s, "SpatialPolygons"))),]

AU_deposits_ARG <- mineral_deposits_ARG[mineral_deposits$CODE_LIST %in% "AU",]
#AU_deposits_ARG <- AU_deposits[!is.na(over(AU_deposits,as(argentine_geolev1s, "SpatialPolygons"))),]

plot(mineral_deposits_ARG,col="blue", add=T)

## The wait justifies setting up EC2...

# Now merge data from IPUMS census
a <- data_2001_mining_employment
str(a)
b <- argentine_geolev1s@data
str(b)
argentine_geolev1s@data <- data.frame(b, a[match(b[,"GEOLEVEL1"], paste("0",a[,"GeoLev1"],sep="")),])
length(argentine_geolev1s)

# drop NAs from argentine map
argentine_geolev1s_rm_na <- argentine_geolev1s[!is.na(argentine_geolev1s@data$X0),]
length(argentine_geolev1s_rm_na)

argentine_geolev1s_rm_na@data

# Plot with colors
summary(argentine_geolev1s_rm_na@data$X0)
plot(argentine_geolev1s_rm_na, col=gray(argentine_geolev1s_rm_na@data$X0*10))

# Check to see if any mineral type is correlated with these areas
# Could there be bias to report employment in a certain type of mining over others,
# thus hinting at shady business?

# Naive, subset and see what proportion are in this area
mineral_deposits_in_mining_geolev1 <- mineral_deposits[!is.na(over(mineral_deposits, as(argentine_geolev1s_rm_na[argentine_geolev1s_rm_na@data$X0>0,], "SpatialPolygons"))),]

str(mineral_deposits_in_mining_geolev1, max.level=2)
str(mineral_deposits_in_mining_geolev1@data, max.level=2)

# Stats for just mines in mining employment states
summary(mineral_deposits_in_mining_geolev1@data$CODE_LIST)
# Stats for all mines
summary(mineral_deposits_ARG@data$CODE_LIST)

proportions <- summary(mineral_deposits_in_mining_geolev1@data$CODE_LIST)/summary(mineral_deposits_ARG@data$CODE_LIST)
proportions <- sort(proportions, decreasing=T)
proportions

# PB, CU, CLY, SR, AU are most represented

mineral_deposits_ARG_rm_na <- mineral_deposits_ARG[!is.na(mineral_deposits_ARG@data$CODE_LIST),]

plot(mineral_deposits_ARG_rm_na[mineral_deposits_ARG_rm_na@data$CODE_LIST=="PB",], add=T, col="blue")

# More nuanced, set up a categorical regression to test
# if mining employment is related to number of each type of mine,
# report value of coefficient and R^2