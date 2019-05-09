###load necessary packages
library(shiny)
require(leaflet)
library(tidyverse)
library(data.table)
library(ggplot2)
library(maptools)
library(rgdal)
library(geosphere)
library(rgeos)
library(sf)
library(RColorBrewer)
library(reshape2)
library(plyr)
library(forecast)

###set system language to Chinese
Sys.setlocale(category = "LC_ALL", locale = "cht")

###population data
pop_all <- fread("./data/result/Total_population.csv", stringsAsFactors = FALSE)

###map data
County <- sf::st_read("./data/County/COUNTY_MOI_1070516.shp", stringsAsFactors = FALSE)
nl <- c("臺中", "臺南", "高雄", "新竹", "嘉義")
for(i in nl) County$COUNTYNAME[grepl(i, County$COUNTYNAME)] <- i
County <- st_transform(County, '+proj=longlat +datum=WGS84')#transform gis system
Bound <- as(County, "Spatial")#transform it to SpatialDataFrame
Bound <- fortify(Bound)#transform it to data.frame
centers <- as(County, "Spatial")
row.names(centers) <- centers$COUNTYENG
centers <- data.frame(gCentroid(centers, byid = TRUE))#get the center location of county
centers$CountyName <- row.names(centers)
County2 <- dplyr::left_join(x = County, y = pop_all, by = "COUNTYNAME")
Countylist <- data.frame(County2)
Countylist$COUNTYENG <- substr(Countylist$COUNTYENG, 1, regexpr("City|County", Countylist$COUNTYENG) - 2)#get the English name of county
Countylist <- unique(Countylist[, 3:4])
Countylist$COUNTYMIX <- paste0(Countylist$COUNTYNAME, " (", Countylist$COUNTYENG, ")")#get the Chinese integrated English name of county

###Unemployment Rate data
unemploymentrate <- fread("./data/result/unemploymentrate.csv", stringsAsFactors = FALSE)
###Disposable Income data
DI <- fread("./data/result/disposable_income.csv", stringsAsFactors = FALSE)

###make a data list
mappinglist <- list(pop_all, unemploymentrate, DI)
names(mappinglist) <- c("Population", 
                        "Unemployment Rate",
                        "Disposable Income")

setDF(pop_all)

###generate a new form of population data to show the population data in the Data Exploration
formalpop <- melt(pop_all, id = 1:3, variable.name = "Age", value.name = "Population")
formalpop$Age <- as.character(formalpop$Age)
setDT(pop_all)
formalpop$Age <- gsub("c", "", formalpop$Age)
formalpop$Age <- gsub("to", "-", formalpop$Age)
formalpop$Age <- gsub("85", "85+", formalpop$Age)
formalpop$Age <- gsub("_pop", "", formalpop$Age)

