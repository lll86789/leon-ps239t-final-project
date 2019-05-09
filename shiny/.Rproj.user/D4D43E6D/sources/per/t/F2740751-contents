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

Sys.setlocale(category = "LC_ALL", locale = "cht")

pop_all <- fread("./data/result/Total_population.csv", stringsAsFactors = FALSE)

County <- sf::st_read("./data/County/COUNTY_MOI_1070516.shp", stringsAsFactors = FALSE)
nl <- c("臺中", "臺南", "高雄", "新竹", "嘉義")
for(i in nl) County$COUNTYNAME[grepl(i, County$COUNTYNAME)] <- i
County <- st_transform(County, '+proj=longlat +datum=WGS84')
Bound <- as(County, "Spatial")
Bound <- fortify(Bound)
centers <- as(County, "Spatial")
row.names(centers) <- centers$COUNTYENG
centers <- data.frame(gCentroid(centers, byid = TRUE))
centers$CountyName <- row.names(centers)
County2 <- dplyr::left_join(x = County, y = pop_all, by = "COUNTYNAME")
Countylist <- data.frame(County2)
Countylist$COUNTYENG <- substr(Countylist$COUNTYENG, 1, regexpr("City|County", Countylist$COUNTYENG) - 2)
Countylist <- unique(Countylist[, 3:4])
Countylist$COUNTYMIX <- paste0(Countylist$COUNTYNAME, " (", Countylist$COUNTYENG, ")")

unemploymentrate <- fread("./data/result/unemploymentrate.csv", stringsAsFactors = FALSE)
DI <- fread("./data/result/disposable_income.csv", stringsAsFactors = FALSE)

mappinglist <- list(pop_all, unemploymentrate, DI)
names(mappinglist) <- c("Population", 
                        "Unemployment Rate",
                        "Disposable Income")

setDF(pop_all)
formalpop <- melt(pop_all, id = 1:3, variable.name = "Age", value.name = "Population")
formalpop$Age <- as.character(formalpop$Age)
setDT(pop_all)
formalpop$Age <- gsub("c", "", formalpop$Age)
formalpop$Age <- gsub("to", "-", formalpop$Age)
formalpop$Age <- gsub("85", "85+", formalpop$Age)
formalpop$Age <- gsub("_pop", "", formalpop$Age)

