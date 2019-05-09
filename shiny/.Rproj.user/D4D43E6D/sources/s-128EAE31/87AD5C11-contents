###load necessary packages
library(tidyverse)
library(data.table)

###set system language to Chinese
Sys.setlocale(category = "LC_ALL", locale = "cht")
###set working directory
setwd("./shiny")
##set variable name
nam <- c("County", "Sex", "Total", "c0", "c1to4", "c1", "c2", "c3", "c4", "c5to9", 
         "c10to14", "c15to19", "c20to24", "c25to29", "c30to34", "c35to39", "c40to44", "c45to49", 
         "c50to54", "c55to59", "c60to64", "c65to69", "c70to74", "c75to79", "c80to84", "c85to89", 
         "c90to94", "c95to99", "c100")
##make a county list
pop <- openxlsx::read.xlsx("./data/COUNTY.xlsx", sheet = "105", startRow = 4)#read data
pop <- pop[, c(1:29)]#select necessary columns
setDT(pop)#set it to data.table
names(pop) <- nam#rename
pop <- pop[!is.na(Total), ]#delete NA data
pop$County <- rep(pop$County[!is.na(pop$County)], each = 3)#generate missing county value
pop <- pop[Sex != "計"&!County %in% c("福建省", "總計", "臺灣地區", "臺灣省"), ]#delete unnecessary data
county_list <- pop[, c(1:2)]#select county column
county_list <- county_list[!grepl("新竹市|嘉義市|高雄縣|臺中縣|臺南縣", County), ]#delete unnecessary data
nl <- c("臺中", "臺南", "高雄", "新竹", "嘉義")
for(i in nl) county_list[grepl(i, County), ]$County <- substr(county_list[grepl(i, County), ]$County, 1, 2)#rename some county

###read data function
readPop <- function(x){
  pop <- openxlsx::read.xlsx("./data/COUNTY.xlsx", sheet = as.character(x), startRow = 4)
  pop <- pop[, c(1:29)]
  setDT(pop)
  names(pop) <- nam
  pop <- pop[!is.na(Total), ]
  pop$County <- rep(pop$County[!is.na(pop$County)], each = 3)
  pop$County <- gsub("\\s", "", pop$County)#delete space in county name
  pop <- pop[, lapply(.SD, as.numeric), by = .(County, Sex)]#transform character to numeric
  pop <- pop[, c(1:2, 4:5, 10:29)][Sex != "計"&!County %in% c("福建省", "總計", "臺灣地區", "臺灣省"), ]#select necessary columns and delete unnecessary data
  pop[, `:=`(c0to4 = c0+c1to4, c85 = c85to89 + c90to94 + c95to99 + c100, County = ifelse(grepl("臺北縣", County), "新北市", ifelse(grepl("桃園縣", County), "桃園市", County)))]#modify data to necessary form
  pop <- pop[, c(1:2, 25, 5:20, 26)]#select necessary data
  
  nl <- c("臺中", "臺南", "高雄")
  nl2 <- c("新竹", "嘉義")
  for (i in nl){
    pop[grepl(i, County), ]$County <- substr(pop[grepl(i, County), ]$County, 1, 2)
    pop <- pop[, lapply(.SD, sum), by = .(County, Sex)]
  }#sum up some county and replace their name
  if (x >= 71){
    for (i in nl2){
      pop[grepl(i, County), ]$County <- substr(pop[grepl(i, County), ]$County, 1, 2)
      pop <- pop[, lapply(.SD, sum), by = .(County, Sex)]
    }
  }else{
    pop <- pop[!grepl("新竹市|嘉義市", County), ]
    for (i in nl2){
      pop[grepl(i, County), ]$County <- substr(pop[grepl(i, County), ]$County, 1, 2)
    }
  }#dealing with this two county which has missing data before 1982
  
  pop <- dplyr::left_join(x = county_list, y = pop, by = c("County", "Sex"))
  return(pop)
}

pop_all <- rbindlist(lapply(63:107, FUN = function(x){
  df <- readPop(x)
  df$Year <- rep(x + 1911, nrow(df))
  df <- df[, c(21, 1:20)]
  return(df)
}))#read all data and merge them into a dataframe

names(pop_all)[2] <- "COUNTYNAME"
pop_total <- pop_all[, c(1:2, 4:21)][, lapply(.SD, FUN = sum), by = .(Year, COUNTYNAME)]#make new rows which sum up the values in different sex
pop_total$Sex <- "Total"
pop_total <- pop_total[, c(1:2, 21, 3:20)]
pop_all <- rbind(pop_all, pop_total)
pop_all$Total_pop <- rowSums(pop_all[, 4:21])#calculate total population instead of age-specific population
pop_all$Sex <- ifelse(pop_all$Sex == "男", "Male", ifelse(pop_all$Sex == "女", "Female", pop_all$Sex))#translate sex from Chinese to English

write.csv(pop_all, "./data/result/Total_population.csv", row.names = FALSE)

###Create value names
var_name <- fread("./data/unemploymentrate.csv", stringsAsFactors = FALSE, skip = 2, fill = TRUE)
var_name <- gsub("\\W", "", names(var_name)[3:24])
###
unemploymentrate <- fread("./data/unemploymentrate.csv", stringsAsFactors = FALSE, skip = 4, fill = TRUE)
unemploymentrate <- unemploymentrate[1:20, ]#select necessary columns
unemploymentrate$V1 <- NULL
unemploymentrate$V23 <- gsub("\\W", "", unemploymentrate$V23)#delete punctuations
unemploymentrate$V23 <- as.numeric(unemploymentrate$V23)
unemploymentrate$V24 <- gsub("\\W", "", unemploymentrate$V24)
unemploymentrate$V24 <- as.numeric(unemploymentrate$V24)
names(unemploymentrate) <- c("Year", var_name)
setDF(unemploymentrate)
unemploymentrate <- melt(unemploymentrate, id = 1, variable.name = "COUNTYNAME", 
                         value.name = "Total_pop")#reshape the dataframe
unemploymentrate$COUNTYNAME <- as.character(unemploymentrate$COUNTYNAME)#transform factor to character
for(i in nl) unemploymentrate$COUNTYNAME[grepl(i, unemploymentrate$COUNTYNAME)] <- i#rename county
write.csv(unemploymentrate, "./data/result/unemploymentrate.csv", row.names = FALSE)

###
DI <- fread("./data/disposable_income.csv", stringsAsFactors = FALSE, skip = 4, fill = TRUE)
DI <- DI[1:20, ]
DI$V1 <- NULL
DI$V23 <- as.numeric(DI$V23)
DI$V24 <- gsub("\\W", "", DI$V24)
DI$V24 <- as.numeric(DI$V24)
DI <- cbind(DI[, 1], as.data.frame(apply(DI[, 2:23], 2, FUN = function(x) x/30)))
names(DI) <- c("Year", var_name)
setDF(DI)
DI <- melt(DI, id = 1, variable.name = "COUNTYNAME", value.name = "Total_pop")
DI$COUNTYNAME <- as.character(DI$COUNTYNAME)
for(i in nl) DI$COUNTYNAME[grepl(i, DI$COUNTYNAME)] <- i

write.csv(DI, "./data/result/disposable_income.csv", row.names = FALSE)

###
air <- fread("./data/air_condition.csv", stringsAsFactors = FALSE, skip = 4, fill = TRUE, header = FALSE)
air <- air[1:20, ]
air$V1 <- NULL
air <- as.data.frame(apply(air, 2, FUN = function(x) as.numeric(gsub("\\W", "", x))), stringsAsFactors = FALSE)
names(air) <- c("Year", var_name)
setDF(air)
air <- melt(air, id = 1, variable.name = "COUNTYNAME", value.name = "Total_pop")
air$COUNTYNAME <- as.character(air$COUNTYNAME)
for(i in nl) air$COUNTYNAME[grepl(i, air$COUNTYNAME)] <- i

write.csv(air, "./data/result/air_condition.csv", row.names = FALSE)

###
crime <- fread("./data/crime.csv", stringsAsFactors = FALSE, skip = 4, fill = TRUE, header = FALSE)
crime <- crime[1:20, ]
crime$V1 <- NULL
crime <- as.data.frame(apply(crime, 2, FUN = function(x) as.numeric(gsub("\\W", "", x))), stringsAsFactors = FALSE)
names(crime) <- c("Year", var_name)
setDF(crime)
crime <- melt(crime, id = 1, variable.name = "COUNTYNAME", value.name = "Total_pop")
crime$COUNTYNAME <- as.character(crime$COUNTYNAME)
for(i in nl) crime$COUNTYNAME[grepl(i, crime$COUNTYNAME)] <- i

write.csv(crime, "./data/result/crime.csv", row.names = FALSE)

###
garbage <- fread("./data/garbage.csv", stringsAsFactors = FALSE, skip = 4, fill = TRUE, header = FALSE)
garbage <- garbage[1:20, ]
garbage$V1 <- NULL
garbage <- as.data.frame(apply(garbage, 2, FUN = function(x) as.numeric(gsub("\\W", "", x))), stringsAsFactors = FALSE)
names(garbage) <- c("Year", var_name)
setDF(garbage)
garbage <- melt(garbage, id = 1, variable.name = "COUNTYNAME", value.name = "Total_pop")
garbage$COUNTYNAME <- as.character(garbage$COUNTYNAME)
for(i in nl) garbage$COUNTYNAME[grepl(i, garbage$COUNTYNAME)] <- i

write.csv(garbage, "./data/result/garbage.csv", row.names = FALSE)

###
hospital <- fread("./data/hospital.csv", stringsAsFactors = FALSE, skip = 4, fill = TRUE, header = FALSE)
hospital <- hospital[1:20, ]
hospital$V1 <- NULL
hospital <- as.data.frame(apply(hospital, 2, FUN = function(x) as.numeric(gsub("\\W", "", x))), stringsAsFactors = FALSE)
names(hospital) <- c("Year", var_name)
setDF(hospital)
hospital <- melt(hospital, id = 1, variable.name = "COUNTYNAME", value.name = "Total_pop")
hospital$COUNTYNAME <- as.character(hospital$COUNTYNAME)
for(i in nl) hospital$COUNTYNAME[grepl(i, hospital$COUNTYNAME)] <- i

write.csv(hospital, "./data/result/hospital.csv", row.names = FALSE)

###
nursery <- fread("./data/nursery.csv", stringsAsFactors = FALSE, skip = 4, fill = TRUE, header = FALSE)
nursery <- nursery[1:20, ]
nursery$V1 <- NULL
nursery <- as.data.frame(apply(nursery, 2, FUN = function(x) as.numeric(gsub("\\W", "", x))), stringsAsFactors = FALSE)
names(nursery) <- c("Year", var_name)
setDF(nursery)
nursery <- melt(nursery, id = 1, variable.name = "COUNTYNAME", value.name = "Total_pop")
nursery$COUNTYNAME <- as.character(nursery$COUNTYNAME)
for(i in nl) nursery$COUNTYNAME[grepl(i, nursery$COUNTYNAME)] <- i

write.csv(nursery, "./data/result/nursery.csv", row.names = FALSE)
