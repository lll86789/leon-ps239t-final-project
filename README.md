Author: Chih Cheng Lee (Leon)

Date: May 08, 2019

Project: Population of Taiwan

# Short Description
This is a shiny app for ploting and predicting the population of Taiwan. 
We can easily upload our own data with a standard format to plot or model. After modeling, we also can check our model result. 
The goal of this project is to let us do exploratory data analysis easier. 

# Dependencies
language: R

version.string: R version 3.5.1 (2018-07-02)

OS: Windows

# Files
## Data (Raw)
The following files and folders are stored here: "leon-ps239T-final-project\shiny\data"

* COUNTY.xlsx: The Population of Taiwan in County level - the population of Taiwan in county level from 1974 to 2018(01縣市人口按性別及五齡組(63): https://www.ris.gov.tw/app/portal/346). 
* unemploymentrate.csv: The Unemployment Rate of Taiwan in County level - the unemploymen rate of Taiwan in county level from 1998 to 2017(人力資源 / 失業率(％): https://statdb.dgbas.gov.tw/pxweb/Dialog/statfile9.asp). 
* disposable_income.csv: The Average Disposable income of Taiwan in County level - the average disposable income of Taiwan in county level from 1998 to 2017(家庭收支 / 平均每人每年可支配所得(元): https://statdb.dgbas.gov.tw/pxweb/Dialog/statfile9.asp). 
* hospital.csv: The Number of Hospital in Taiwan in County level - the number of hospital in Taiwan in county level from 1998 to 2017(醫療資源 / 醫療機構數(所): https://statdb.dgbas.gov.tw/pxweb/Dialog/statfile9.asp). 
* nursery.csv: The Number of Children in Nursery in Taiwan in County level - the number of children in nursery in Taiwan in county level from 1998 to 2017(少年、兒童福利服務 / 托兒所收托人數(人): https://statdb.dgbas.gov.tw/pxweb/Dialog/statfile9.asp). 
* garbage.csv: The Average amount of garbage generated by a person per Day in Taiwan in County level - the average amount of garbage generated by a person per day in Taiwan in county level from 1998 to 2017(環境保護 / 平均每人每日垃圾清運量(公斤): https://statdb.dgbas.gov.tw/pxweb/Dialog/statfile9.asp). 
* air_condition.csv: The Density of PM2.5 in Taiwan in County level - the density of PM2.5 in Taiwan in county level from 1998 to 2017(環境保護 / 空氣中總懸浮微粒濃度(微克／立方公尺): https://statdb.dgbas.gov.tw/pxweb/Dialog/statfile9.asp). 
* crime.csv: The Crime Rate of Taiwan in County level - the crime rate(per one hundred thousand) of Taiwan in county level from 1998 to 2017(社會治安 / 犯罪人口率(人／十萬人): https://statdb.dgbas.gov.tw/pxweb/Dialog/statfile9.asp). 

## Data (Generated)
The following files and folders are stored here: "leon-ps239T-final-project\shiny\data\result"

* Total_population.csv: generated from COUNTY.xlsx (Data (Raw))
* unemploymentrate.csv: generated from unemploymentrate.csv (Data (Raw))
* disposable_income.csv: generated from disposable_income.csv (Data (Raw))
* hospital.csv: generated from hospital.csv (Data (Raw))
* nursery.csv: generated from nursery.csv (Data (Raw))
* garbage.csv: generated from garbage.csv (Data (Raw))
* air_condition.csv: generated from air_condition.csv (Data (Raw))
* crime.csv: generated from crime.csv (Data (Raw))

## Code
The following scripts are stored here: "leon-ps239T-final-project\Scripts"

1. 00_Install_packages.R - Installs packages used in later files.
2. 01_Data_Processing.R - Generate usable data from the raw data. 

The following scripts are stored here: "leon-ps239T-final-project\shiny"

* ui.R - Shiny user interface script. 
* server.R - Shiny server script. 
* global.R - load data and packages in this script. 

# Results
Please open the Shiny app. 
There are 3 pages in the app. 

* File Upload - In this page you can upload your own data and train the model. 
* Interactive map - In this page you can do exploratory data analysis. 
* Data Exploration - In this page you can check you data. 

# More Infomation
## Standard format for uploading data
* Coulumns: Year, County Name, Values

## Website
