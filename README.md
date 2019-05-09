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
* disposable_income.csv: The Average Disposable income of Taiwan in County level - the average disposable income of Taiwan in county level from 1998 to 2017(家庭收支 / 家庭收支-平均每戶可支配所得(元): https://statdb.dgbas.gov.tw/pxweb/Dialog/statfile9.asp). 

## Data (Generated)
The following files and folders are stored here: "leon-ps239T-final-project\shiny\data\result"

* Total_population.csv: generated from COUNTY.xlsx (Data (Raw))
* unemploymentrate.csv: generated from unemploymentrate.csv (Data (Raw))
* disposable_income.csv: generated from disposable_income.csv (Data (Raw))

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
