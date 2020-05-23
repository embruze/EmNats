
###################################################################################
# Author: EB/NL
# Last Updated: 5/14/2020
# Title: Covid-19 Overdose and Suicide, year 2020
# Modification log:
# 5/22/2020: N removed hard coding of SAH/NonE post variables from data calls
#            Removed duplicate read-in of Omaha, Evanston
# 5/23/2020: E added Sonoma County and added San Francisco suicide code '801'
###################################################################################

## clear work space
rm(list = ls())
options (scipen = 999)
options(max.print=99999)

## load libraries
library(readr)
library(readxl)
library(ggplot2)
library(tibble)
library(vctrs)
library(stringr)
library(RSocrata)
library(sf)
library(dplyr)
library(gmodels)
library(arsenal)
library(httr)
library(jsonlite)

## Read in 911/Police Report Data
## Each city is read in and cleaned separately to account for differences in coding in original data
## dataType indicates whether the data are listed as 911 calls or incident reports
## set indicates whether the data include suicides alone, overdoses alone, or both

# Phoenix, AZ
url <- "https://www.phoenixopendata.com/dataset/64a60154-3b2d-4583-8fb5-6d5e1b469c28/resource/3c0ae3ec-456f-45f4-801d-b8d6699ba32e/download/callsforsrvc2020.csv"
Phoenix <- read.csv(url)
Phoenix$CALL_RECEIVED <- substr(Phoenix$CALL_RECEIVED, start = 1, stop = 10)
Phoenix$date <- as.Date(Phoenix$CALL_RECEIVED, "%m/%d/%y")
Phoenix <- Phoenix[Phoenix$date >= "2020-02-01" & Phoenix$date < "2020-05-01",]
Phoenix$CallType <- as.factor(Phoenix$FINAL_CALL_TYPE)
Phoenix$overdose <- ifelse(grepl("overdose", Phoenix$FINAL_CALL_TYPE, ignore.case = TRUE), 1, 0)
Phoenix$suicide <- ifelse(grepl("suicid", Phoenix$FINAL_CALL_TYPE, ignore.case = TRUE), 1, 0)
Phoenix$hangup <- ifelse(grepl("9-1-1 HANG-UP CALL", Phoenix$FINAL_CALL_TYPE, ignore.case = TRUE), 1, 0)
Phoenix$state <- "AZ"
Phoenix$city <- "Phoenix"
Phoenix$dataType <- "calls911"
Phoenix$set <- "Both"
Phoenix <- select(Phoenix, set, state, city, dataType, date, CallType, overdose, suicide, hangup)
Phoenix <- Phoenix %>%
  mutate_all(as.character)

# Santa Monica, CA
url <- "https://data.smgov.net/resource/ia9m-wspt.json"
SantaMonica <- read.socrata(url)
SantaMonica$date <- as.Date(SantaMonica$incident_date, "%m/%d/%y")
SantaMonica <- SantaMonica[SantaMonica$date >= "2020-02-01" & SantaMonica$date < "2020-05-01",]
SantaMonica$CallType <- as.factor(SantaMonica$call_type)
SantaMonica$overdose <- ifelse(grepl("overdose", SantaMonica$CallType, ignore.case = TRUE), 1, 0)
SantaMonica$suicide <- ifelse(grepl("suicid", SantaMonica$CallType, ignore.case = TRUE), 1, 0)
SantaMonica$hangup <- ifelse(grepl("9-1-1 Hang Up", SantaMonica$CallType, ignore.case = TRUE), 1, 0)
SantaMonica$state <- "CA"
SantaMonica$city <- "SantaMonica"
SantaMonica$dataType <- "calls911"
SantaMonica$set <- "Both"
SantaMonica <- select(SantaMonica, set, state, city, dataType, date, CallType, overdose, suicide, hangup)
SantaMonica <- SantaMonica %>%
  mutate_all(as.character)

#St. Petersburg, FL
url <- "https://stat.stpete.org/resource/2eks-pg5j.json"
StPete <- read.socrata(url)
StPete$date <- StPete$crime_date
StPete$date <- substr(StPete$date, start = 1, stop = 10)
StPete <- StPete[StPete$date >= "2020-02-01" & StPete$date < "2020-05-01",]
StPete$CallType <- as.factor(StPete$type_of_engagement)
StPete$overdose <- ifelse(grepl("overdose", StPete$CallType, ignore.case = TRUE), 1, 0)
StPete$suicide <- ifelse(grepl("suicid", StPete$CallType, ignore.case = TRUE), 1, 0)
StPete$hangup <- ifelse(grepl("HANGUP", StPete$CallType, ignore.case = TRUE), 1, 0)
StPete$state <- "FL"
StPete$city <- "StPete"
StPete$dataType <- "calls911"
StPete$set <- "Both"
StPete <- select(StPete, set, state, city, dataType, date, CallType, overdose, suicide, hangup)
StPete <- StPete %>%
  mutate_all(as.character)

#Evanston, IL
#https://data.cityofevanston.org/api/views/rfe6-b4mt/rows.csv?accessType=DOWNLOAD
url <- "https://data.cityofevanston.org/resource/rfe6-b4mt.json"
Evanston <- read.socrata(url)
Evanston$date <- Evanston$call_received_date_time
Evanston$date <- substr(Evanston$date, start = 1, stop = 10)
Evanston <- Evanston[Evanston$date >= "2020-02-01" & Evanston$date < "2020-05-01",]
Evanston$CallType <- as.factor(Evanston$nature_code_description)
Evanston$overdose <- ifelse(grepl("overdose", Evanston$CallType, ignore.case = TRUE), 1, 0)
Evanston$suicide <- ifelse(grepl("suicid", Evanston$CallType, ignore.case = TRUE), 1, 0)
Evanston$hangup <- NA
Evanston$state <- "IL"
Evanston$city <- "Evanston"
Evanston$dataType <- "calls911"
Evanston$set <- "Both"
Evanston <- select(Evanston, set, state, dataType, city, date, CallType, overdose, suicide, hangup)
Evanston <- Evanston %>%
  mutate_all(as.character)

#Griffith, IN
url <- "https://www.openpolicedata.com/GriffithIN/Data/CFS/2020CFS.csv"
Griffith <- read.csv(url)
Griffith$date <-Griffith$Date
Griffith$date <- as.Date(Griffith$date, "%m/%d/%y")
Griffith <- Griffith[Griffith$date >= "2020-02-01" & Griffith$date < "2020-05-01",]
Griffith$CallType <- as.factor(Griffith$Nature)
Griffith$overdose <- ifelse(grepl("overdose", Griffith$CallType, ignore.case = TRUE), 1, 0)
Griffith$suicide <- ifelse(grepl("suicid", Griffith$CallType, ignore.case = TRUE), 1, 0)
Griffith$hangup <- ifelse(grepl("911 HANGUP", Griffith$CallType, ignore.case = TRUE), 1, 0)
Griffith$state <- "IN"
Griffith$city <- "Griffith"
Griffith$dataType <- "calls911"
Griffith$set <- "Both"
Griffith <- select(Griffith, set, state, city, dataType, date, CallType, overdose, suicide, hangup)
Griffith <- Griffith %>%
  mutate_all(as.character)

#Louisville, KY
url <- "https://data.louisvilleky.gov/sites/default/files/27091/Crime_Data_2019.csv"
Louisville <- read.csv(url)
Louisville$date <- substr(Louisville$DATE_REPORTED, start = 1, stop = 10)
Louisville <- Louisville[Louisville$date >= "2020-02-01" & Louisville$date < "2020-05-01",]
Louisville$CallType <- as.factor(Louisville$UOR_DESC)
Louisville$overdose <- ifelse(grepl("overdose", Louisville$CallType, ignore.case = TRUE), 1, 0)
Louisville$suicide <- ifelse(grepl("suicid", Louisville$CallType, ignore.case = TRUE), 1, 0)
Louisville$hangup <- NA
Louisville$state <- "KY"
Louisville$city <- "Louisville"
Louisville$dataType <- "pdIncident"
Louisville$set <- "Both"
Louisville <- select(Louisville, set, state, city, dataType, date, CallType, overdose, suicide, hangup)
Louisville <- Louisville %>%
  mutate_all(as.character)

#Baltimore, MD	
#NOTE: Confirm path for hard coded data before running
Baltimore <- read_csv("Desktop/Papers/COVID/Overdose_Suicide_paper/HardCoded/Baltimore.csv")
Baltimore$date <- substr(Baltimore$CallDateTime, start = 1, stop = 10)
Baltimore$date <- as.Date(Baltimore$date, "%m/%d/%y")
Baltimore <- Baltimore[Baltimore$date >= "2020-02-01" & Baltimore$date < "2020-05-01",]
Baltimore$CallType <- as.factor(Baltimore$Description)
Baltimore$overdose <- ifelse(grepl("overdose", Baltimore$CallType, ignore.case = TRUE), 1, 0)
Baltimore$suicide <- NA
Baltimore$hangup <- ifelse(grepl("911/NO VOICE", Baltimore$CallType, ignore.case = TRUE), 1, 0)
Baltimore$hangup <- ifelse(grepl("911/HANGUP", Baltimore$CallType, ignore.case = TRUE), 1, paste(Baltimore$hangup))
Baltimore$hangup <- ifelse(grepl("FALSE CALL", Baltimore$CallType, ignore.case = TRUE), 1, paste(Baltimore$hangup))
Baltimore$hangup <- ifelse(grepl("911 PHONE MISUSE", Baltimore$CallType, ignore.case = TRUE), 1, paste(Baltimore$hangup))
Baltimore$state <- "MD"
Baltimore$city <- "Baltimore"
Baltimore$dataType <- "calls911"
Baltimore$set <- "OverdoseOnly"
Baltimore <- select(Baltimore, set, state, city, date, dataType, CallType, overdose, suicide, hangup)
Baltimore <- Baltimore %>%
  mutate_all(as.character)

#Detroit, MI
#NOTE: Confirm path for hard coded data before running
#url <- "https://opendata.arcgis.com/datasets/2dab2f70653f4bb8b4f2b51619ec8329_0.csv"
#Detroit <- read.csv(url)
Detroit <- read_csv("Desktop/Papers/COVID/Overdose_Suicide_paper/HardCoded/Detroit.csv")
Detroit <- select(Detroit, zip_code, calldescription, category, call_timestamp, intaketime)
Detroit$date <- as.Date(Detroit$call_timestamp)
Detroit$date <- substr(Detroit$date, start = 1, stop = 10)
Detroit <- Detroit[Detroit$date >= "2020-02-01" & Detroit$date < "2020-05-01",]
Detroit$CallType <- as.factor(Detroit$calldescription) ##ONE DOWN/DRUG OD, ONE DOWN/OVERDOSE,OVERDOSE NT ALRT OR UNK STATUS
Detroit$overdose <- ifelse(grepl("overdose", Detroit$CallType, ignore.case = TRUE), 1, 0)
Detroit$overdose <- ifelse(grepl("DRUG OD", Detroit$CallType, ignore.case = TRUE), 1, paste(Detroit$overdose))
Detroit$suicide <- ifelse(grepl("suicid", Detroit$CallType, ignore.case = TRUE), 1, 0)
Detroit$hangup <- ifelse(grepl("HANGUP CALLS", Detroit$CallType, ignore.case = TRUE), 1, 0)
Detroit$state <- "MI"
Detroit$city <- "Detroit"
Detroit$dataType <- "calls911"
Detroit$overdose <- as.numeric(Detroit$overdose)
Detroit$set <- "Both"
Detroit$date <- as.character(Detroit$date) #Error: Column `date` can't be converted from Date to character
Detroit <- select(Detroit, set, state, city, dataType, date, CallType, overdose, suicide, hangup)
Detroit <- Detroit %>%
  mutate_all(as.character)

# Fargo, ND, MAKE SURE TO INCLUDE CSV
#NOTE: Confirm path for hard coded data before running
Fargo <- read_csv("Desktop/Papers/COVID/Overdose_Suicide_paper/HardCoded/Fargo.csv")
Fargo$date <- as.Date(Fargo$DateTime, "%m/%d/%y")
Fargo <- Fargo[Fargo$date >= "2020-02-01" & Fargo$date < "2020-05-01",]
Fargo$CallType <- as.factor(Fargo$Desctiption) #Typo is in original file
Fargo$overdose <- ifelse(grepl("overdose", Fargo$CallType, ignore.case = TRUE), 1, 0)
Fargo$overdose <- ifelse(Fargo$overdose=="OD", 1, paste(Fargo$overdose))
Fargo$overdose <- ifelse(Fargo$overdose=="POSS OD", 1, paste(Fargo$overdose))#POSS UNINTENTIONAL OD, #OD, POSS OD
Fargo$overdose <- ifelse(Fargo$overdose=="POSS UNINTENTIONAL OD", 1, paste(Fargo$overdose))
Fargo$suicide <- ifelse(grepl("suicid", Fargo$CallType, ignore.case = TRUE), 1, 0)
Fargo$hangup <- ifelse(grepl("911 HANG UP", Fargo$CallType, ignore.case = TRUE), 1, 0)
Fargo$state <- "ND"
Fargo$city <- "Fargo"
Fargo$dataType <- "calls911"
Fargo$overdose <- as.numeric(Fargo$overdose)
Fargo$set <- "Both"
Fargo <- select(Fargo, set, state, city, date, dataType, CallType, overdose, suicide, hangup)
Fargo <- Fargo %>%
  mutate_all(as.character)

#Omaha, NE 
url <- "https://police-static.cityofomaha.org/crime-data/2020/Incidents_2020.csv"
Omaha <- read.csv(url)
Omaha$date <- Omaha$Reported.Date
Omaha$date <- as.Date(Omaha$date, "%m/%d/%y")
Omaha <- Omaha[Omaha$date >= "2020-02-01" & Omaha$date < "2020-05-01",]
Omaha$CallType <- as.factor(Omaha$Statute.Ordinance.Description) 
Omaha$overdose <- ifelse(grepl("overdose", Omaha$CallType, ignore.case = TRUE), 1, 0)
Omaha$overdose <- ifelse(grepl("DRUG OD", Omaha$CallType, ignore.case = TRUE), 1, paste(Omaha$overdose))
Omaha$suicide <- ifelse(grepl("suicid", Omaha$CallType, ignore.case = TRUE), 1, 0)
Omaha$hangup <- NA
Omaha$state <- "NE"
Omaha$city <- "Omaha"
Omaha$dataType <- "pdIncident"
Omaha$set <- "Both"
Omaha <- select(Omaha, set, state, city, date, dataType, CallType, overdose, suicide, hangup)
Omaha <- Omaha %>%
  mutate_all(as.character)

#Cincinnati, OH
url <- "https://data.cincinnati-oh.gov/resource/vnsz-a3wp.json"
Cincinnati <- read.socrata(url)
Cincinnati$date <- Cincinnati$create_time_incident
Cincinnati$date <- as.Date(Cincinnati$date, "%m/%d/%y")
Cincinnati <- Cincinnati[Cincinnati$date >= "2020-02-01" & Cincinnati$date < "2020-05-01",]
Cincinnati$CallType <- as.factor(Cincinnati$cfd_incident_type_group) 
Cincinnati$overdose <- ifelse(grepl("overdose", Cincinnati$CallType, ignore.case = TRUE), 1, 0)
Cincinnati$suicide <- ifelse(grepl("SUIC", Cincinnati$CallType, ignore.case = TRUE), 1, 0)
Cincinnati$hangup <- NA
Cincinnati$state <- "OH"
Cincinnati$city <- "Cincinnati"
Cincinnati$dataType <- "fireEMSIncident"
Cincinnati$set <- "Both"
Cincinnati <- select(Cincinnati, set, state, city, date, dataType, CallType, overdose, suicide, hangup)
Cincinnati <- Cincinnati %>%
  mutate_all(as.character)

#Ashville, NC (API is SLOW) - CSV is also written
#NOTE: Confirm path for hard coded data before running
#geo_url <- "https://arcgis.ashevillenc.gov/arcgis/rest/services/PublicSafety/APDIncidents/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json"
#Ashville <- read_sf(geo_url) 
#st_geometry(Ashville) <- NULL
Ashville <- read_csv("Desktop/Papers/COVID/Overdose_Suicide_paper/HardCoded/Ashville.csv")
Ashville$date <- Ashville$date_occurred
Ashville$date <- as.Date.character(Ashville$date, "%Y%m%d")
Ashville <- Ashville[Ashville$date >= "2020-02-01" & Ashville$date < "2020-05-01",]
Ashville$CallType <- as.factor(Ashville$offense_long_description) 
Ashville$overdose <- ifelse(grepl("overdose", Ashville$CallType, ignore.case = TRUE), 1, 0)
Ashville$suicide <- NA
Ashville$hangup <- NA
Ashville$state <- "NC"
Ashville$city <- "Ashville"
Ashville$dataType <- "pdIncident"
Ashville$set <- "OverdoseOnly"
Ashville <- select(Ashville, set, state, city, date, dataType, CallType, overdose, suicide, hangup)
Ashville <- Ashville %>%
  mutate_all(as.character)

#Seattle, WA https://data.seattle.gov/api/views/33kz-ixgy/rows.csv?accessType=DOWNLOAD
#NOTE: Confirm path for hard coded data before running
#url <- "https://data.seattle.gov/resource/33kz-ixgy.json"
#Seattle <- read.socrata(url)
Seattle <- read_csv("Desktop/Papers/COVID/Overdose_Suicide_paper/HardCoded/Seattle.csv")
Seattle$date <- Seattle$original_time_queued
Seattle <- filter(Seattle, grepl("2020", date, ignore.case = TRUE))
Seattle$date <- as.Date(Seattle$date, "%m/%d/%y")
Seattle <- Seattle[Seattle$date >= "2020-02-01" & Seattle$date < "2020-05-01",]
Seattle$CallType <- as.factor(Seattle$initial_call_type) 
Seattle$overdose <- ifelse(grepl("overdose", Seattle$CallType, ignore.case = TRUE), 1, 0)
Seattle$suicide <- ifelse(grepl("SUIC", Seattle$CallType, ignore.case = TRUE), 1, 0)
Seattle$hangup <- ifelse(grepl("open line", Seattle$CallType, ignore.case = TRUE), 1, 0)
Seattle$state <- "WA"
Seattle$city <- "Seattle"
Seattle$dataType <- "calls911"
Seattle$set <- "Both"
Seattle <- select(Seattle, set, state, city, date, dataType, CallType, overdose, suicide, hangup)
Seattle <- Seattle %>%
  mutate_all(as.character)

#Milwaukee, WI
url <- "https://data.milwaukee.gov/dataset/9b425ad5-c5b9-4f3d-8c52-579806ff3f7e/resource/662070a0-eadc-4328-b0f5-e8e73531c8e2/download/mfdems.csv"
Milwaukee <- read.csv(url)
Milwaukee$date <- Milwaukee$Incident.Date
Milwaukee$date <- as.Date(Milwaukee$date)
Milwaukee <- Milwaukee[Milwaukee$date >= "2020-02-01" & Milwaukee$date < "2020-05-01",]
Milwaukee$CallType <- as.factor(Milwaukee$Final.Call.for.Service.Type) 
Milwaukee$overdose <- ifelse(grepl("overdose", Milwaukee$CallType, ignore.case = TRUE), 1, 0)
Milwaukee$suicide <- ifelse(grepl("SUIC", Milwaukee$CallType, ignore.case = TRUE), 1, 0)
Milwaukee$hangup <- NA
Milwaukee$state <- "WI"
Milwaukee$city <- "Milwaukee"
Milwaukee$dataType <- "fireEMSIncident"
Milwaukee$set <- "Both"
Milwaukee <- select(Milwaukee, set, state, city, date, dataType, CallType, overdose, suicide, hangup)
Milwaukee <- Milwaukee %>%
  mutate_all(as.character)

#Madison, WI, EXCLUDED, HAS NEITHER SUICIDE NOR OD RECORDED IN PERIOD OF INTEREST
#url <- "https://opendata.arcgis.com/datasets/61c36ee8e2d14cd094a265a288e27151_2.csv"
#Madison <- read.csv(url)
#Madison$date <- as.Date(Madison$IncidentDate)
#Madison <- Madison[Madison$date >= "2020-02-01" & Madison$date < "2020-05-01",]
#Madison$CallType <- as.factor(Madison$Details) 
#Madison$overdose <- ifelse(grepl("overdose", Madison$CallType, ignore.case = TRUE), 1, 0)
#Madison$suicide <- ifelse(grepl("SUIC", Madison$CallType, ignore.case = TRUE), 1, 0)
#Madison$hangup <- NA
#Madison$state <- "WI"
#Madison$city <- "Madison"
#Madison$dataType <- "pdIncident"
#Madison$set <- "Both"
#Madison <- select(Madison, set, state, city, date, dataType, CallType, overdose, suicide, hangup)

#New Orleans, LA, NALOXONE ADMINISTRATION BUT NOT OVERDOSE?
url <- "https://data.nola.gov/resource/hp7u-i9hf.json"
NewOrleans <- read.socrata(url)
NewOrleans$date <- as.character(NewOrleans$timecreate)
NewOrleans$date <- substr(NewOrleans$date, start = 1, stop = 10)
NewOrleans$date <- as.character.Date(NewOrleans$date, format="%m/%d/%y")
NewOrleans <- NewOrleans[NewOrleans$date >= "2020-02-01" & NewOrleans$date < "2020-05-01",]
NewOrleans$CallType <- paste(NewOrleans$typetext, NewOrleans$initialtypetext, sep = " ")
NewOrleans$overdose <- ifelse(grepl("NALOXONE", NewOrleans$CallType, ignore.case = TRUE), 1, 0)
NewOrleans$suicide <- ifelse(grepl("SUIC", NewOrleans$CallType, ignore.case = TRUE), 1, 0)
NewOrleans$hangup <- ifelse(grepl("SILENT 911", NewOrleans$CallType, ignore.case = TRUE), 1, 0)
NewOrleans$state <- "LA"
NewOrleans$city <- "NewOrleans"
NewOrleans$dataType <- "calls911"
NewOrleans$set <- "Both"
NewOrleans <- select(NewOrleans, set, state, city, date, dataType, CallType, overdose, suicide, hangup)
NewOrleans <- NewOrleans %>%
  mutate_all(as.character)

#Atanta, GA
url <- "https://opendata.arcgis.com/datasets/f51383e693ef43e2bd5372be1e38ace0_0.csv"
Atlanta <- read.csv(url)
Atlanta$dataType <- "calls911"
Atlanta$state <- "GA"
Atlanta$city <- "Atlanta"
Atlanta$date <- substr(Atlanta$CallDateTime, start = 1, stop = 10)
Atlanta$date <- as.Date(Atlanta$date, format="%Y/%m/%d")
Atlanta <- Atlanta[Atlanta$date >= "2020-02-01" & Atlanta$date < "2020-05-01",]
Atlanta$CallType <- as.factor(Atlanta$NatureOfCall)
Atlanta$overdose <- NA
Atlanta$suicide <- ifelse(grepl("SUIC", Atlanta$CallType, ignore.case = TRUE), 1, 0)
Atlanta$hangup <- ifelse(grepl("911 HANG UP", Atlanta$CallType, ignore.case = TRUE), 1, 0)
Atlanta$set <- "SuicOnly"
Atlanta <- select(Atlanta, set, state, city, date, dataType, CallType, overdose, suicide, hangup)
Atlanta <- Atlanta %>%
  mutate_all(as.character)

#Charleston, SC, EXCLUDED data last updated 3/31/20
#url <- "https://www.charleston-sc.gov/DocumentCenter/View/26416/2020CAD_YTD_CSV"
#Charleston <- read.csv(url)
#Charleston$dataType <- "calls911"
#Charleston$state <- "SC"
#Charleston$city <- "Charleston"
#Charleston$date <- as.Date(Charleston$Response_Date, format="%d/%m/%Y")
#Charleston <- Charleston[Charleston$date >= "2020-02-01" & Charleston$date < "2020-05-01",]

#Kansas city, MO
url <- "https://data.kcmo.org/resource/vsgj-uufz.json"
KansasCity <- read.socrata(url)
KansasCity$dataType <- "pdIncident"
KansasCity$state <- "MO"
KansasCity$city <- "KansasCity"
KansasCity$date <- as.Date(KansasCity$reported_date, format="%Y-%m-%d")
KansasCity <- KansasCity[KansasCity$date >= "2020-02-01" & KansasCity$date < "2020-05-01",]
KansasCity$CallType <- as.factor(KansasCity$offense)
KansasCity$overdose <- NA
KansasCity$suicide <- ifelse(grepl("SUIC", KansasCity$CallType, ignore.case = TRUE), 1, 0)
KansasCity$hangup <- NA
KansasCity$set <- "SuicOnly"
KansasCity <- select(KansasCity, set, state, city, date, dataType, CallType, overdose, suicide, hangup)
KansasCity <- KansasCity %>%
  mutate_all(as.character)

#Marin County, EXCLUDED no suicides or overdoses in period
#url <- "https://data.marincounty.org/resource/ahxi-5nsc.json"
#Marin <- read.socrata(url)
#Marin$dataType <- "pdIncident"
#Marin$state <- "CA"
#Marin$city <- "Marin"
#Marin$date <- as.Date(Marin$incident_date_time, format="%Y-%m-%d")
#Marin <- Marin[Marin$date >= "2020-02-01" & Marin$date < "2020-05-01",]

#Nashville, TN, EXCLUDED last updated 4/14/20
#url <- "https://data.nashville.gov/resource/nhhg-pnxf.json"
#Nashville <- read.socrata(url)
#Nashville$dataType <- "calls911"
#Nashville$state <- "TN"
#Nashville$city <- "Nashville"
#Nashville$date <- as.Date(Nashville$call_received, format="%Y-%m-%d")
#Nashville <- Nashville[Nashville$date >= "2020-02-01" & Nashville$date < "2020-05-01",]

#Chandler, AZ, EXCLUDED due to API throttling making it impossible to read in or download data
#url <- "https://data.chandlerpd.com/wp-json/open-data/v1/calls-for-service-2020?per_page=100"
#pages <- list()
#for(i in 0:490){
#  mydata <- fromJSON(paste0(url, "&page=", i), flatten=TRUE)
#  message("Retrieving page ", i)
#  pages[[i+1]] <- mydata
#}
#Chandler <- rbind_pages(pages[sapply(pages, length)>0])
#Chandler$dataType <- "calls911"
#Chandler$state <- "AZ"
#Chandler$city <- "Chandler"

#Montgomery, AL, EXCLUDED last updated, 3/31/20, something weird is happening with the dates
#url <- "https://opendata.arcgis.com/datasets/f43b4d1fd1cb4ee5a6a144581174cb41_0.csv"
#Montgomery <- read.csv(url)
#Montgomery$dataType <- "pdIncident"
#Montgomery$state <- "AL"
#Montgomery$city <- "Montgomery"
#Montgomery <- filter(Montgomery, Year==2020)
#Montgomery$date <- as.Date(Montgomery$Case_Res, format="%Y/%m/%d")
#Montgomery <- Montgomery[Montgomery$date >= "2020-02-01" & Montgomery$date < "2020-05-01",]

#St. Paul, MN (Ramsey County)
#https://opendata.ramseycounty.us/api/views/khr9-xwfu/rows.csv?accessType=DOWNLOAD
url <- "https://opendata.ramseycounty.us/resource/khr9-xwfu.json"
StPaul <- read.socrata(url)
StPaul$dataType <- "calls911"
StPaul$state <- "MN"
StPaul$city <- "StPaul"
StPaul$year <- substr(StPaul$response_date, start = 1, stop = 4)
StPaul <- filter(StPaul, year=="2020")
StPaul$date <- substr(StPaul$response_date, start = 1, stop = 10)
StPaul <- StPaul[StPaul$date >= "2020-02-01" & StPaul$date < "2020-05-01",]
StPaul$CallType <- as.factor(StPaul$problem)
StPaul$overdose <- NA
StPaul$suicide <- ifelse(grepl("SUIC", StPaul$CallType, ignore.case = TRUE), 1, 0)
StPaul$hangup <- ifelse(grepl("Hangup", StPaul$CallType, ignore.case = TRUE), 1, 0)
StPaul$set <- "SuicOnly"
StPaul <- select(StPaul, set, state, city, date, dataType, CallType, overdose, suicide, hangup)
StPaul <- StPaul %>%
  mutate_all(as.character)

#Minneapolis, MN, EXCLUDED no suic or overdose
#url <- "https://information.stpaul.gov/resource/gppb-g9cg.json"
#Minneapolis <- read.socrata(url)
#Minneapolis$dataType <- "pdIncident"
#Minneapolis$state <- "MN"
#Minneapolis$city <- "Minneapolis"
#Minneapolis <- Minneapolis[Minneapolis$date >= "2020-02-01" & Minneapolis$date < "2020-05-01",]
#Minneapolis$date <- substr(Minneapolis$date, start = 1, stop = 10)
#Minneapolis$CallType <- as.factor(Minneapolis$incident_type)
#Minneapolis$overdose <- NA
#Minneapolis$suicide <- ifelse(grepl("SUIC", Minneapolis$CallType, ignore.case = TRUE), 1, 0)
#Minneapolis$hangup <- ifelse(grepl("Hangup", Minneapolis$CallType, ignore.case = TRUE), 1, 0)

#Ann Arbor, MI
#NOTE: Confirm path for hard coded data before running
AnnArbor <- read_csv("Desktop/Papers/COVID/Overdose_Suicide_paper/HardCoded/AnnArbor.csv")
AnnArbor$dataType <- "calls911"
AnnArbor$state <- "MI"
AnnArbor$city <- "AnnArbor"
AnnArbor$date <- as.character.Date(AnnArbor$CALLDATE)
AnnArbor$date <- as.Date(AnnArbor$date,format="%m/%d/%y") 
AnnArbor <- AnnArbor[AnnArbor$date >= "2020-02-01" & AnnArbor$date < "2020-05-01",]
AnnArbor$CallType <- as.factor(AnnArbor$REPORTED_OFF_DESC)
AnnArbor$overdose <- NA
AnnArbor$suicide <- ifelse(grepl("SUIC", AnnArbor$CallType, ignore.case = TRUE), 1, 0)
AnnArbor$hangup <- ifelse(grepl("911 HANG UP", AnnArbor$CallType, ignore.case = TRUE), 1, 0)
AnnArbor$set <- "SuicOnly"
AnnArbor <- select(AnnArbor, set, state, city, date, dataType, CallType, overdose, suicide, hangup)
AnnArbor <- AnnArbor %>%
  mutate_all(as.character)

#Fort Wayne, IN
#NOTE: Confirm path for hard coded data before running
FortWayne <- read_csv("Desktop/Papers/COVID/Overdose_Suicide_paper/HardCoded/FortWayne.csv")
FortWayne$dataType <- "calls911"
FortWayne$state <- "IN"
FortWayne$city <- "FortWayne"
FortWayne$date <- as.Date(FortWayne$Date, format="%m/%d/%y") 
FortWayne <- FortWayne[FortWayne$date >= "2020-02-01" & FortWayne$date < "2020-05-01",]
FortWayne$CallType <- as.factor(FortWayne$Nature)
FortWayne$overdose <- NA
FortWayne$suicide <- ifelse(grepl("SUIC", FortWayne$CallType, ignore.case = TRUE), 1, 0)
FortWayne$hangup <- ifelse(grepl("911 HANG UP", FortWayne$CallType, ignore.case = TRUE), 1, 0)
FortWayne$set <- "SuicOnly"
FortWayne <- select(FortWayne, set, state, city, date, dataType, CallType, overdose, suicide, hangup)
FortWayne <- FortWayne %>%
  mutate_all(as.character)

#Hartford, CT
url <- "https://data.hartford.gov/resource/9a5q-r34k.json"
Hartford <- read.socrata(url)
Hartford$dataType <- "calls911"
Hartford$state <- "CT"
Hartford$city <- "Hartford"
Hartford <- Hartford[Hartford$date >= "2020-02-01" & Hartford$date < "2020-05-01",]
Hartford$CallType <- as.factor(Hartford$description_1)
Hartford$overdose <- NA
Hartford$suicide <- ifelse(grepl("SUIC", Hartford$CallType, ignore.case = TRUE), 1, 0)
Hartford$hangup <- NA
Hartford$set <- "SuicOnly"
Hartford <- select(Hartford, set, state, city, date, dataType, CallType, overdose, suicide, hangup)
Hartford <- Hartford %>%
  mutate_all(as.character)

#Albany, NY
url <- "https://data.albanyny.gov/resource/m4jx-di39.json"
Albany <- read.socrata(url)
Albany$dataType <- "calls911"
Albany$state <- "NY"
Albany$city <- "Albany"
Albany$date <- Albany$call_date
Albany <- Albany[Albany$date >= "2020-02-01" & Albany$date < "2020-05-01",]
Albany$CallType <- as.factor(Albany$call_type)
Albany$overdose <- NA
Albany$suicide <- ifelse(grepl("SUIC", Albany$CallType, ignore.case = TRUE), 1, 0)
Albany$hangup <- ifelse(grepl("911", Albany$CallType, ignore.case = TRUE), 1, 0)
Albany$set <- "SuicOnly"
Albany$date <- as.Date(Albany$date)
Albany <- select(Albany, set, state, city, date, dataType, CallType, overdose, suicide, hangup)
Albany <- Albany %>%
  mutate_all(as.character)

#San Francisco, CA
#NOTE: Confirm path for hard coded data before running
SanFrancisco <- read_csv("Desktop/Papers/COVID/Overdose_Suicide_paper/HardCoded/SanFrancisco.csv")
SanFrancisco$dataType <- "calls911"
SanFrancisco$state <- "CA"
SanFrancisco$city <- "SanFrancisco"
SanFrancisco$date <- SanFrancisco$'Call Date'
SanFrancisco$date <- as.character.Date(SanFrancisco$date)
SanFrancisco$date <- as.Date(SanFrancisco$date, format="%m/%d/%y")
SanFrancisco <- SanFrancisco[SanFrancisco$date >= "2020-02-01" & SanFrancisco$date < "2020-05-01",]
SanFrancisco$CallType <- as.factor(SanFrancisco$`Original Crime Type Name`)
SanFrancisco$overdose <- ifelse(SanFrancisco$CallType=="Od", 1, 0)
SanFrancisco$overdose <- ifelse(SanFrancisco$CallType=="Od Pt", 1, paste(SanFrancisco$overdose))
SanFrancisco$overdose <- ifelse(SanFrancisco$CallType=="Fentanyl Od", 1, paste(SanFrancisco$overdose))
SanFrancisco$overdose <- ifelse(SanFrancisco$CallType=="Overdose", 1, paste(SanFrancisco$overdose))
SanFrancisco$suicide <- ifelse(grepl("SUIC", SanFrancisco$CallType, ignore.case = TRUE), 1, 0)
SanFrancisco$suicide <- ifelse(grepl("801", SanFrancisco$CallType, ignore.case = TRUE), 1, SanFrancisco$suicide)
SanFrancisco$hangup <- ifelse(grepl("911", SanFrancisco$CallType, ignore.case = TRUE), 1, 0)
SanFrancisco$set <- "Both"
SanFrancisco <- select(SanFrancisco, set, state, city, date, dataType, CallType, overdose, suicide, hangup)
SanFrancisco <- SanFrancisco %>%
  mutate_all(as.character)

#Los Angeles, CA
#NOTE: Confirm path for hard coded data before running
LosAngeles <- read_csv("Desktop/Papers/COVID/Overdose_Suicide_paper/HardCoded/LosAngeles.csv")
LosAngeles$dataType <- "calls911"
LosAngeles$state <- "CA"
LosAngeles$city <- "LosAngeles"
LosAngeles$date <- as.character.Date(LosAngeles$Dispatch_Date)
LosAngeles$date <- as.Date(LosAngeles$date, format="%m/%d/%y")
LosAngeles <- LosAngeles[LosAngeles$date >= "2020-02-01" & LosAngeles$date < "2020-05-01",]
LosAngeles$CallType <- as.factor(LosAngeles$Call_Type_Text)
LosAngeles$overdose <- ifelse(grepl("O/D", LosAngeles$CallType, ignore.case = TRUE), 1, 0)
LosAngeles$suicide <- ifelse(grepl("SUIC", LosAngeles$CallType, ignore.case = TRUE), 1, 0)
LosAngeles$hangup <- NA
LosAngeles$set <- "Both"
LosAngeles <- select(LosAngeles, set, state, city, dataType, date, CallType, overdose, suicide, hangup)
LosAngeles <- LosAngeles %>%
  mutate_all(as.character)

# Mesa, AZ
# Overdose data provided in a separate table, need to read in both
url <- "https://data.mesaaz.gov/resource/4k95-x7aw.json"
MesaFire <- read.socrata(url)
url <- "https://data.mesaaz.gov/resource/qufy-tzv6.json"
MesaOD <- read.socrata(url)
MesaFire$incident_date <- MesaFire$creation_date_time
MesaFire$incident_disposition <- MesaFire$event_type_description
MesaFire$month <- MesaFire$creation_month
MesaFire$year <- MesaFire$creation_year
MesaOD$incident_disposition <- paste("overdose", MesaOD$incident_disposition, sep="")
MesaOD <- select(MesaOD, incident_date, incident_disposition, month, year)
MesaFire <- select(MesaFire, incident_date, incident_disposition, month, year)
Mesa <- bind_rows(MesaFire, MesaOD)
Mesa$dataType <- "fireEMSIncident"
Mesa$state <- "AZ"
Mesa$city <- "Mesa"
Mesa$date <- as.Date(Mesa$incident_date, format="%m/%d/%y")
Mesa <- filter(Mesa, year==2020)
Mesa <- filter(Mesa, month!="Jan")
Mesa <- filter(Mesa, month!="May")
Mesa$CallType <- as.factor(Mesa$incident_disposition)
Mesa$suicide <- ifelse(grepl("SUIC", Mesa$CallType, ignore.case = TRUE), 1, 0)
Mesa$overdose <- ifelse(grepl("overdose", Mesa$CallType, ignore.case = TRUE), 1, 0)
Mesa$hangup <- NA
Mesa$set <- "Both"
Mesa <- select(Mesa, set, state, city, dataType, date, CallType, overdose, suicide, hangup)
Mesa <- Mesa %>%
  mutate_all(as.character)

#Salt Lake, UT
SaltLake <- read.csv("https://opendata.utah.gov/api/views/7gdm-xsej/rows.csv?accessType=DOWNLOAD")
SaltLake$date <- as.Date(SaltLake$OCC.DATE, format="%m/%d/%Y")
SaltLake <- SaltLake[SaltLake$date >= "2020-02-01" & SaltLake$date < "2020-05-01",]
SaltLake$CallType <- as.factor(SaltLake$CALL.TYPE)
SaltLake$hangup <- ifelse(SaltLake$CallType=="911 HANGUP CALL", 1, 0)
SaltLake$hangup <- ifelse(SaltLake$CallType=="911 HANG UP TEXT", 1, paste(SaltLake$hangup))
SaltLake$hangup <- ifelse(SaltLake$CallType=="CELL 911 HANGUP", 1, paste(SaltLake$hangup))
SaltLake$suicide <- ifelse(grepl("SUICID", SaltLake$CallType, ignore.case = TRUE), 1, 0)
SaltLake$overdose <- ifelse(grepl("OVERDOSE", SaltLake$CallType, ignore.case = TRUE), 1, 0)
SaltLake$state <- "UT"
SaltLake$city <- "SaltLake"
SaltLake$dataType <- "calls911"
SaltLake$set <- "Both"
SaltLake <- select(SaltLake, set, state, city, dataType, date, CallType, overdose, suicide, hangup)
SaltLake <- SaltLake %>%
  mutate_all(as.character)

# Santa Rosa, CA
SantaRosa <- read.csv("https://opendata.arcgis.com/datasets/73c68826a06a405e87161e609a08947b_0.csv")
SantaRosa$date <- as.character.Date(SantaRosa$Date, format="%m/%d/%Y")
SantaRosa$date <- as.Date(SantaRosa$date)
SantaRosa <- SantaRosa[SantaRosa$date >= "2020-02-01" & SantaRosa$date < "2020-05-01",]
SantaRosa$CallType <- as.factor(SantaRosa$Nature_Description)
SantaRosa$suicide <- ifelse(grepl("SUICID", SantaRosa$CallType, ignore.case = TRUE), 1, 0)
SantaRosa$overdose <- NA
SantaRosa$hangup <- ifelse(grepl("911", SantaRosa$CallType, ignore.case = TRUE), 1, 0)
SantaRosa$state <- "CA"
SantaRosa$city <- "SantaRosa"
SantaRosa$dataType <- "calls911"
SantaRosa$set <- "SuicOnly"
SantaRosa <- select(SantaRosa, set, state, city, dataType, date, CallType, overdose, suicide, hangup)
SantaRosa <- SantaRosa %>%
  mutate_all(as.character)

# Santa Ana, CA
SantaAna <- read.csv("https://www.santa-ana.org/file-download/download/public/7688")
SantaAna$date <- as.character(SantaAna$IncidentDate)
SantaAna$date <- as.Date(SantaAna$date, format = "%m/%d/%Y")
SantaAna <- SantaAna[SantaAna$date >= "2020-02-01" & SantaAna$date < "2020-05-01",]
SantaAna$CallType <- as.factor(SantaAna$IncidentDescription)
SantaAna$suicide <- ifelse(grepl("SUICID", SantaAna$CallType, ignore.case = TRUE), 1, 0)
SantaAna$overdose <- NA
SantaAna$hangup <- ifelse(grepl("FALSE 911 CALL", SantaAna$CallType, ignore.case = TRUE), 1, 0)
SantaAna$state <- "CA"
SantaAna$city <- "SantaAna"
SantaAna$dataType <- "calls911"
SantaAna$set <- "SuicOnly"
SantaAna <- select(SantaAna, set, state, city, dataType, date, CallType, overdose, suicide, hangup)
SantaAna <- SantaAna %>%
  mutate_all(as.character)

#Dallas, TX
Dallas <- read.csv("https://www.dallasopendata.com/api/views/qv6i-rri7/rows.csv?accessType=DOWNLOAD")
Dallas <- filter(Dallas, Year.of.Incident==2020)
Dallas$date <- as.character(Dallas$Date.of.Report)
Dallas$date <- substr(Dallas$date, start = 1, stop = 10)
Dallas$date <- as.character.Date(Dallas$date, format = "%m/%d/%Y")
Dallas <- Dallas[Dallas$date >= "2020-02-01" & Dallas$date < "2020-05-01",]
Dallas$CallType <- paste(Dallas$Victim.Injury.Description, Dallas$Modus.Operandi..MO.)
Dallas$CallType <- as.factor(Dallas$CallType) 
Dallas$overdose <- ifelse(grepl("OVERDOSE", Dallas$CallType, ignore.case = TRUE), 1, 0)
Dallas$suicide <- ifelse(grepl("SUICID", Dallas$CallType, ignore.case = TRUE), 1, 0)
Dallas$hangup <- ifelse(grepl("04 - 911 HANG UP", Dallas$Call..911..Problem, ignore.case = TRUE), 1, 0)
Dallas$state <- "TX"
Dallas$city <- "Dallas"
Dallas$dataType <- "calls911"
Dallas$set <- "Both"
Dallas <- select(Dallas, set, state, city, dataType, date, CallType, overdose, suicide, hangup)
Dallas <- Dallas %>%
  mutate_all(as.character)

# Tucson, AZ 
Tucson <- read.csv("https://opendata.arcgis.com/datasets/c32b1adee46e497d88380a791284f8b9_53.csv?outSR=%7B%22latestWkid%22%3A2868%2C%22wkid%22%3A2868%7D")
Tucson$date <- as.character(Tucson$ACTDATE)
Tucson$date <- substr(Tucson$date, start = 1, stop = 10)
Tucson$date <- as.character.Date(Tucson$date, format = "%Y/%m/%d")
Tucson$date <- as.Date(Tucson$date)
Tucson <- Tucson[Tucson$date >= "2020-02-01" & Tucson$date < "2020-05-01",]
Tucson$CallType <- as.factor(Tucson$NatureCodeDesc)
Tucson$overdose <- ifelse(grepl("OVERDOSE", Tucson$CallType, ignore.case = TRUE), 1, 0)
Tucson$suicide <- ifelse(grepl("SUICID", Tucson$CallType, ignore.case = TRUE), 1, 0)
Tucson$hangup <- ifelse(grepl("911", Tucson$CallType, ignore.case = TRUE), 1, 0)
Tucson$hangup <- ifelse(grepl("MISDIAL", Tucson$CallType, ignore.case = TRUE), 1, paste(Tucson$hangup))
Tucson$hangup <- ifelse(grepl("OPEN LINE", Tucson$CallType, ignore.case = TRUE), 1, paste(Tucson$hangup))
Tucson$state <- "AZ"
Tucson$city <- "Tucson"
Tucson$dataType <- "calls911"
Tucson$set <- "Both"
Tucson <- select(Tucson, set, state, city, dataType, date, CallType, overdose, suicide, hangup)
Tucson <- Tucson %>%
  mutate_all(as.character)

# Richmond, CA
Richmond <- read.csv("https://www.transparentrichmond.org/api/views/k4y4-5quj/rows.csv?accessType=DOWNLOAD")
Richmond$date <- as.character(Richmond$createdDateUtc)
Richmond$date <- substr(Richmond$date, start = 1, stop = 10)
Richmond$date <- as.Date(Richmond$date, "%m/%d/%Y")
Richmond <- Richmond[Richmond$date >= "2020-02-01" & Richmond$date < "2020-05-01",]
Richmond$CallType <- as.factor(Richmond$secondaryEventType)
Richmond$overdose <- 0
Richmond$suicide <- ifelse(grepl("SUICID", Richmond$CallType, ignore.case = TRUE), 1, 0)
Richmond$hangup <- ifelse(grepl("911 DISCONNECT", Richmond$CallType, ignore.case = TRUE), 1, 0)
Richmond$state <- "CA"
Richmond$city <- "Richmond"
Richmond$dataType <- "calls911"
Richmond$set <- "SuicOnly"
Richmond <- select(Richmond, set, state, city, dataType, date, CallType, overdose, suicide, hangup)
Richmond <- Richmond %>%
  mutate_all(as.character)

# Naperville, IL
Naperville <- read.csv("https://data.naperville.il.us/api/views/7g4n-9xjz/rows.csv?accessType=DOWNLOAD")
Naperville$date <- as.character(Naperville$Date.Occured)
Naperville$date  <- as.Date(Naperville$date, "%m/%d/%Y")
Naperville <- Naperville[Naperville$date  >= "2020-02-01" & Naperville$date  < "2020-05-01",]
Naperville$CallType <- as.factor(Naperville$Offense)
Naperville$overdose <- ifelse(grepl("OVERDOSE", Naperville$CallType, ignore.case = TRUE), 1, 0)
Naperville$suicide <- ifelse(grepl("SUICID", Naperville$CallType, ignore.case = TRUE), 1, 0)
Naperville$hangup <- 0
Naperville$state <- "IL"
Naperville$city <- "Naperville"
Naperville$dataType <- "pdIncident"
Naperville$set <- "Both"
Naperville <- select(Naperville, set, state, city, dataType, date, CallType, overdose, suicide, hangup)
Naperville <- Naperville %>%
  mutate_all(as.character)

#Lincoln, NE
#NOTE: Confirm path for hard coded data before running
Lincoln <- read_csv("Desktop/Papers/COVID/Overdose_Suicide_paper/HardCoded/Lincoln.csv") #missing a lot of dates
Lincoln$date <- as.character(Lincoln$Rpt_Date)
Lincoln$date <- as.Date(Lincoln$date, "%Y%m%d")
Lincoln <- Lincoln[Lincoln$date  >= "2020-02-01" & Lincoln$date  < "2020-05-01",]
Lincoln$CallType <- as.factor(Lincoln$CALL_TYPE)
Lincoln$overdose <- ifelse(grepl("-OD", Lincoln$CallType, ignore.case = TRUE), 1, 0)
Lincoln$suicide <- ifelse(grepl("SUI", Lincoln$CallType, ignore.case = TRUE), 1, 0)
Lincoln$hangup <- NA
Lincoln$state <- "NE"
Lincoln$city <- "Lincoln"
Lincoln$dataType <- "pdIncident"
Lincoln$set <- "Both"
Lincoln <- select(Lincoln, set, state, city, dataType, date, CallType, overdose, suicide, hangup)
Lincoln <- Lincoln %>%
  mutate_all(as.character)

#Montgomery County, MD (Gaithesburg, Bethesda), split cities? Also for St. Paul?
Montgomery <- read_csv("https://data.montgomerycountymd.gov/api/views/icn6-v9z3/rows.csv?accessType=DOWNLOAD")
Montgomery$date <- Montgomery$'Dispatch Date / Time'
Montgomery$date <- as.Date(Montgomery$date, "%m/%d/%Y")
Montgomery <- Montgomery %>% filter(!is.na(date))
Montgomery <- Montgomery[Montgomery$date >= "2020-02-01" & Montgomery$date  < "2020-05-01",]
Montgomery$CallType <- as.factor(Montgomery$'Crime Name3')
Montgomery$overdose <- ifelse(grepl("OVERDOSE", Montgomery$CallType, ignore.case = TRUE), 1, 0)
Montgomery$suicide <- ifelse(grepl("SUICIDE", Montgomery$CallType, ignore.case = TRUE), 1, 0)
Montgomery$hangup <- NA
Montgomery$state <- "MD"
Montgomery$city <- "Montgomery"
Montgomery$dataType <- "pdIncident"
Montgomery$set <- "Both"
Montgomery <- select(Montgomery, set, state, city, dataType, date, CallType, overdose, suicide, hangup)
Montgomery <- Montgomery %>%
  mutate_all(as.character)

#Columbia, MO
#NOTE: Confirm path for hard coded data before running
Columbia <- read_csv("Desktop/Papers/COVID/Overdose_Suicide_paper/HardCoded/Columbia.csv")
Columbia$date <- Columbia$CallDateTime
Columbia$date <- as.Date(Columbia$date, "%m/%d/%y")
Columbia <- Columbia[Columbia$date >= "2020-02-01" & Columbia$date  < "2020-05-01",]
Columbia$CallType <- as.factor(Columbia$ExtNatureDisplayName)
Columbia$overdose <- ifelse(grepl("OVERDOSE", Columbia$CallType, ignore.case = TRUE), 1, 0)
Columbia$suicide <- NA
Columbia$hangup <- NA
Columbia$state <- "MO"
Columbia$city <- "Columbia"
Columbia$dataType <- "pdIncident"
Columbia$set <- "OverdoseOnly"
Columbia <- select(Columbia, set, state, city, dataType, date, CallType, overdose, suicide, hangup)
Columbia <- Columbia %>%
  mutate_all(as.character)

#Raleigh, NC
Raleigh <- read_csv("https://opendata.arcgis.com/datasets/24c0b37fa9bb4e16ba8bcaa7e806c615_0.csv")
Raleigh$date <- Raleigh$reported_date
Raleigh$date <- substr(Raleigh$date, start = 1, stop = 10)
Raleigh$date <- as.Date(Raleigh$date, "%Y/%m/%d")
Raleigh <- Raleigh[Raleigh$date >= "2020-02-01" & Raleigh$date  < "2020-05-01",]
Raleigh$CallType <- as.factor(Raleigh$crime_description)
Raleigh$overdose <- ifelse(grepl("OVERDOSE", Raleigh$CallType, ignore.case = TRUE), 1, 0)
Raleigh$suicide <- ifelse(grepl("SUIC", Raleigh$CallType, ignore.case = TRUE), 1, 0)
Raleigh$hangup <- NA
Raleigh$state <- "NC"
Raleigh$city <- "Raleigh"
Raleigh$dataType <- "pdIncident"
Raleigh$set <- "Both"
Raleigh <- select(Raleigh, set, state, city, dataType, date, CallType, overdose, suicide, hangup)
Raleigh <- Raleigh %>%
  mutate_all(as.character)

#Boston, MA
#NOTE: Confirm path for hard coded data before running
Boston <- read_csv("Desktop/Papers/COVID/Overdose_Suicide_paper/HardCoded/Boston.csv")
Boston$date <- Boston$'Received Date'
Boston$date <- as.Date(Boston$date, "%m/%d/%y")
Boston <- Boston[Boston$date >= "2020-02-01" & Boston$date  < "2020-05-01",]
Boston$CallType <- as.factor(Boston$`Call Type`) #JUMPER   (P) (E) (F)
Boston$overdose <- ifelse(grepl("OVERDOSE", Boston$CallType, ignore.case = TRUE), 1, 0)
Boston$suicide <- NA
Boston$hangup <- ifelse(grepl("HANGUP CALL", Boston$CallType, ignore.case = TRUE), 1, 0)
Boston$state <- "MA"
Boston$city <- "Boston"
Boston$dataType <- "calls911"
Boston$set <- "OverdoseOnly"
Boston <- select(Boston, set, state, city, dataType, date, CallType, overdose, suicide, hangup)
Boston <- Boston %>%
  mutate_all(as.character)

#Sonoma County, CA
Sonoma <- read_csv("https://data.sonomacounty.ca.gov/api/views/bpq8-s7gr/rows.csv?accessType=DOWNLOAD")
Sonoma$date <- Sonoma$'date time'
Sonoma$date <- as.Date(Sonoma$date, "%m/%d/%Y")
Sonoma <- Sonoma[Sonoma$date >= "2020-02-01" & Sonoma$date  < "2020-05-01",]
Sonoma$CallType <- as.factor(Sonoma$`nature description`) 
Sonoma$overdose <- ifelse(grepl("OVERDOSE", Sonoma$CallType, ignore.case = TRUE), 1, 0)
Sonoma$suicide <- ifelse(grepl("SUIC", Sonoma$CallType, ignore.case = TRUE), 1, 0)
Sonoma$hangup <- ifelse(grepl("HANGUP CALL", Sonoma$CallType, ignore.case = TRUE), 1, 0)
Sonoma$state <- "CA"
Sonoma$city <- "Sonoma"
Sonoma$dataType <- "calls911"
Sonoma$set <- "Both"
Sonoma <- select(Sonoma, set, state, city, dataType, date, CallType, overdose, suicide, hangup)
Sonoma <- Sonoma %>%
  mutate_all(as.character)

## Bind each of the dfs toghether
covidAll <- vec_rbind(Albany, Ashville)
covidAll <- vec_rbind(covidAll, Atlanta)
covidAll <- vec_rbind(covidAll, Baltimore) 
covidAll <- vec_rbind(covidAll, Cincinnati)
covidAll <- vec_rbind(covidAll, Detroit)
covidAll <- vec_rbind(covidAll, Evanston)
covidAll <- vec_rbind(covidAll, Fargo)
covidAll <- vec_rbind(covidAll, FortWayne)
covidAll <- vec_rbind(covidAll, Griffith)
covidAll <- vec_rbind(covidAll, Hartford)
covidAll <- vec_rbind(covidAll, KansasCity)
covidAll <- vec_rbind(covidAll, LosAngeles)
covidAll <- vec_rbind(covidAll, Louisville)
covidAll <- vec_rbind(covidAll, Mesa)
covidAll <- vec_rbind(covidAll, Milwaukee)
covidAll <- vec_rbind(covidAll, NewOrleans)
covidAll <- vec_rbind(covidAll, Omaha)
covidAll <- vec_rbind(covidAll, Phoenix)
covidAll <- vec_rbind(covidAll, SanFrancisco)
covidAll <- vec_rbind(covidAll, SantaMonica)
covidAll <- vec_rbind(covidAll, Seattle)
covidAll <- vec_rbind(covidAll, StPaul)
covidAll <- vec_rbind(covidAll, StPete)
covidAll <- vec_rbind(covidAll, SaltLake)
covidAll <- vec_rbind(covidAll, SantaRosa)
covidAll <- vec_rbind(covidAll, SantaAna)
covidAll <- vec_rbind(covidAll, Dallas)
covidAll <- vec_rbind(covidAll, Tucson)
covidAll <- vec_rbind(covidAll, Richmond)
covidAll <- vec_rbind(covidAll, Columbia)
covidAll <- vec_rbind(covidAll, Lincoln)
covidAll <- vec_rbind(covidAll, Montgomery)
covidAll <- vec_rbind(covidAll, Naperville)
covidAll <- vec_rbind(covidAll, Raleigh)
covidAll <- vec_rbind(covidAll, AnnArbor)
covidAll <- vec_rbind(covidAll, Boston)
covidAll <- vec_rbind(covidAll, Sonoma)

# Put all variables into character format
covidAll <- covidAll %>%
  mutate_all(as.character)

# Remove individual place datasets from memory
rm(Albany, Ashville, Atlanta, Baltimore, Cincinnati, Detroit, Evanston, FortWayne, Fargo, Griffith, Hartford,
   KansasCity, LosAngeles, Louisville, Mesa, MesaFire, MesaOD, Milwaukee, NewOrleans, Omaha, Phoenix,
   postSAH_ALL, preSAH_ALL, SaltLake, SanFrancisco, SantaAna, SantaMonica, SantaRosa, Seattle, StPaul, StPete, 
   covidBoth, covidOverdose, covidSuic, Dallas, Tucson, Richmond, Columbia, Lincoln, Montgomery, Naperville,
   Raleigh, AnnArbor, Boston)

# Save CSV version of combined dataset
#write.csv(covidAll, "covidAll_clean.csv")
