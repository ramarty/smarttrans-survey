# smarTTrans Master

# Filepaths --------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/CrashMap-Nairobi"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/CrashMap-Nairobi"

if(Sys.info()[["user"]] == "meyhar") project_file_path <- "~/Dropbox/DIME_Meyhar/Kenya/roadcrash-survey"

rawdata_file_path <- file.path(project_file_path, "Data", "RawData")
finaldata_file_path <- file.path(project_file_path, "Data", "FinalData")

# Packages ---------------------------------------------------------------------
library(leaflet)
library(dplyr)
library(rgdal)
library(rgeos)
library(data.table)
library(lubridate)
library(ggplot2)
library(raster)
library(sp)
library(spdep)
library(broom)
library(raster)
library(parallel)
library(pbmcapply)
library(stringr)
library(dplyr)
library(readxl)
library(haven)
library(tidyr)
library(doBy)
library(grid)
library(gridExtra)
library(gtable)
library(googlesheets)
