#--------------------------------library----------------------------------------
library(readxl)
library(vtable)
library(caret)
library(janitor)
library(tidyverse)
library(scales)
library(lubridate)
library(tidymodels)
library(themis)
library(baguette)
library(pROC)
library(openxlsx)
#--------------------------------read data--------------------------------------
hazmat <- read_excel("Iran_Hazmat_Microdata.xlsx")
#-------------------------------Save & load data--------------------------------
save(hazmat,
     file = "RawIranHazmat.RData")

load("RawIranHazmat.RData")

class(hazmat)
names(hazmat)
