#renv::activate()
#renv::init()
#Project specific packages--------------------------------------------------------------
#library("stringr")
library("ggplot2")
library("dplyr")
library("tidyr")
library(e1071)

#Load functions--------------------------------
source(file = "utils.R")

#Setting up meta options--------------
Sys.setenv(TZ='UTC')
options(java.parameters = "-Xmx4g") #setups up memory to 4gs
options(scipen=999)