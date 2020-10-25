
#Project specific packages--------------------------------------------------------------
#library("stringr")
library("dplyr")
library("tidyr")
library(e1071)

#Load functions--------------------------------
source(file = "utils/utils.R")

#Setting up meta options--------------
Sys.setenv(TZ='UTC')
options(java.parameters = "-Xmx4g") #setups up memory to 4gs
options(scipen=999)

