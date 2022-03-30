
###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS
#  Date:        03/2022
#  Description: Generate the dataset from psychopy
#
#   
###########################################################################

rm(list=ls()) # remove all objects

# Packages ----------------------------------------------------------------

library(tidyverse)
library(anytime)
library(readr)
library(dplyr)


# Functions ---------------------------------------------------------------
source("05.functions/dataset_concatenation.R")


# loading data ----
datasetname<-"dataset"
dataset_concatenation(datasetname)
load(paste0("04.data/",datasetname,".RData") )


table(dataset$subject)








#################################################
# 
# END
#
#################################################