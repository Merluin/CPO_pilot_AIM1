
###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS
#  Date:        03/2022
#  Description: Accuracy for OFMT
#
#  Update:      18/05/2022
###########################################################################

rm(list=ls()) # remove all objects

# Packages ----------------------------------------------------------------

library(tidyverse)
library(anytime)
library(readr)
library(dplyr)
library(stringr)


# Functions ---------------------------------------------------------------
source("05.functions/OFMT_concatenation.R")


# loading data ----
datasetname<-"dataset_OFMT"
OFMT_concatenation(datasetname)
load(paste0("04.data/",datasetname,".RData") )


