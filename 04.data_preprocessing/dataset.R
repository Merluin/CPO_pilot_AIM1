#################################################
# 
# Name:           Psychopy dataset
# Programmer:     Thomas Quettier
# Date:           18/11/2021
# Description:    Put together several csv files
#
#################################################


# Library & dataset
rm(list=ls()) # remove all objects
# neaded packages
library(dplyr)
library(tidyverse)

# find nb participant
folder_dir<-c("03.original_data/")
foldername<-list.files(folder_dir,pattern='csv')
nbsub<-sapply(folder_dir,function(folder_dir){length(foldername)})
effective<-c(1:nbsub)

# loading data ----
dataset <- read.csv(paste(folder_dir,foldername[1],sep = ""), sep=",", header=TRUE,stringsAsFactors = FALSE)

for (i in 2:length(effective)){
  data2<-read.csv(paste(folder_dir,foldername[i],sep = ""), sep=",", header=TRUE,stringsAsFactors = FALSE)
  dataset<-rbind(dataset,data2)
}

save(dataset,file="04.data_preprocessing/dataset.RData")

#################################################
# 
# END
#
#################################################