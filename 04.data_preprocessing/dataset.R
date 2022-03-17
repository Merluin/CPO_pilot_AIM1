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
folder_dir<-c("data")
nbsub<-sapply(folder_dir,function(dir){length(list.files(dir,pattern='csv'))})
effective<-c(1:nbsub)

# loading data ----
dataset <- read.csv(paste(folder_dir,"/Pt_1.csv",sep = ""), sep=",", header=TRUE,stringsAsFactors = FALSE)

for (i in 2:length(effective)){
  data2<-read.csv(paste(folder_dir,"/Pt_",effective[i],".csv",sep = ""), sep=",", header=TRUE,stringsAsFactors = FALSE)
  dataset<-rbind(dataset,data2)
}

save(dataset,file="data/dataset.RData")

#################################################
# 
# END
#
#################################################