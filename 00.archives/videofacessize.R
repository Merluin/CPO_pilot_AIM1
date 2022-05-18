###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS
#  Date:        03/2022
#  Description: video faces size 
#
#  Update:      13/05/2022
###########################################################################

rm(list=ls()) # remove all objects

# Packages ----------------------------------------------------------------

library(tidyverse)
library(anytime)
library(readr)
library(dplyr)
library(stringr)

# loading data ----
dataset <- read.csv("03.original_data/Benedettamisure.csv", sep=";", header=TRUE,stringsAsFactors = FALSE)
dataset<-dataset%>%
  mutate(Benedetta = sub('\\ cm', '', dataset$Benedetta),
         Benedetta = sub('\\,', '.', Benedetta),
         Benedetta = as.numeric(Benedetta))

dataset[3,3]<-"subtle"



subtle<-dataset%>%filter(file_emotion_level=="subtle")
full<-dataset%>%filter(file_emotion_level=="full")

summary(subtle$Benedetta)
summary(full$Benedetta)
t.test(full$Benedetta,subtle$Benedetta)
  
