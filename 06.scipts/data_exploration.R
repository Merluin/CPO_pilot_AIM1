
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
library(stringr)


# Functions ---------------------------------------------------------------
source("05.functions/dataset_concatenation.R")


# loading data ----
datasetname<-"dataset"
dataset_concatenation(datasetname)
load(paste0("04.data/",datasetname,".RData") )

# keep trials with responce to wheel 1
dataset<-dataset%>%
  filter(loop_exp.thisN >= 0,
         wheelresp.stopped == "None") 

# clean dataset
dataset$wheelresp.x<-str_remove_all(dataset$wheelresp.x, "[\\[|\\] ']")
dataset$wheelresp.y<-str_remove_all(dataset$wheelresp.y, "[\\[|\\] ']")
dataset$compound.x<-str_remove_all(dataset$compound.x, "[\\[|\\] ']")
dataset$compound.y<-str_remove_all(dataset$compound.y, "[\\[|\\] ']")


name<-colnames(dataset)

dataset<-dataset%>%
  select(subject, id,wheelresp.x,wheelresp.y,compound.x,compound.y,compound.stopped,file_emotion,file_emotion_level,
         wheelresp_cake,compound_cake,wheelresp_level,compound_level)%>%
  'colnames<-'(c("subject" , "id", "wheelresp.x","wheelresp.y","compound.x","compound.y","compound.stopped","file_emotion","file_emotion_level",
                 "resp_emotion","compound_cake","resp_level","compound_level"))


# dataset wheel1
wheel1<-dataset%>%
  select(subject, id,wheelresp.x,wheelresp.y,file_emotion,file_emotion_level,
         resp_emotion,resp_level)%>%
  mutate(wheelresp.x = str_split(wheelresp.x,","),
         wheelresp.x = as.numeric(sapply(wheelresp.x, function(x) ifelse(length(x) > 1, dplyr::last(x), x))),
         wheelresp.y = str_split(wheelresp.y,","),
         wheelresp.y = as.numeric(sapply(wheelresp.y, function(x) ifelse(length(x) > 1, dplyr::last(x), x))),
         wheelresp.x = wheelresp.x + 270,
         wheel ="wheel1")%>%
  'colnames<-'(c("subject" , "id", "x","y","file_emotion","file_level",
                 "resp_emotion","resp_level","wheel"))

# dataset wheel1
wheel2<-dataset%>%
  select(subject, id,compound.x,compound.y,file_emotion,file_emotion_level,
         compound_cake,compound_level,compound.stopped)%>%
  filter(compound.stopped == "None")%>%
  mutate(compound.x = str_split(compound.x,","),
         compound.x = as.numeric(sapply(compound.x, function(x) ifelse(length(x) > 1, dplyr::last(x), x))),
         compound.y = str_split(compound.y,","),
         compound.y = as.numeric(sapply(compound.y, function(x) ifelse(length(x) > 1, dplyr::last(x), x))),
         compound.x = compound.x - 270,
         wheel ="wheel2")%>%
  select(subject, id,compound.x,compound.y,file_emotion,file_emotion_level,
         compound_cake,compound_level,wheel)%>%
  'colnames<-'(c("subject" , "id", "x","y","file_emotion","file_level",
                 "resp_emotion","resp_level","wheel"))


plot<-rbind(wheel1,wheel2)



plot%>%
  ggplot()+
  geom_point(aes(x=x,y= y, color = file_level, shape=as.factor(resp_level)))+
  facet_wrap(wheel ~ file_emotion)






#################################################
# 
# END
#
#################################################