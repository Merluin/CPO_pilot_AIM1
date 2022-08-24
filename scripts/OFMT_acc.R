
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
library(ggplot2)


# Functions ---------------------------------------------------------------
source("05.functions/OFMT_concatenation.R")


# loading data ----
datasetname<-"dataset_OFMT"
OFMT_concatenation(datasetname)
load(paste0("04.data/",datasetname,".RData") )


# keep trials with responce to wheel 1
data<-dataset%>%
  filter(Zone.Type== "response_button_text", inclusionCheck == "test")%>%
  select(Participant.Private.ID, Reaction.Time, Correct,ANSWER,Response,face1, face2)%>%
  mutate(Correct = as.numeric(as.character(Correct)))%>%
  'colnames<-'(c("subject" ,"rt.correct","correct","ref","resp","face1","face2"))

slider<-dataset%>%
  filter(Zone.Type== "response_slider_endValue", inclusionCheck == "test")%>%
  select(Participant.Private.ID, Reaction.Time, Response)%>%
  mutate(Response = as.numeric(Response))%>%
  'colnames<-'(c("subject" ,"rt.slider","resp.slider"))

data<-cbind(data,slider$rt.slider,slider$resp.slider)%>%
  'colnames<-'(c("subject" ,"rt.correct","correct","ref","resp","face1","face2","rt.slider","resp.slider"))

               

data%>%
  group_by(subject,ref)%>%
  summarise_at("correct",sum)%>%
  group_by(ref)%>%
  summarise_at("correct",list(mean,sd))%>%
  data.frame()%>%
  ggplot()+
  geom_bar(aes(y=fn1,x=ref, fill= ref),stat='identity')+
  geom_errorbar( aes(x=ref, ymin=fn1-fn2, ymax=fn1+fn2), width=0.2, size=.5)

data%>%
  group_by(subject,ref)%>%
  summarise_at("rt.correct",mean)%>%
  group_by(ref)%>%
  summarise_at("rt.correct",list(mean,sd))%>%
  data.frame()%>%
  ggplot()+
  geom_bar(aes(y=fn1,x=ref, fill= ref),stat='identity')+
  geom_errorbar( aes(x=ref, ymin=fn1-fn2, ymax=fn1+fn2), width=0.2, size=.5)

data%>%
  group_by(subject,ref)%>%
  summarise_at("resp.slider",mean)%>%
  group_by(ref)%>%
  summarise_at("resp.slider",list(mean,sd))%>%
  data.frame()%>%
  ggplot()+
  geom_bar(aes(y=fn1,x=ref, fill= ref),stat='identity')+
  geom_errorbar( aes(x=ref, ymin=fn1-fn2, ymax=fn1+fn2), width=0.2, size=.5)

data%>%
  group_by(subject,ref)%>%
  summarise_at("rt.correct",mean)%>%
  group_by(ref)%>%
  summarise_at("rt.correct",list(mean,sd))%>%
  data.frame()%>%
  ggplot()+
  geom_bar(aes(y=fn1,x=ref, fill= ref),stat='identity')+
  geom_errorbar( aes(x=ref, ymin=fn1-fn2, ymax=fn1+fn2), width=0.2, size=.5)
  




