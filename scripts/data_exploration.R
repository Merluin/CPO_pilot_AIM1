
###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS
#  Date:        03/2022
#  Description: Generate the dataset from psychopy
#
#  Update:      05/04/2022
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

# columns selection 
dataset<-dataset%>%
  select(subject, id,wheelresp.x,wheelresp.y,compound.x,compound.y,compound.stopped,file_emotion,file_emotion_level,
         wheelresp_emotion,compound_emotion,wheelresp_level,compound_level)%>%
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
         wheel ="wheel1",
         file_emotion = case_when(file_emotion == "angry" ~ "rabbia",
                                  file_emotion == "disgusted" ~ "disgusto",   
                                  file_emotion == "fear" ~ "paura",
                                  file_emotion == "happy" ~ "gioia",
                                  file_emotion == "neutral" ~  "neutral",    
                                  file_emotion == "sad" ~ "tristezza",
                                  file_emotion == "surprised" ~ "sorpresa"),
         resp_emotion = ifelse(resp_emotion=="orgolio","orgoglio",resp_emotion),
         correct = ifelse(file_emotion == resp_emotion,1,0),
         count = 1)%>%
  'colnames<-'(c("subject" , "id", "x","y","file_emotion","file_level",
                 "resp_emotion","resp_level","wheel","correct","count"))

# dataset wheel2
wheel2<-dataset%>%
  select(subject, id,compound.x,compound.y,file_emotion,file_emotion_level,
         compound_cake,compound_level,compound.stopped)%>%
  filter(compound.stopped == "None")%>%
  mutate(compound.x = str_split(compound.x,","),
         compound.x = as.numeric(sapply(compound.x, function(x) ifelse(length(x) > 1, dplyr::last(x), x))),
         compound.y = str_split(compound.y,","),
         compound.y = as.numeric(sapply(compound.y, function(x) ifelse(length(x) > 1, dplyr::last(x), x))),
         compound.x = compound.x - 270,
         wheel ="wheel2",
         file_emotion = case_when(file_emotion == "angry" ~ "rabbia",
                                  file_emotion == "disgusted" ~ "disgusto",   
                                  file_emotion == "fear" ~ "paura",
                                  file_emotion == "happy" ~ "gioia",
                                  file_emotion == "neutral" ~  "neutral",    
                                  file_emotion == "sad" ~ "tristezza",
                                  file_emotion == "surprised" ~ "sorpresa"),
         compound_cake = ifelse(compound_cake=="orgolio","orgoglio",compound_cake),
         correct = ifelse(file_emotion == compound_cake,1,0),
         count = 1,
         compound_cake= factor(compound_cake,levels = c("neutral",
                                        "orgoglio","euforia","gioia","soddisfazione",
                                        "sollievo","speranza","interesse","sorpresa",
                                        "tristezza","paura","vergogna","colpa",
                                        "invidia","disgusto","disprezzo","rabbia")))%>%
  select(subject, id,compound.x,compound.y,file_emotion,file_emotion_level,
         compound_cake,compound_level,wheel,correct,count)%>%
  'colnames<-'(c("subject" , "id", "x","y","file_emotion","file_level",
                 "resp_emotion","resp_level","wheel","correct","count"))


plot<-rbind(wheel1,wheel2)

# Plot
plot%>%
  ggplot()+
  geom_point(aes(x=x,y= y, color = file_level, shape=as.factor(resp_level)))+
  facet_wrap(wheel ~ file_emotion)

#barplot
plot%>%
  filter(wheel == "wheel1")%>%
  group_by(subject,file_emotion,file_level,wheel)%>%
  summarise_at(vars(correct),list(mean))%>%
  group_by(file_emotion,file_level,wheel)%>%
  summarise_at(vars(correct),list(mean))%>%
  ggplot()+
  geom_bar(aes(y=correct,x=file_emotion, fill= file_level), stat="identity",position = position_dodge2(width = 0.1, preserve = "single", padding = -0.1), alpha=0.7)

#barplot X levels
plot%>%
  filter(wheel == "wheel1")%>%
  group_by(subject,file_emotion,file_level,resp_level)%>%
  summarise_at(vars(correct),list(mean))%>%
  group_by(file_emotion,file_level,resp_level)%>%
  summarise_at(vars(correct),list(mean))%>%
  ggplot()+
  geom_bar(aes(y=correct,x=file_emotion, fill= resp_level), stat="identity",position = position_dodge2(width = 0.1, preserve = "single", padding = -0.1), alpha=0.7)+
  facet_grid("file_level")

#barplot wheel2 subtle
plot%>%
  filter(wheel == "wheel2")%>%
  group_by(subject,file_emotion,resp_emotion,file_level)%>%
  summarise_at(vars(count),list(sum))%>%
  group_by(file_emotion,resp_emotion,file_level)%>%
  summarise_at(vars(count),list(mean))%>%
  mutate(resp_emotion= factor(resp_emotion,levels = c("neutral",
                                         "orgoglio","euforia","gioia","soddisfazione",
                                         "sollievo","speranza","interesse","sorpresa",
                                         "tristezza","paura","vergogna","colpa",
                                         "invidia","disgusto","disprezzo","rabbia")),
         file_emotion= factor(file_emotion,levels = c("neutral",
                                                      "gioia",
                                                      "sorpresa",
                                                      "tristezza","paura",
                                                      "disgusto","rabbia")),
         file_quarter = case_when(file_emotion == "neutral"~ 0,
                             file_emotion == "gioia"~ 1,
                             file_emotion == "sorpresa"~ 2,
                             file_emotion == "tristezza"~ 3,
                             file_emotion == "paura"~ 3,
                             file_emotion == "disgusto"~ 4,
                             file_emotion == "rabbia"~ 4),
         
         emo_quarter = case_when(resp_emotion == "neutral"~ 0,
                                 resp_emotion == "orgoglio"~ 1,
                                 resp_emotion == "euforia"~ 1,
                                 resp_emotion == "gioia"~ 1,
                                 resp_emotion == "soddisfazione"~ 1,
                                 resp_emotion == "sollievo"~ 2,
                                 resp_emotion == "speranza"~ 2,
                                 resp_emotion == "interesse"~ 2,
                                 resp_emotion == "sorpresa"~ 2,
                                 resp_emotion == "tristezza"~ 3,
                                 resp_emotion == "paura"~ 3,
                                 resp_emotion == "vergogna"~ 3,
                                 resp_emotion == "colpa"~ 3,
                                 resp_emotion == "invidia"~ 4,
                                 resp_emotion == "disgusto"~ 4,
                                 resp_emotion == "disprezzo"~ 4,
                                 resp_emotion == "rabbia"~ 4))%>%
  ggplot()+
  geom_bar(aes(y=count,x=resp_emotion, fill= file_level), stat="identity",position = position_dodge2(width = 0.1, preserve = "single", padding = -0.1), alpha=0.7)+
  #scale_x_continuous(labels = "resp_emotion")+
  facet_grid("file_emotion" )
  


#################################################
# 
# END
#
#################################################

