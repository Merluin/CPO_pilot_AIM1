#################################################
# 
# Name:           Psychopy dataset
# Programmer:     Thomas Quettier
# Date:           18/11/2021
# Description:    Put together several csv files
#
#################################################
OFMT_concatenation <- function(dataset_name)
{
  
  # packages
  library(dplyr)
  library(tidyverse)
  
  # find nb of file
  folder_dir<-c("03.original_data/OFMT/")
  
  #concatenate all file
  dataset<-list.files(path=folder_dir, full.names = TRUE,pattern='csv') %>%
    lapply(.,function(x) read.csv(x, sep=";", header=TRUE, stringsAsFactors = FALSE ))%>%
    bind_rows()
  
  dataset<- dataset%>%
    mutate(subject = as.numeric(as.factor(Participant.Private.ID)))
  
  
  #write.csv2(dataset, file = "03.original_data/Pt_.csv")
  save(dataset,file=paste0("04.data/",dataset_name,".RData"))
  
  
} #end function  

#################################################
# 
# END
#
#################################################