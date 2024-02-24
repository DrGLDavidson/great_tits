# 📦 PACKAGES ----
library(tidyverse) # a range of helpful packages
library(janitor)# helps to format the data



#💾 IMPORTING DATA ----

setwd("~/GitHub/great_tits/data")# setting the working directory

data<-read.csv(file = "NestRecordsMadingley2019.csv", header = TRUE)
names(data)

#🧽 CLEANING DATA ---- 

nest_2019<-data%>%
  select(site , boxNumber, site.boxNumber, Species, maleID, femaleID, pitIDM, pitIDF, numberFledged)# making a new df
head(nest_2019)#checking the first 6 lines 

nest_2019<- janitor::clean_names(nest_2019)# cleaning column names, snake_case
#nest_2019<- nest_2019 %>% mutate_all(na_if,"")#making all of the blanks into NA's


nest_2019<-cbind(nest_2019, year='2019')#add a new column named "year" with information about what year it comes from 
names(nest_2019)#checking the headings 
head(nest_2019)#checking the first 6 lines 

nest_2019_grt<-nest_2019%>%
  filter(species=="GRETI") #filter out blue_tit 
  
nest_2019_grt%>% 
  is.na() %>% 
  sum()#2 NAs    

PITIDM<-unique(nest_2019_grt$pitIDM)# not sure what this does 
PITIDM<-as.data.frame(PITIDM)# not sure what this does 
# see in the nest box, how many different tags there are 
# need to know the ref tag 




