# 📦 PACKAGES ----
library(tidyverse) # a range of helpful packages
library(janitor)# helps to format the data



#💾 IMPORTING DATA ----

setwd("~/GitHub/great_tits/data")# setting the working directory

data_2021<-read.csv(file = "NestRecordsMadingley2021.csv", header = TRUE)
glimpse(data_2021)

#🧽 CLEANING DATA ---- 

nest_2021<-data_2021%>%
  select(site , boxNumber, site.boxNumber, Species, Antenna.PIT.tag.ID.1, Antenna.Pit.tag.ID.2, numberFledged)# making a new df
#there is no data for male and female will find later 
head(nest_2021)#checking the first 6 lines 

nest_2021<- janitor::clean_names(nest_2021)# cleaning column names, snake_case
#nest_2019<- nest_2019 %>% mutate_all(na_if,"")#making all of the blanks into NA's

nest_2021<-cbind(nest_2021, year='2021') #add a new column named "year" with information about what year it comes from 
names(nest_2021) #checking the headings 
head(nest_2021)#checking the first 6 lines 

nest_2021_grt<-nest_2021%>%
  filter(species=="GRETI") #filter out blue_tit 

nest_2020_grt%>% 
  is.na() %>% 
  sum()#25 NA's removed ??   

PITIDM<-unique(nest_2021_grt$pitIDM)# not sure what this does 
PITIDM<-as.data.frame(PITIDM)# not sure what this does 
