# 📦 PACKAGES ----
library(tidyverse) # a range of helpful packages
library(janitor)# helps to format the data



#💾 IMPORTING DATA ----

setwd("~/GitHub/great_tits/data")# setting the working directory

data_2020<-read.csv(file = "NestRecordsMadingley2020.csv", header = TRUE)
glimpse(data_2020)

#🧽 CLEANING DATA ---- 

nest_2020<-data_2020%>%
  select(site , boxNumber, site.boxNumber, Species, Antenna.PIT.tag.ID.1, Antenna.Pit.tag.ID.2,BTO.ring.derived.from.Antenna.ID.1, BTO.ring.derived.from.Antenna.ID.2, Female.Ring.number.from.trapping, Male.ring.number.from.trapping, numberFledged)# making a new df
#there is no data for male and female will find later 
head(nest_2020)#checking the first 6 lines 

nest_2020<- janitor::clean_names(nest_2020)# cleaning column names, snake_case
#nest_2019<- nest_2019 %>% mutate_all(na_if,"")#making all of the blanks into NA's

nest_2020<-cbind(nest_2020, year='2020')#add a new column named "year" with information about what year it comes from 
names(nest_2020)#checking the headings 
head(nest_2020)#checking the first 6 lines 

nest_2020_grt<-nest_2020%>%
  filter(species=="GRETI") #filter out blue_tit 

nest_2020_grt%>% 
  is.na() %>% 
  sum()#25 NA's - removed ??   

write.csv(nest_2020_grt,file="nest_id_from_2020.csv")

glimpse(nest_2020_grt)


PITIDM_2020<-unique(nest_2020_grt$pit_idm)# not sure what this does 
PITIDM_2020<-as.data.frame(PITIDM_2020)# not sure what this does 
# see in the nest box, how many different tags there are 
# need to know the ref tag 
