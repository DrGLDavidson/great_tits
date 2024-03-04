# 📦 PACKAGES ----
library(tidyverse) # a range of helpful packages
library(janitor)# helps to format the data
library(lubridate)# good dates 



#💾 IMPORTING DATA ----

setwd("~/GitHub/great_tits/data/2019 Visit counters/050519_MW8_eyes_MW140_eyes")# setting the working directory

eyes_8_140_2019<-read.delim(file= "C1252RT.TXT", sep="\t", header=TRUE)#loading in data
names(eyes_8_140_2019)


#🧽 CLEANING DATA ---- 

eyes_8_140_2019_f<-eyes_8_140_2019%>%
  select(Date, Hmsec, CounterID, TagID_1)
names(eyes_8_140_2019_f) # flituring the data


names(eyes_8_140_2019_f)[names(eyes_8_140_2019_f) == "TagID_1"] <- "RFID" # renaming RFID 


uniqueIDs<-unique(eyes_8_140_2019_f$RFID)
uniqueIDs #lets you see the different ids in the df
#""           "TagID_1 "   "0300030EFF" "011016FE22" "011016ED44" "011017319D"


eyes_8_140_2019_f<-eyes_8_140_2019_f[!grepl('TagID_1', eyes_8_140_2019_f$RFID),]# removing tagid one 

eyes_8_140_2019_f<-eyes_8_140_2019_f[!eyes_8_140_2019_f$RFID=="",]#remove blank rows
head(eyes_8_140_2019_f)

# 🌎 Data exploration ----

uniqueID<- unique(eyes_8_140_2019_f$RFID)
uniqueID #only 3 different individuals
#"0300030EFF" "011016FE22" "011016ED44" "011017319D"

#"0300024FEF", "0300030EFF" -> is the start and stop indicator 

referencetag<-eyes_8_140_2019_f[eyes_8_140_2019_f$RFID == '0300030EFF',]#flituring out 0300030EFF
referencetag # can see when the indicator was used


eyes_8_140_2019_f<-eyes_8_140_2019_f%>%
  mutate(Date=ymd_hms(Date))
class(eyes_8_140_2019_f$Date) # changing the data format to POSIXct class

#👀🐤 Latency ----

#extracting the first occurence of each RFID tag
latency<-eyes_8_140_2019_f %>% 
  arrange(Date) %>% 
  group_by(RFID) %>% 
  slice(1) %>% # makes the data set and arrages it
  ungroup()

#df with all of the first occurances of tags
referencetag<-eyes_8_140_2019_f[eyes_8_140_2019_f$RFID == '0300030EFF',]
referencetag# checking 


eyes_8_140_2019_f.2<-slice(referencetag, 2)# making a 2nd row
eyes_8_140_2019_f.2


# removing 0300024FEF from the df latency + bind eye_102_2019_f.2 to latency
latency<-latency[-c(1),]
latency # not sure if this works 

latency2<-bind_rows(eyes_8_140_2019_f.2,latency)
names(latency)
# date is the first column 

#create column latency (sub) 

latency3<-latency2 %>% 
  mutate(latency = Date - Date [row_number()==1])# - to find lat

latency4<-eyes_8_140_2019_f%>%
  arrange(Date)%>%
  mutate(latency = Date - Date[row_number()==2])%>%
  group_by(RFID)%>%
  slice(1)%>%
  ungroup()


# binding the df together ----

eyes_8_140_2019_b<-eyes_8_140_2019_f
 

eyes_8_140_2019_b<-cbind(eyes_8_140_2019_b, nestbox='mw10')
eyes_8_140_2019_b<-cbind(eyes_8_140_2019_b, year='2019')
head(eyes_8_140_2019_b)


# multiples 👯‍♀️----


#column names 
eyes_8_2019<- eyes_8_140_2019_b[c(1,3, 4, 6),]
head(eyes_8_2019)

#delete multiple rows
eyes_8_2019_1<- eyes_8_140_2019_b[-c(43:1),]
eyes_8_2019_1


#column names 
eyes_140_2019<- eyes_8_140_2019_b[c(1, 3, 4, 6),]
head(eyes_140_2019)

#delete multiple rows
eyes_140_2019_1<- eyes_8_140_2019_b[-c(44,70)] 
head(eyes_140_2019_1)


#⏰  45mins----


eyes_8_140_2019_t<-subset(eyes_8_140_2019_f,Date >= as.POSIXct('2019-05-03 08:04:16', tz="UTC")) # do from last indicator

eyes_8_140_2019_t<-subset(eyes_8_140_2019_f,Date <= as.POSIXct('2019-05-03 08:49:16', tz="UTC")) # make sure 45 mins

write.csv(totalVisits45min,file="totalVisits45m_mw10_2019.csv")
