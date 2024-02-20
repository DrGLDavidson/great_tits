# 📦 PACKAGES ----
library(tidyverse) # a range of helpful packages
library(janitor)# helps to format the data



#💾 IMPORTING DATA ----

setwd("~/GitHub/great_tits/data/2019 Visit counters/030419_MW102_eyes")# setting the working directory

eyes_102_2019<-read.delim(file= "C1272RT.TXT", sep="\t", header=TRUE)#loading in data
names(eyes_102_2019)


#🧽 CLEANING DATA ---- 

eyes_102_2019_f<-eyes_102_2019%>%
  select(Date, Hmsec, CounterID, TagID_1)
names(eyes_102_2019_f) # flituring the data


names(eyes_102_2019_f)[names(eyes_102_2019_f) == "TagID_1"] <- "RFID" # renaming RFID 


uniqueIDs<-unique(eyes_102_2019_f$RFID)
uniqueIDs #???

eyes_102_2019_f<-eyes_102_2019_f[!grepl('TagID_1', eyes_102_2019_f$RFID),]# removing tagid one 


 
eyes_102_2019_f<-eyes_102_2019_f[!eyes_102_2019_f$RFID=="",] #remove blank rows

# 🌎 Data exploration ----

uniqueID<- unique(eyes_102_2019_f$RFID)
uniqueID #only 3 different individuals
#"0300024FEF" "011016C265" "01101737FA"

#"0300024FEF" -> is the start and stop indicator 

referenceTag<-eyes_102_2019_f[eyes_102_2019_f$RFID == '0300030EFF',]#flituring out 0300030EFF
referenceTag # NOT SURE WHATS THIS DID?


eyes_102_2019_f<-eyes_102_2019_f%>%
  mutate(Date=ymd_hms(Date))
class(eyes_102_2019_f$Date) # chaneing the data format to POSIXct class

#⏰ ----

eyes_102_2019_t<-subset(eyes_102_2019_f,Date <= as.POSIXct('2019-05-03 07:56:01', tz="UTC")) # do you do it from the indictor one or the first bird??

eyes_102_2019_t<-subset(eyes_102_2019_f,Date >= as.POSIXct('2019-05-03 08:45:11', tz="UTC")) 





#The reference tag is 0300024FEF. The experimental time should be restricted to 45 minutes from the last time the reference tag was read. Following the GIT hub tutorial, you should be able to do this with the new data. 




