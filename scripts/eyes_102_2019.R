# 📦 PACKAGES ----
library(tidyverse) # a range of helpful packages
library(janitor)# helps to format the data
library(lubridate)

# new ----
# 📦 PACKAGES ----
library(tidyverse) # a range of helpful packages
library(janitor)# helps to format the data
library(lubridate)# good dates 

#new ----

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
uniqueIDs #lets you see the different ids in the df
#""           "TagID_1 "   "0300024FEF" "011016C265" "01101737FA"


eyes_102_2019_f<-eyes_102_2019_f[!grepl('TagID_1', eyes_102_2019_f$RFID),]# removing tagid one 

eyes_102_2019_f<-eyes_102_2019_f[!eyes_102_2019_f$RFID=="",]#remove blank rows
head(eyes_102_2019_f)

# 🌎 DATA EXPLORATION ----

uniqueID<- unique(eyes_102_2019_f$RFID)
uniqueID #only 3 different individuals
#"0300024FEF" "011016C265" "01101737FA"

#"0300024FEF", "0300030EFF" -> is the start and stop indicator 

referencetag<-eyes_102_2019_f[eyes_102_2019_f$RFID == '0300024FEF',]#flituring out 0300030EFF
referencetag # can see when the indicator was used


eyes_102_2019_f<-eyes_102_2019_f%>%
  mutate(Date=ymd_hms(Date))
class(eyes_102_2019_f$Date) # changing the data format to POSIXct class

#with year/mw ----

#⏰ 45MINS ----

#first need to filter according to experiment time and last RFID tag before experiment start. Can do this my row number but also time may be better. Look at referencetag dataframe for time values. 


eyes_102_2019_t<-subset(eyes_102_2019_f,Date >= as.POSIXct('2019-05-03 07:57:33 ', tz="UTC")) # do from last indicator

#this time is 45 minutes from the above time manually write this
eyes_102_2019_t2<-subset(eyes_102_2019_t,Date <= as.POSIXct('2019-05-03 08:45:11 ', tz="UTC")) # make sure 45 mins
###FOR EMMA TO ADD THE CODE TO ADD THE NESTBOX AND YEAR TO data frame t2


# 📅 DATE ----
# adding the date and the nest box name to the df

eyes_102_2019_b.3<-eyes_102_2019_t2

eyes_102_2019_b.3<-cbind(eyes_102_2019_b.3, nestbox='mw102') 
eyes_102_2019_b.3<-cbind(eyes_102_2019_b.3, year='2019')
head(eyes_102_2019_b.3)


#👀 LATENCY ----

#create a column 'latency' where the date of each row is subtrated from the first row #this data isn't used but it shows you all latecies relative to the reference tag for every visit . its a good sanity check to make sure dataframe is right 
latency_eyes_102_2019<-eyes_102_2019_b.3%>%
  mutate(latency = Date - Date[row_number()==1])

###here you could just eyeball the row that is the first visit for each tag, extrat that data and save it. 
#the next code in theory should extract just the first visit for each tag and you could save that df without having to filter by row. 

#extract the latency of the first visit from the reference tag for each tag from the t2 dataframe. please double check the final df matches the one above for latency PER TAG. cross reference output here with the above df output. latencies should be the same 

latency_final_eyes_102_2019<-eyes_102_2019_b.3%>%
  arrange(Date)%>%
  mutate(latency = Date - Date[row_number()==1])%>%
  group_by(RFID)%>%
  slice(1)%>%
  ungroup()

head(latency_final_eyes_102_2019)
glimpse(latency_final_eyes_102_2019)

#💾 SAVING----
setwd("~/GitHub/great_tits/scripts")
write.csv(latency_final_eyes_102_2019,file="totalVisits45m_mw102_2019.csv")

##everything past here other than saving your dataframe is redundant. 





# old ----
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
uniqueIDs #lets you see the different ids in the df

eyes_102_2019_f<-eyes_102_2019_f[!grepl('TagID_1', eyes_102_2019_f$RFID),]# removing tagid one 


 
eyes_102_2019_f<-eyes_102_2019_f[!eyes_102_2019_f$RFID=="",] #remove blank rows
head(eyes_102_2019_f)

# 🌎 Data exploration ----

uniqueID<- unique(eyes_102_2019_f$RFID)
uniqueID #only 3 different individuals
#"0300024FEF" "011016C265" "01101737FA"

#"0300024FEF" -> is the start and stop indicator 

referenceTag<-eyes_102_2019_f[eyes_102_2019_f$RFID == '0300024FEF',]#flituring out 0300030EFF
referenceTag # can see when the indicator was used


eyes_102_2019_f<-eyes_102_2019_f%>%
  mutate(Date=ymd_hms(Date))
class(eyes_102_2019_f$Date) # changing the data format to POSIXct class

#👀🐤 Latency ----

#extracting the first occurence of each RFID tag
latency<-eyes_102_2019_f %>% 
  arrange(Date) %>% 
  group_by(RFID) %>% 
  slice(1) %>% # makes the data set and arrages it
  ungroup()

#df with all of the first occurances of tags
referencetag<-eyes_102_2019_f[eyes_102_2019_f$RFID == '0300024FEF',]
referencetag# checking 


eyes_102_2019_f.2<-slice(referencetag, 2)# making a 2nd row
eyes_102_2019_f.2


# removing 0300024FEF from the df latency + bind eye_102_2019_f.2 to latency
latency<-latency[-c(1),]
latency # not sure if this works 

latency2<-bind_rows(eyes_102_2019_f.2,latency)
names(latency)
# date is the first column 

#create column latency (sub) 

latency3<-latency2 %>% 
  mutate(latency = Date - Date [row_number()==1])# - to find lat

latency4<-eyes_102_2019_f%>%
  arrange(Date)%>%
  mutate(latency = Date - Date[row_number()==2])%>%
  group_by(RFID)%>%
  slice(1)%>%
  ungroup()


#⏰  45mins----

eyes_102_2019_t<-subset(eyes_102_2019_f,Date >= as.POSIXct('2019-05-03 07:56:01', tz="UTC")) # do from last indicator

eyes_102_2019_t<-subset(eyes_102_2019_f,Date <= as.POSIXct('2019-05-03 08:41:01', tz="UTC")) # make sure 45 mins

totalVisits1hr<-eyes_102_2019_t%>%
  count(RFID, sort = TRUE) 

totalVisits1hr<-cbind(totalVisits1hr, nestbox='mw102')
totalVisits1hr<-cbind(totalVisits1hr, year='2019')
head(totalVisits1hr)
write.csv(totalVisits1hr,file="totalVisits45m_mw102_2019.csv")

#The reference tag is 0300024FEF. The experimental time should be restricted to 45 minutes from the last time the reference tag was read. Following the GIT hub tutorial, you should be able to do this with the new data. 




