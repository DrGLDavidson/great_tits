# 📦 PACKAGES ----
library(tidyverse) # a range of helpful packages
library(janitor)# helps to format the data
library(lubridate)# good dates 


#💾 IMPORTING DATA ----

setwd("~/GitHub/great_tits/data/2020 Visit counters/160520_MW14_ID_MW49_eyes_MW66_control")# setting the working directory

eyes_49_2020<-read.delim(file= "C1252RT.TXT", sep="\t", header=TRUE)#loading in data
names(eyes_49_2020)


#🧽 CLEANING DATA ---- 

eyes_49_2020_f<-eyes_49_2020%>%
  select(Date, Hmsec, CounterID, TagID_1)
names(eyes_49_2020_f) # flituring the data


names(eyes_49_2020_f)[names(eyes_49_2020_f) == "TagID_1"] <- "RFID" # renaming RFID 


uniqueIDs<-unique(eyes_49_2020_f$RFID)
uniqueIDs #lets you see the different ids in the df
# ""           "TagID_1 "   "0300030EFF" "01103FC5AF" "01103F4206" "01103FD262" "011016F4AD" "01103FDC8F"

eyes_49_2020_f<-eyes_49_2020_f[!grepl('TagID_1', eyes_49_2020_f$RFID),]# removing tagid one 

eyes_49_2020_f<-eyes_49_2020_f[!eyes_49_2020_f$RFID=="",]#remove blank rows
head(eyes_49_2020_f)

# 🌎 DATA EXPLORATION ----

uniqueID<- unique(eyes_49_2020_f$RFID)
uniqueID #only 3 different individuals
# "0300030EFF" "01103FC5AF" "01103F4206""01103FD262" "011016F4AD" "01103FDC8F"

#"0300024FEF", "0300030EFF" -> is the start and stop indicator 

referencetag<-eyes_49_2020_f[eyes_49_2020_f$RFID == '0300030EFF',]#flituring out 0300030EFF
referencetag # can see when the indicator was used
# different time written down online 


eyes_49_2020_f<-eyes_49_2020_f%>%
  mutate(Date=ymd_hms(Date))
class(eyes_49_2020_f$Date) # changing the data format to POSIXct class

#with year/mw ----

#⏰ 45MINS ----

#first need to filter according to experiment time and last RFID tag before experiment start. Can do this my row number but also time may be better. Look at referencetag dataframe for time values. 


eyes_49_2020_t<-subset(eyes_49_2020_f,Date >= as.POSIXct('2020-05-16 12:11:52', tz="UTC")) # do from last indicator

#this time is 45 minutes from the above time manually write this
eyes_49_2020_t2<-subset(eyes_49_2020_t,Date <= as.POSIXct('2020-05-16 13:08:46', tz="UTC")) # make sure 45 mins
###FOR EMMA TO ADD THE CODE TO ADD THE NESTBOX AND YEAR TO data frame t2


# 📅 DATE ----
# adding the date and the nest box name to the df

eyes_49_2020_b.3<-eyes_49_2020_t2

eyes_49_2020_b.3<-cbind(eyes_49_2020_b.3, nestbox='mw49') 
eyes_49_2020_b.3<-cbind(eyes_49_2020_b.3, year='2020')
head(eyes_49_2020_b.3)


#👀 LATENCY ----

#create a column 'latency' where the date of each row is subtrated from the first row #this data isn't used but it shows you all latecies relative to the reference tag for every visit . its a good sanity check to make sure dataframe is right 
latency_eyes_49_2020<-eyes_49_2020_b.3%>%
  mutate(latency = Date - Date[row_number()==1])

###here you could just eyeball the row that is the first visit for each tag, extrat that data and save it. 
#the next code in theory should extract just the first visit for each tag and you could save that df without having to filter by row. 

#extract the latency of the first visit from the reference tag for each tag from the t2 dataframe. please double check the final df matches the one above for latency PER TAG. cross reference output here with the above df output. latencies should be the same 

latency_final_eyes_49_2020<-eyes_49_2020_b.3%>%
  arrange(Date)%>%
  mutate(latency = Date - Date[row_number()==1])%>%
  group_by(RFID)%>%
  slice(1)%>%
  ungroup()

head(latency_final_eyes_49_2020)
glimpse(latency_final_eyes_49_2020)

#💾 SAVING----
setwd("~/GitHub/great_tits/scripts")
write.csv(latency_final_eyes_49_2020,file="totalVisits45m_mw49_2020.csv")

##everything past here other than saving your dataframe is redundant. 

