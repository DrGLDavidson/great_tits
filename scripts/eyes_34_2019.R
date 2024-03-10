# 📦 PACKAGES ----
library(tidyverse) # a range of helpful packages
library(janitor)# helps to format the data
library(lubridate)# good dates 


#💾 IMPORTING DATA ----

setwd("~/GitHub/great_tits/data/2019 Visit counters/090519_MW27_control_MW34_eyes_MW6_eyes")# setting the working directory

eyes_34_2019<-read.delim(file= "C1256RT.TXT", sep="\t", header=TRUE)#loading in data
names(eyes_34_2019)


#🧽 CLEANING DATA ---- 

eyes_34_2019_f<-eyes_34_2019%>%
  select(Date, Hmsec, CounterID, TagID_1)
names(eyes_34_2019_f) # flituring the data


names(eyes_34_2019_f)[names(eyes_34_2019_f) == "TagID_1"] <- "RFID" # renaming RFID 


uniqueIDs<-unique(eyes_34_2019_f$RFID)
uniqueIDs #lets you see the different ids in the df
# "0300024FEF" ""           "TagID_1 " "0419483034" "0110174FC8" "01103F5B01" "011016FBDD" "01101738AB" "011016FB0C"



eyes_34_2019_f<-eyes_34_2019_f[!grepl('TagID_1', eyes_34_2019_f$RFID),]# removing tagid one 

eyes_34_2019_f<-eyes_34_2019_f[!eyes_34_2019_f$RFID=="",]#remove blank rows
head(eyes_34_2019_f)

# 🌎 DATA EXPLORATION ----

uniqueID<- unique(eyes_34_2019_f$RFID)
uniqueID #only 3 different individuals
#"0300024FEF" "0419483034" "0110174FC8" "01103F5B01" "011016FBDD" "01101738AB" "011016FB0C"

#"0300024FEF", "0300030EFF" -> is the start and stop indicator 

referencetag<-eyes_34_2019_f[eyes_34_2019_f$RFID == '0300024FEF',]#flituring out 0300030EFF
referencetag # can see when the indicator was used


eyes_34_2019_f<-eyes_34_2019_f%>%
  mutate(Date=ymd_hms(Date))
class(eyes_34_2019_f$Date) # changing the data format to POSIXct class

#with year/mw ----

#⏰ 45MINS ----

#first need to filter according to experiment time and last RFID tag before experiment start. Can do this my row number but also time may be better. Look at referencetag dataframe for time values. 


eyes_34_2019_t<-subset(eyes_34_2019_f,Date >= as.POSIXct('2019-05-09 10:08:16 ', tz="UTC")) # do from last indicator

#this time is 45 minutes from the above time manually write this
eyes_34_2019_t2<-subset(eyes_34_2019_t,Date <= as.POSIXct('2019-05-09 10:54:13 ', tz="UTC")) # make sure 45 mins
###FOR EMMA TO ADD THE CODE TO ADD THE NESTBOX AND YEAR TO data frame t2


# 📅 DATE ----
# adding the date and the nest box name to the df

eyes_34_2019_b.3<-eyes_34_2019_t2

eyes_34_2019_b.3<-cbind(eyes_34_2019_b.3, nestbox='mw34') 
eyes_34_2019_b.3<-cbind(eyes_34_2019_b.3, year='2019')
head(eyes_34_2019_b.3)


#👀 LATENCY ----

#create a column 'latency' where the date of each row is subtrated from the first row #this data isn't used but it shows you all latecies relative to the reference tag for every visit . its a good sanity check to make sure dataframe is right 
latency_eyes_34_2019<-eyes_34_2019_b.3%>%
  mutate(latency = Date - Date[row_number()==1])

###here you could just eyeball the row that is the first visit for each tag, extrat that data and save it. 
#the next code in theory should extract just the first visit for each tag and you could save that df without having to filter by row. 

#extract the latency of the first visit from the reference tag for each tag from the t2 dataframe. please double check the final df matches the one above for latency PER TAG. cross reference output here with the above df output. latencies should be the same 

latency_final_eyes_34_2019<-eyes_34_2019_b.3%>%
  arrange(Date)%>%
  mutate(latency = Date - Date[row_number()==1])%>%
  group_by(RFID)%>%
  slice(1)%>%
  ungroup()

head(latency_final_eyes_34_2019)
glimpse(latency_final_eyes_34_2019)

#💾 SAVING----
setwd("~/GitHub/great_tits/scripts")
write.csv(latency_final_eyes_34_2019,file="totalVisits45m_mw34_2019.csv")

##everything past here other than saving your dataframe is redundant. 
