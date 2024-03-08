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

#first need to filter according to experiment time and last RFID tag before experiment start. Can do this my row number but also time may be better. Look at referencetag dataframe for time values. 


#⏰  45mins----


eyes_8_2019_t<-subset(eyes_8_140_2019_f,Date >= as.POSIXct('2019-05-05 10:17:40', tz="UTC")) # do from last indicator

#this time is 45 minutes from the above time manually write this
eyes_8_2019_t2<-subset(eyes_8_2019_t,Date <= as.POSIXct('2019-05-05 11:02:54', tz="UTC")) # make sure 45 mins
###FOR EMMA TO ADD THE CODE TO ADD THE NESTBOX AND YEAR TO data frame t2

#create a column 'latency' where the date of each row is subtrated from the first row #this data isn't used but it shows you all latecies relative to the reference tag for every visit . its a good sanity check to make sure dataframe is right 
latency_eyes_8_2019<-eyes_8_2019_t2%>%
  mutate(latency = Date - Date[row_number()==1])

###here you could just eyeball the row that is the first visit for each tag, extrat that data and save it. 
#the next code in theory should extract just the first visit for each tag and you could save that df without having to filter by row. 

#extract the latency of the first visit from the reference tag for each tag from the t2 dataframe. please double check the final df matches the one above for latency PER TAG. cross reference output here with the above df output. latencies should be the same 

latency_final_eyes_8_2019<-eyes_8_2019_t2%>%
  arrange(Date)%>%
  mutate(latency = Date - Date[row_number()==1])%>%
  group_by(RFID)%>%
  slice(1)%>%
  ungroup()

##everything past here other than saving your dataframe is redundant. 

###add a column that extracts the time between each visit regardless of tag
btwvisits<-eyes_8_2019_t2%>%
  arrange(Date)%>%
  mutate(BtwVisitInterval = Date - lag(Date, default=first(Date)))

#add a column that extracts the time between each visit according to tag
wtnvisits<-btwvisits%>%
  group_by(RFID)%>%
  arrange(Date)%>%
  mutate(WtnVisitInterval = Date - lag(Date, default=first(Date)))%>%
  arrange(RFID)%>%
  ungroup()


#extracting the first occurence of each RFID tag and latency of other tags after reference tag 
latency<-eyes_8_2019_t2 %>% 
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
 

eyes_8_140_2019_b<-cbind(eyes_8_140_2019_b, nestbox='mw10')# maybe do after as different boxes - change name 
eyes_8_140_2019_b<-cbind(eyes_8_140_2019_b, year='2019')
head(eyes_8_140_2019_b)


# multiples 👯‍♀️----
# select out each, make new df and select by time 

#column names 
eyes_8_2019<- eyes_8_140_2019_b[c(1,3, 4, 6),]
head(eyes_8_2019)

#delete multiple rows
eyes_8_2019_1<- eyes_8_140_2019_b[-c(43:1),]
eyes_8_2019_1


#column names 
eyes_140_2019<- eyes_8_140_2019_b[c(1, 3, 4, 6),] #not working 
head(eyes_140_2019)

#delete multiple rows
eyes_140_2019_1<- eyes_8_140_2019_b[-c(44,70)] #not working 
head(eyes_140_2019_1)


#⏰  45mins----


eyes_8_140_2019_t<-subset(eyes_8_140_2019_f,Date >= as.POSIXct('2019-05-03 08:04:16', tz="UTC")) # do from last indicator

eyes_8_140_2019_t<-subset(eyes_8_140_2019_f,Date <= as.POSIXct('2019-05-03 08:49:16', tz="UTC")) # make sure 45 mins

write.csv(totalVisits45min,file="totalVisits45m_mw10_2019.csv")
