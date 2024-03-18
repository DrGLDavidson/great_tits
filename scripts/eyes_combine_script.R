library(dplyr)

path<-setwd("~/GitHub/great_tits/scripts") # setting the path

eye_files<-list.files(path, pattern = "totalVisits", recursive = TRUE) # finding the comman word

eyes<-lapply(eye_files, read.csv) # reading the file 

eyes_combine<- dplyr::bind_rows(eyes)


setwd("~/GitHub/great_tits/scripts")
write.csv(eyes_combine,file="eyes_combine")


repeat_birds<- duplicated(eyes_combine$RFID)
repeat_birds<- as.data.frame(repeat_birds)


eyes_with_repeats <- merge(eyes_combine,repeat_birds,by="row.names", all.x = T) 
write.csv(eyes_with_repeats,file="eyes_with_repeats") # man check control f

# age ----


setwd("~/GitHub/great_tits/data")
individual_age<-read.csv(file= "IndividualsTable_ageatBreedingSeason.csv", header=TRUE)

names(individual_age)

ias<-individual_age %>% 
  select(age,sex,Age2019BreedingSeason,Age2020BreedingSeason,Age2021BreedingSeason,pitID)

names(ias)[names(ias) == "pitID"] <- "RFID" # chanign the names 
names(ias)

eyes_ias <-merge (eyes_with_repeats,ias, by = "RFID", all.x = T)
write.csv(eyes_ias,file="eyes_ias.csv")

# task 
# excle man copy dates to age col using fliture (new coloumn age at time of sampling - can fliture by year)
# na - find why the na
# 20 have missing data (age at breeding season and add man) = gab does this # /
# sex - gab will look /
# methods - what i have done with the data - extracting master data base etc 
# mention issues - cross reference id 

# next time - fledgling 

# nestID 2019 
# select antenna 2  and number fledged - new df 
# antenna 2 - rename to RFID
#break down into year eyes_ias (fliture, 3 files)
# merge by RFID - merge onto the master df 
# repeat for antenna 1 
# repeat for 2020, 2021
# bind rows 


# f

path<-setwd("~/GitHub/great_tits/data")
repeats<-read.csv(file = "eyes_ias_repeatability.csv", header = TRUE)
nest_2019<-read.csv(file = "nest_id_from_2019.csv", header = TRUE)
nest_2020<-read.csv(file = "nest_id_from_2020.csv", header = TRUE)
nest_2021<-read.csv(file = "nest_id_from_2021.csv", header = TRUE)

head(repeats)

names(nest_2019)
df_2019<- nest_2019 %>% 
  select(antenna_pit_tag_id_1, number_fledged, site_box_number) # can double check if matching

names(df_2019)[names(df_2019) == "antenna_pit_tag_id_1"] <- "RFID" # renaming RFID 



df_2019_2<- nest_2019 %>% 
  select(antenna_pit_tag_id_2, number_fledged, site_box_number) 

names(df_2019_2)[names(df_2019_2) == "antenna_pit_tag_id_2"] <- "RFID"


repeats_2019<-repeats[repeats$year == '2019',]

df_2019_final<-bind_rows(df_2019,df_2019_2) 

repeats_2019 <-merge (repeats_2019,df_2019_final, by = "RFID", all.x = T)

rm(df_2019, data, df_2019_2, nest_2019) # how to remove 
# dont over right repeats_2019 

# 2020

# 2021

# final bind 

final_data<- bind_rows(repeats_2019, repeats_2020, repeats_2021) # 


