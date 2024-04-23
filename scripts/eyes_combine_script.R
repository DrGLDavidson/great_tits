# Packages ----

library(dplyr)

# merging scripts ----

path<-setwd("~/GitHub/great_tits/scripts") # setting the path

eye_files<-list.files(path, pattern = "totalVisits", recursive = TRUE) # finding the comman word

eyes<-lapply(eye_files, read.csv) # reading the file 

eyes_combine<- dplyr::bind_rows(eyes) 


setwd("~/GitHub/great_tits/scripts")
write.csv(eyes_combine,file="eyes_combine")


repeat_birds<- duplicated(eyes_combine$RFID) # finding the dups 
repeat_birds<- as.data.frame(repeat_birds) # dups as df


eyes_with_repeats <- merge(eyes_combine,repeat_birds,by="row.names", all.x = T) 
write.csv(eyes_with_repeats,file="eyes_with_repeats") # man check control f

# Adding new variables to the df  ----


setwd("~/GitHub/great_tits/data")
individual_age<-read.csv(file= "IndividualsTable_ageatBreedingSeason.csv", header=TRUE) # reading the data

names(individual_age)

ias<-individual_age %>% 
  select(age,sex,Age2019BreedingSeason,Age2020BreedingSeason,Age2021BreedingSeason,pitID)

names(ias)[names(ias) == "pitID"] <- "RFID" # changing the names 
names(ias)

eyes_ias <-merge (eyes_with_repeats,ias, by = "RFID", all.x = T) # merge by RFID 
write.csv(eyes_ias,file="eyes_ias.csv")

# task 
# excle man copy dates to age col using fliture (new coloumn age at time of sampling - can fliture by year)
# na - find why the na
# 20 have missing data (age at breeding season and add man) = gab does this # /
# sex - gab will look /
# methods - what i have done with the data - extracting master data base etc 
# mention issues - cross reference id 




# fledglings ----

# nestID 2019 
# select antenna 2  and number fledged - new df 
# antenna 2 - rename to RFID
#break down into year eyes_ias (fliture, 3 files)
# merge by RFID - merge onto the master df 
# repeat for antenna 1 
# repeat for 2020, 2021
# bind rows 

setwd("~/GitHub/great_tits/data") #setting in the working directory 

repeats<-read.csv(file = "eyes_ias_repeatability.csv", header = TRUE) #loading in the data 
nest_2019<-read.csv(file = "nest_id_from_2019.csv", header = TRUE) # fledgling data 
nest_2020<-read.csv(file = "nest_id_from_2020.csv", header = TRUE)
nest_2021<-read.csv(file = "nest_id_from_2021.csv", header = TRUE)

# 2019 ----

head(repeats) 
names(nest_2019)

df_2019<- nest_2019 %>% 
  select(antenna_pit_tag_id_1, number_fledged, site_box_number) # can cross ref using the site box number and the mw 

names(df_2019)[names(df_2019) == "antenna_pit_tag_id_1"] <- "RFID" # renaming RFID 



df_2019_2<- nest_2019 %>% 
  select(antenna_pit_tag_id_2, number_fledged, site_box_number) # making a second df for the ID 2

names(df_2019_2)[names(df_2019_2) == "antenna_pit_tag_id_2"] <- "RFID"


repeats_2019<-repeats[repeats$year == '2019',] # not sure what this does 

df_2019_final<-bind_rows(df_2019, df_2019_2) # binding the 2 df together 

repeats_2019 <-merge (repeats_2019, df_2019_final, by = "RFID", all.x = T) # merging the dfs by RFID  


rm(df_2019, data, df_2019_2, nest_2019) # how to remove 
# don't write over right repeats_2019 


# 2020 -----


df_2020<- nest_2020 %>% 
  select(antenna_pit_tag_id_1, number_fledged, site_box_number) # can cross ref using the site box number and the mw 

names(df_2020)[names(df_2020) == "antenna_pit_tag_id_1"] <- "RFID" # renaming RFID 


df_2020_2<- nest_2020 %>% 
  select(antenna_pit_tag_id_2, number_fledged, site_box_number) # making a second df for the ID 2

names(df_2020_2)[names(df_2020_2) == "antenna_pit_tag_id_2"] <- "RFID"


repeats_2020<-repeats[repeats$year == '2020',] # not sure what this does 

df_2020_final<-bind_rows(df_2020, df_2020_2) # binding the 2 df together 

repeats_2020 <-merge (repeats_2020, df_2020_final, by = "RFID", all.x = T) # merging the dfs by RFID  


rm(df_2020, df_2020_2, nest_2020) # how to remove 

# 2021 -----

df_2021<- nest_2021 %>% 
  select(antenna_pit_tag_id_1, number_fledged, site_box_number) # can cross ref using the site box number and the mw 

names(df_2021)[names(df_2021) == "antenna_pit_tag_id_1"] <- "RFID" # renaming RFID 


df_2021_2<- nest_2021 %>% 
  select(antenna_pit_tag_id_2, number_fledged, site_box_number) # making a second df for the ID 2

names(df_2021_2)[names(df_2021_2) == "antenna_pit_tag_id_2"] <- "RFID"


repeats_2021<-repeats[repeats$year == '2021',] # not sure what this does 

df_2021_final<-bind_rows(df_2021, df_2021_2) # binding the 2 df together 

repeats_2021 <-merge (repeats_2021, df_2021_final, by = "RFID", all.x = T) # merging the dfs by RFID  


# 2021/2019 wrong variable type 

str(repeats_2019)
str(repeats_2020)
str(repeats_2021)# seeing the variable types
# 2019 fledged is an character - wrong variable type 


repeats_2021$number_fledged<- as.integer(repeats_2021$number_fledged)# changing the variable types 
repeats_2019$number_fledged<- as.integer(repeats_2019$number_fledged)# changing the variable types 

str(repeats_2019)
str(repeats_2020)
str(repeats_2021) 
str(final_data_glm) 

rm(df_2021, data, df_2021_2, nest_2021) # how to remove 

# binding fledglings to make the final df -----

final_data<- bind_rows(repeats_2019, repeats_2020, repeats_2021) #binding to make the final df

setwd("~/GitHub/great_tits/scripts")
write.csv(final_data,file="final_data.csv")


#_______________________----

# count since may added man ----









