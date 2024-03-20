setwd("~/GitHub/great_tits/data") # setting the path
tsm<-read.csv(file= "final_data_edit.csv", header=TRUE) # inserting the new data

final_data_glm<-tsm%>%
  select(RFID, Count_may, Date, Hmsec, CounterID, nestbox, year, latency, repeat_birds, sex, Age2019BreedingSeason, Age2020BreedingSeason, Age2021BreedingSeason, ageFinal, number_fledged, site_box_number)
names(final_data_glm) # flituring the data
