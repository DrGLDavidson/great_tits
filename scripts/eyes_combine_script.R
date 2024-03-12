library(dplyr)

path<-setwd("~/GitHub/great_tits/scripts")

eye_files<-list.files(path, pattern = "totalVisits", recursive = TRUE)

eyes<-lapply(eye_files, read.csv)

eyes_combine<- dplyr::bind_rows(eyes)


setwd("~/GitHub/great_tits/scripts")
write.csv(eyes_combine,file="eyes_combine")
