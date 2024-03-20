# 📦PACKAGES ----
library(lme4)
library(lmerTest)
library(rptR)
library(Matrix)
library(tidyverse) # a range of helpful packages
library(janitor)# helps to format the data
library(kableExtra)
library(performance)# needed to instal for the stats graphs (posterior check)
library(ggExtra)
library(broom.mixed) # broom tables 
library(patchwork)

#_________________-----
# LOADING IN DATA 

setwd("~/GitHub/great_tits/data") # setting the path
tsm<- read.csv(file= "final_data_edit.csv", header=TRUE) # inserting the new data

final_data_glm<-tsm %>%
  select(RFID, Count_may, Date, Hmsec, CounterID, nestbox, year, latency, repeat_birds, sex, Age2019BreedingSeason, Age2020BreedingSeason, Age2021BreedingSeason, ageFinal, number_fledged, site_box_number)
names(final_data_glm) # flituring the data
#___________-----

#TEST 1 QUESTION: IS LATENCY REPEATABLE AND THEREFORE A PERSONALITY TRAIT? 

#repeatability in simplest form 'unadjusted'
rep1 <- rpt(latency ~(1 | RFID), grname = "RFID", data = final_data_glm, 
            datatype = "Gaussian", nboot = 1000, npermut = 0)
print(rep1)

#_______________----

rep2 <- rpt(latency ~sex + ageFinal + year+ (1|nestbox)+ (1 | RFID), grname = "RFID", data = final_data_glm, 
            datatype = "Gaussian", nboot = 1000, npermut = 0)
print(rep2)

# _____________-----

##GLMM

#TEST 2: 

lsmodel1<-lmer(latency ~  sex*ageFinal+(1|nestbox)+ (1 | RFID), data = final_data_glm)  ##need to add fledglings
summary(lsmodel1)

lsmodel1 %>% 
  broom::tidy(conf.int = F)#add 95% conf intervals - doesn't work

performance::check_model(lsmodel1)# seeing the fit of the model,in a image form

drop1(lsmodel1, test = "F")# can look at the AIC, want the smallest AIC
# diet:starting AIC not extremely different

#____________----

#interaction is not significant and can be removed
lsmodel2<-lmer(latency ~  sex+ageFinal+(1|nestbox)+ (1 | RFID), data = final_data_glm)  ##need to add fledglings
summary(lsmodel2)

#####FLEDGLING

model3<-lmer(number_fledged+ sex*latency + age*latency + dayOfTheMonth +(1|nestbox)+ (1 | RFID), data = df)

#drop non-significant interactions and rerun















setwd("~/GitHub/great_tits/data") # setting the path
tsm<-read.csv(file= "final_data_edit.csv", header=TRUE) # inserting the new data

final_data_glm<-tsm%>%
  select(RFID, Count_may, Date, Hmsec, CounterID, nestbox, year, latency, repeat_birds, sex, Age2019BreedingSeason, Age2020BreedingSeason, Age2021BreedingSeason, ageFinal, number_fledged, site_box_number)
names(final_data_glm) # flituring the data
