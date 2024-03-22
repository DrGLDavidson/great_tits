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

# R  = 0.434 (0.1-0.5) mod 
# SE = dont need to report 
# CI = 
# p 

# plot? - 

#_______________----

rep2 <- rpt(latency ~sex + ageFinal + year+ (1|nestbox)+ (1 | RFID), grname = "RFID", data = final_data_glm, 
            datatype = "Gaussian", nboot = 1000, npermut = 0)
print(rep2)

# not when controlled

# _____________-----

##GLMM

#TEST 2: 

lsmodel1<-lmer(latency ~  sex*ageFinal+Count_may+(1|nestbox)+ (1 | RFID), data = final_data_glm)  ##need to add fledglings
summary(lsmodel1)

lsmodel1 %>% 
  broom::tidy(conf.int = F)#add 95% conf intervals - doesn't work

performance::check_model(lsmodel1)# seeing the fit of the model,in a image form

drop1(lsmodel1, test = "F")# can look at the AIC, want the smallest AIC


#____________----

#interaction is not significant and can be removed
lsmodel2<-lmer(latency ~  sex+ageFinal+ Count_may+(1|nestbox)+ (1 | RFID), data = final_data_glm)  ##need to add fledglings
summary(lsmodel2)

#####FLEDGLING

model3<-lmer(number_fledged ~ sex*latency + ageFinal*latency + Count_may +(1|nestbox)+ (1 | RFID), data = final_data_glm)
summary(model3)


model3b<-lmer(number_fledged ~ sex*latency + ageFinal + Count_may +(1|nestbox)+ (1 | RFID), data = final_data_glm)
summary(model3b)


model4<-lmer(number_fledged ~ sex+latency + ageFinal + Count_may +(1|nestbox)+ (1 | RFID), data = final_data_glm)
summary(model4)
performance::check_model(model4)
# different to what other studues have found

names(final_data_glm)
#drop non-significant interactions and rerun

model5<-lmer(number_fledged ~ sex*latency + ageFinal + Count_may + year + (1|nestbox)+ (1 | RFID), data = final_data_glm)
summary(model5)
# 45 mins 
# reproductive success and laydate, the later breeding in the year the poorer the amount of offspring, so there isnt enough food - not seeing this here so this is different 
# mandingly woods (G) great tit chicks look very good quality and they all seem to be of the size but in cork they is alot of varibltiy in the quality 
#mad good food? not effect on 
# predictions for personalty, better risk and shy on the year (habit and predator), could be the same over the different years, no selection (lit)


# intro, methods and dis
# results the least amount of words 
# main concepts, why import 
# cover page 
# formative 9th 8am 
# questions write down and email on Friday the 5th 
# stats phil, concepts of behavour ellen or becky 










setwd("~/GitHub/great_tits/data") # setting the path
tsm<-read.csv(file= "final_data_edit.csv", header=TRUE) # inserting the new data

final_data_glm<-tsm%>%
  select(RFID, Count_may, Date, Hmsec, CounterID, nestbox, year, latency, repeat_birds, sex, Age2019BreedingSeason, Age2020BreedingSeason, Age2021BreedingSeason, ageFinal, number_fledged, site_box_number)
names(final_data_glm) # flituring the data
