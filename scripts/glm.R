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
library(GGally)
library(gt) 
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
# SE = don't need to report 
# CI = lines in the graph, the should never go across 0, in this case they don't - [0, 0.764]
# p = 0.0211

# plot? - shown in the video and on the doc (have the same rank order differences )

#plot 

only_repeat_birds <- final_data_glm %>%
filter(!(repeat_birds %in% c("FALSE"))) # flituring out non repeats
# 011016FA48 doesnt repeat ?
# changed 
# doesnt show all of the data 

ggplot(data = only_repeat_birds, aes(x =year, y =latency)) +
  geom_point(aes(color = RFID),
             alpha = 0.8, 
             show.legend = FALSE)+
  xlim(2019,2021)+
  ylim(4,2600)+
  geom_line(aes(color = RFID))+
  theme_classic()+
  labs(x = "Year", # labs names
       y = "Latency (mins)")



head(final_data_glm)


#_______________----

rep2 <- rpt(latency ~sex + ageFinal + year+ (1|nestbox)+ (1 | RFID), grname = "RFID", data = final_data_glm, 
            datatype = "Gaussian", nboot = 1000, npermut = 0)

summary(rep2)


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

summary_table1 <- 
  lsmodel1 %>% 
  broom::tidy(conf.int = TRUE) %>% 
  mutate(p.value = scales::pvalue(p.value)) %>% # changes the pvalues <0.001
  rename("Term"="term",
         "Coefficient" = "estimate", # changing the names to be better
         "Standard Error" = "std.error",
         "t" = "statistic",
         "p value" = "p.value",
         "lower.CI" = "conf.low",
         "upper.CI" = "conf.high")%>%
  mutate(across(c(Coefficient: t), round,5)) %>% 
  kbl() %>% 
  kable_styling(latex_options = "hold_position") %>% # to stop the table moving in markdown!!!!
  #  row_spec(c(3,5,7), color = 'white', background = 'purple') %>% # the most sig highlighted in colour
  row_spec(c(0), italic = TRUE, align = "c") %>% # titles italic
  kable_styling() # fancy style

# remove effect and group
summary_table2 <-
  remove_column(summary_table1,1) 
  remove_column(summary_table2, 1)
  

# remove rows by cropping 


#____________----

#interaction is not significant and can be removed
lsmodel2<-lmer(latency ~  sex+ageFinal+ Count_may+(1|nestbox)+ (1 | RFID), data = final_data_glm)  ##need to add fledglings
summary(lsmodel2)

lsmodel2.1<-lmer(latency ~  sex+ageFinal+ Count_may+number_fledged+ year+(1|nestbox)+ (1 | RFID), data = final_data_glm)  ##fledglings not sig so was removed 
summary(lsmodel2.1)

fledged table_2.1<- 
  lsmodel2.1 %>% 
  broom::tidy(conf.int = TRUE) %>% 
  mutate(p.value = scales::pvalue(p.value)) %>% # changes the pvalues <0.001
  rename("Term"="term",
         "Coefficient" = "estimate", # changing the names to be better
         "Standard Error" = "std.error",
         "t" = "statistic",
         "p value" = "p.value",
         "lower.CI" = "conf.low",
         "upper.CI" = "conf.high")%>%
  mutate(across(c(Coefficient: t), round,5)) %>% 
  kbl() %>% 
  kable_styling(latex_options = "hold_position") %>% # to stop the table moving in markdown!!!!
  #  row_spec(c(3,5,7), color = 'white', background = 'purple') %>% # the most sig highlighted in colour
  row_spec(c(0), italic = TRUE, align = "c") 





#___________________________________________________________________
#adding year -----


lsmodel2.2<-lmer(latency ~  sex+ageFinal+ Count_may+ year +(1|nestbox)+ (1 | RFID), data = final_data_glm)  
summary(lsmodel2.2)

# new table 

count_may_table <- 
  lsmodel2.2 %>% 
  broom::tidy(conf.int = TRUE) %>% 
  mutate(p.value = scales::pvalue(p.value)) %>% # changes the pvalues <0.001
  rename("Term"="term",
         "Coefficient" = "estimate", # changing the names to be better
         "Standard Error" = "std.error",
         "t" = "statistic",
         "p value" = "p.value",
         "lower.CI" = "conf.low",
         "upper.CI" = "conf.high")%>%
  mutate(across(c(Coefficient: t), round,5)) %>% 
  kbl() %>% 
  kable_styling(latex_options = "hold_position") %>% # to stop the table moving in markdown!!!!
  #  row_spec(c(3,5,7), color = 'white', background = 'purple') %>% # the most sig highlighted in colour
  row_spec(c(0), italic = TRUE, align = "c") %>% # titles italic
  kable_styling() # fancy style

#____________________------
# removing count may ====
lsmodel2.3<-lmer(latency ~  sex+ageFinal+ year +(1|nestbox)+ (1 | RFID), data = final_data_glm)# removed may 
summary(lsmodel2.3)


lsmodel2.33<-lmer(latency ~  sex+ageFinal+ year:sex+(1|nestbox)+ (1 | RFID), data = final_data_glm)# removed may 
summary(lsmodel2.33)

summary_table2.33 <- 
  lsmodel2.33 %>% 
  broom::tidy(conf.int = TRUE) %>% 
  mutate(p.value = scales::pvalue(p.value)) %>% # changes the pvalues <0.001
  rename("Term"="term",
         "Coefficient" = "estimate", # changing the names to be better
         "Standard Error" = "std.error",
         "t" = "statistic",
         "p value" = "p.value",
         "lower.CI" = "conf.low",
         "upper.CI" = "conf.high")%>%
  mutate(across(c(Coefficient: t), round,5)) %>% 
  kbl() %>% 
  kable_styling(latex_options = "hold_position") %>% # to stop the table moving in markdown!!!!
  #  row_spec(c(3,5,7), color = 'white', background = 'purple') %>% # the most sig highlighted in colour
  row_spec(c(5), color = 'white', background = 'deepskyblue') %>% # the most sig highlighted in colour
  row_spec(c(0), italic = TRUE, align = "c") %>% # titles italic
  kable_styling() # fancy style



summary_table2.3 <- 
  lsmodel2.3 %>% 
  broom::tidy(conf.int = TRUE) %>% 
  mutate(p.value = scales::pvalue(p.value)) %>% # changes the pvalues <0.001
  rename("Term"="term",
         "Coefficient" = "estimate", # changing the names to be better
         "Standard Error" = "std.error",
         "t" = "statistic",
         "p value" = "p.value",
         "lower.CI" = "conf.low",
         "upper.CI" = "conf.high")%>%
  mutate(across(c(Coefficient: t), round,5)) %>% 
  kbl() %>% 
  kable_styling(latex_options = "hold_position") %>% # to stop the table moving in markdown!!!!
  #  row_spec(c(3,5,7), color = 'white', background = 'purple') %>% # the most sig highlighted in colour
  row_spec(c(4), color = 'white', background = 'purple3') %>% # the most sig highlighted in colour
  row_spec(c(0), italic = TRUE, align = "c") %>% # titles italic
  kable_styling() # fancy style

# graph year lat ----


  ggplot(final_data_glm, 
         aes(x= year, 
             y= latency, 
             colour= sex)) + # separated for each diet percentage 
  theme_classic()+ # theme 
  geom_jitter()+
  scale_colour_manual(values = c("hotpink", "deepskyblue"))+ 
  #geom_smooth(method = "lm", se = TRUE, fullrange = TRUE, colour= "#36454F")+ # colour of the lm
  geom_smooth(method="lm",    #add another layer of data representation.
              se=TRUE,
              aes(colour=sex))+ 
  labs(x = "Year",
       y = "Latency")


ggplot(final_data_glm, 
       aes(x= year, 
           y= latency)) + # separated for each diet percentage 
  theme_classic()+ # theme 
  geom_jitter()+
  scale_colour_manual(values = c("hotpink", "deepskyblue"))+ 
  #geom_smooth(method = "lm", se = TRUE, fullrange = TRUE, colour= "#36454F")+ # colour of the lm
  geom_smooth(method="lm",    #add another layer of data representation.
              se=TRUE))+ 
  labs(x = "year",
       y = "Latency")

#_____________________------


#________________________

summary_table3 <- 
  lsmodel2 %>% 
  broom::tidy(conf.int = TRUE) %>% 
  mutate(p.value = scales::pvalue(p.value)) %>% # changes the pvalues <0.001
  rename("Term"="term",
         "Coefficient" = "estimate", # changing the names to be better
         "Standard Error" = "std.error",
         "t" = "statistic",
         "p value" = "p.value",
         "lower.CI" = "conf.low",
         "upper.CI" = "conf.high")%>%
  mutate(across(c(Coefficient: t), round,5)) %>% 
  kbl() %>% 
  kable_styling(latex_options = "hold_position") %>% # to stop the table moving in markdown!!!!
#  row_spec(c(3,5,7), color = 'white', background = 'purple') %>% # the most sig highlighted in colour
  row_spec(c(0), italic = TRUE, align = "c") %>% # titles italic
  kable_styling() # fancy style

# remove effect and group
summary_table4 <-
  remove_column(summary_table3,1)
  remove_column(summary_table4,1)
  
  
  
#plot ----
age_Lat_scatter2 <- # scatter plot also the main plot
    ggplot(final_data_glm, 
           aes(x= ageFinal, 
               y= latency, 
               colour= sex)) +
    scale_color_discrete(name = "sex")+# change legend title
    theme_classic()+ # theme
    theme(legend.position = "top")+# removes the fig legend
  #  geom_smooth(method = "lm", se=FALSE, fullrange =TRUE)+
    geom_jitter()+
  geom_smooth(method="lm",    #add another layer of data representation.
              se=TRUE,
              aes(colour=sex))+ 
    labs(x = "Age",
         y = "Latency",)
  
  
  change_marginal <- 
    final_data_glm %>% 
    ggplot(aes(
      x=latency,
      colour = factor(sex), # colour with the colour of the different diets
      fill= factor(sex), #fill with the colour of the different diets
      alpha = 0.1, # size
      bandwidth = 175))+ # size
    geom_density()+ # density plot
    theme_void()+ # just the density plot no axis 
    coord_flip()+ # flipping the cords
    theme(legend.position = "none")#
  
  
  age_Lat_scatter2+change_marginal # order of the plots

 # violin plot----
   volin_2_used <- # scatter plot also the main plot
    ggplot(final_data_glm, 
           aes(x= ageFinal, 
               y= latency, 
               fill= ageFinal)) +
    geom_violin(aes(colour = ageFinal), position = position_dodge(0.9),alpha =0.3, size=1.5)+
    geom_point(aes(colour=ageFinal),position = position_dodge(0.9))+
    geom_jitter(aes(colour = ageFinal))+ 
    geom_boxplot(width = 0.1, position = position_dodge(0.9))+
    theme_classic()+ # theme
    theme(legend.position = "top")+# removes the fig legend
    scale_fill_manual(values = c("darkorange", "purple"))+
    scale_colour_manual(values = c("darkorange", "purple"))+
    labs(x = "Age",
         y = "Latency")
  
  # test volin 
  only_male <- final_data_glm %>%
    filter(!(sex %in% c("F")))
  
  male <- 
  ggplot(only_male, 
         aes(x= ageFinal, 
             y= latency, 
             fill= ageFinal)) +
    geom_violin(aes(colour = ageFinal), position = position_dodge(0.9),alpha =0.3, size=1.5)+
    geom_point(aes(colour=ageFinal),position = position_dodge(0.9))+
    geom_jitter(aes(colour = ageFinal))+ 
    geom_boxplot(width = 0.1, position = position_dodge(0.9))+
    theme_classic()+ # theme
    theme(legend.position = "top")+# removes the fig legend
    scale_fill_manual(values = c("deepskyblue", "deepskyblue"))+
    scale_colour_manual(values = c("deepskyblue", "deepskyblue"))+
    labs(x = "Age",
         y = "Latency")
         
  only_female <- final_data_glm %>%
    filter(!(sex %in% c("M")))
  
female <- 
  ggplot(only_female, 
         aes(x= ageFinal, 
             y= latency, 
             fill= ageFinal)) +
    geom_violin(aes(colour = ageFinal), position = position_dodge(0.9),alpha =0.3, size=1.5)+
    geom_point(aes(colour=ageFinal),position = position_dodge(0.9))+
    geom_jitter(aes(colour = ageFinal))+ 
    geom_boxplot(width = 0.1, position = position_dodge(0.9))+
    theme_classic()+ # theme
    theme(legend.position = "top")+# removes the fig legend
    scale_fill_manual(values = c("hotpink", "hotpink"))+
    scale_colour_manual(values = c("hotpink", "hotpink"))+
    labs(x = "Age",
         y = "Latency")   

female+male
 
# could do two fliture by sex to show if older males are more bold 

#####FLEDGLING-----

model3<-lmer(number_fledged ~ sex*latency + ageFinal*latency + Count_may + year+ year*Count_may+(1|nestbox)+ (1 | RFID), data = final_data_glm) # added year
summary(model3)
drop1(model3, test = "F")

model3b<-lmer(number_fledged ~ sex*latency + Count_may + year+ year*Count_may+(1|nestbox)+ (1 | RFID), data = final_data_glm) # added year
summary(model3b)
drop1(model3b, test = "F")
# ageFinal*latency removed 

summary_table_sexlat <- 
  model3b %>% 
  broom::tidy(conf.int = TRUE) %>% 
  mutate(p.value = scales::pvalue(p.value)) %>% # changes the pvalues <0.001
  rename("Term"="term",
         "Coefficient" = "estimate", # changing the names to be better
         "Standard Error" = "std.error",
         "t" = "statistic",
         "p value" = "p.value",
         "lower.CI" = "conf.low",
         "upper.CI" = "conf.high")%>%
  mutate(across(c(Coefficient: t), round,5)) %>% 
  kbl() %>% 
  kable_styling(latex_options = "hold_position") %>% # to stop the table moving in markdown!!!!
  #  row_spec(c(3,5,7), color = 'white', background = 'purple') %>% # the most sig highlighted in colour
  row_spec(c(0), italic = TRUE, align = "c") %>% # titles italic
  kable_styling() # fancy style



names(final_data_glm)
#drop non-significant interactions and rerun
#______________________

#sex-lat removed

model4<-lmer(number_fledged ~ sex+latency + ageFinal + Count_may + year+ year*Count_may+(1|nestbox)+ (1 | RFID), data = final_data_glm) # added year
summary(model4)

performance::check_model(model4)

#_____________________

#summary_table5 <- 
  model4 %>% 
  broom::tidy(conf.int = TRUE) %>% 
  mutate(p.value = scales::pvalue(p.value)) %>% # changes the pvalues <0.001
  rename("Term"="term",
         "Coefficient" = "estimate", # changing the names to be better
         "Standard Error" = "std.error",
         "t" = "statistic",
         "p value" = "p.value",
         "lower.CI" = "conf.low",
         "upper.CI" = "conf.high")%>%
  mutate(across(c(Coefficient: t), round,5)) %>% 
  kbl() %>% 
  kable_styling(latex_options = "hold_position") %>% # to stop the table moving in markdown!!!!
  #  row_spec(c(3,5,7), color = 'white', background = 'purple') %>% # the most sig highlighted in colour
  row_spec(c(0), italic = TRUE, align = "c") %>% # titles italic
  kable_styling() # fancy style

# remove effect and group
summary_table8 <-
  remove_column(summary_table5,1)
remove_column(summary_table8,1)

print(summary_table8)


# graph ----

# bold reproductive success----
# lm for male and female 

bold_repro_scatter_used<- 
  ggplot(final_data_glm, 
         aes(x= number_fledged, 
             y= latency, 
             colour= sex)) + # separated for each diet percentage 
  theme_classic()+ # theme 
  geom_jitter()+
  scale_colour_manual(values = c("hotpink", "deepskyblue"))+ 
 #geom_smooth(method = "lm", se = TRUE, fullrange = TRUE, colour= "#36454F")+ # colour of the lm
  geom_smooth(method="lm",    #add another layer of data representation.
              se=TRUE,
              aes(colour=sex))+ 
  labs(x = "Number fledged",
       y = "Latency")

# ONLY FEMALE AND MALE VERSION OF ABOVE -----


bold_repro_scatter_used_female<- 
  ggplot(only_female, 
         aes(x= number_fledged, 
             y= latency, 
             colour= sex)) + # separated for each diet percentage 
  theme_classic()+ # theme 
  geom_jitter()+
  scale_colour_manual(values = c("hotpink"))+ 
  #geom_smooth(method = "lm", se = TRUE, fullrange = TRUE, colour= "#36454F")+ # colour of the lm
  geom_smooth(method="lm",    #add another layer of data representation.
              se=TRUE,
              aes(colour=sex))+ 
  labs(x = "Number fledged",
       y = "Latency")



bold_repro_scatter_used_male<- 
  ggplot(only_male, 
         aes(x= number_fledged, 
             y= latency, 
             colour= sex)) + # separated for each diet percentage 
  theme_classic()+ # theme 
  geom_jitter()+
  scale_colour_manual(values = c("deepskyblue"))+ 
  #geom_smooth(method = "lm", se = TRUE, fullrange = TRUE, colour= "#36454F")+ # colour of the lm
  geom_smooth(method="lm",    #add another layer of data representation.
              se=TRUE,
              aes(colour=sex))+ 
  labs(x = "Number fledged",
       y = "Latency")



bold_repro_scatter_used_male+bold_repro_scatter_used_female








bold_repro_scatter_used_year<- 
  ggplot(final_data_glm, 
         aes(x= number_fledged, 
             y= latency, 
             colour= factor(year))) + # separated for each diet percentage 
  theme_classic()+ # theme 
  geom_jitter()+
 # scale_colour_manual(values = c("hotpink", "deepskyblue"))+ 
  geom_smooth(method = "lm", se = TRUE, fullrange = TRUE, colour= "#36454F")+ # colour of the lm
  labs(x = "Number fledged",
       y = "Latency")



bold_repro_scatter_used_year2<- 
  ggplot(final_data_glm, 
         aes(x= number_fledged, 
             y= latency,
             colour =Count_may))+
  scale_color_gradient(low = "#AF7AC5", high = "#E74C3C", name ="Count May (days)")+ # manal colour change            colour= Count_may)) + # separated for each diet percentage 
  theme_classic()+ # theme 
  geom_jitter()+
  # scale_colour_manual(values = c("hotpink", "deepskyblue"))+ 
  geom_smooth(method = "lm", se = TRUE, fullrange = TRUE, colour= "#36454F")+ # colour of the lm
  labs(x = "Number fledged",
       y = "Latency")


#_________________________________________________-----
# how the year effects the reproductuve sucess ----
model5<-lmer(number_fledged ~ sex*latency + ageFinal + Count_may + year + (1|nestbox)+ (1 | RFID), data = final_data_glm)
summary(model5)

summary_table6 <- 
  model5 %>% 
  broom::tidy(conf.int = TRUE) %>% 
  mutate(p.value = scales::pvalue(p.value)) %>% # changes the pvalues <0.001
  rename("Term"="term",
         "Coefficient" = "estimate", # changing the names to be better
         "Standard Error" = "std.error",
         "t" = "statistic",
         "p value" = "p.value",
         "lower.CI" = "conf.low",
         "upper.CI" = "conf.high")%>%
  mutate(across(c(Coefficient: t), round,5)) %>% 
  kbl() %>% 
  kable_styling(latex_options = "hold_position") %>% # to stop the table moving in markdown!!!!
  #  row_spec(c(3,5,7), color = 'white', background = 'purple') %>% # the most sig highlighted in colour
  row_spec(c(0), italic = TRUE, align = "c") %>% # titles italic
  kable_styling() # fan
# remove effect and group
summary_table7 <-
  remove_column(summary_table6,1) 

  remove_column(summary_table7,1) 

print(summary_table7)


# test doesnt make anything sig ---- 
drop1(model5, test = "F") # seeing weather to drop something - age
model6<-lmer(number_fledged ~ sex*latency + Count_may + year + (1|nestbox)+ (1 | RFID), data = final_data_glm)
summary(model6)

drop1(model6, test = "F") # seeing weather to drop something - sex*latency
model7<-lmer(number_fledged ~ Count_may + year + (1|nestbox)+ (1 | RFID), data = final_data_glm)
summary(model7)

drop1(model7, test = "F")
model8<-lmer(number_fledged ~ year + (1|nestbox)+ (1 | RFID), data = final_data_glm)
summary(model8)

# plot ----









#800
  
# 45 mins 
# reproductive success and laydate, the later breeding in the year the poorer the amount of offspring, so there isnt enough food - not seeing this here so this is different 
# mandingly woods (G) great tit chicks look very good quality and they all seem to be of the size but in cork they is alot of varibltiy in the quality 
#mad good food? not effect on 
# predictions for personalty, better risk and shy on the year (habit and predator), could be the same over the different years, no selection (lit)


# intro, methods and dis - biggest 
# results the least amount of words 
# main concepts, why import 
# cover page 
# formative 9th 8am 
# questions write down and email on Friday the 5th 
# stats phil, concepts of behavour ellen or becky 



