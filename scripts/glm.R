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
  dplyr::select(-sd__Observation) %>%
  kable_styling(latex_options = "hold_position") %>% # to stop the table moving in markdown!!!!
#row_spec(c(3,5,7), color = 'white', background = 'purple') %>% # the most sig highlighted in colour
  row_spec(c(0), italic = TRUE, align = "c") %>% # titles italic
  kable_styling() # fancy style

# remove effect and group
summary_table2 <-
  remove_column(summary_table1,1) %>% 
  remove_column(summary_table2, 1) %>% 
  

# remove rows by cropping 


#____________----

#interaction is not significant and can be removed
lsmodel2<-lmer(latency ~  sex+ageFinal+ Count_may+(1|nestbox)+ (1 | RFID), data = final_data_glm)  ##need to add fledglings
summary(lsmodel2)

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
  
  
#plot ---

  
  lat_age_scatter <-  # naming it
    ggplot(final_data_glm, # using the filter cricket data set
           aes(x= ageFinal,
               y= latency,#
               colour=se))+
geom_jitter()+
scale_color_gradient(low = "#AF7AC5", high = "#E74C3C", name ="b")+ # manal colour change 
    geom_smooth (method = "lm", se = TRUE, fullrange = TRUE, colour="black")+# colour of the regression line
    theme_classic()+
    labs(x = "Age", # labs names
         y = "Latency (seconds)")
  

age_Lat_scatter2 <- # scatter plot also the main plot
    ggplot(final_data_glm, 
           aes(x= ageFinal, 
               y= latency, 
               colour= factor(sex))) +
    scale_color_discrete(name = "sex")+# change legend title
    theme_classic()+ # theme
    theme(legend.position = "top")+# removes the fig legend
    geom_smooth(method = "lm", se=FALSE, fullrange =TRUE)+
    geom_jitter()+
    labs(x = "Age",
         y = "Latency")
  
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

 # violin plot
  
  age_Lat_scatter2 <- # scatter plot also the main plot
    ggplot(final_data_glm, 
           aes(x= ageFinal, 
               y= latency, 
               fill= sex)) +
    geom_violin(aes(colour = sex), position = position_dodge(0.9),alpha =0.3, size=1)+
    geom_point(aes(colour=sex),position = position_dodge(0.9))+
    geom_jitter(aes(colour = sex))+ 
    geom_boxplot(width = 0.1, trim = FALSE, position = position_dodge(0.9))+
    scale_color_discrete(name = "sex")+# change legend title
    theme_classic()+ # theme
    theme(legend.position = "top")+# removes the fig legend
    scale_fill_manual(values = c("hotpink", "blue"))+
    scale_colour_manual(values = c("hotpink", "blue"))+
    labs(x = "Age",
         y = "Latency")

  
  
  
  
  
  
#####FLEDGLING

model3<-lmer(number_fledged ~ sex*latency + ageFinal*latency + Count_may +(1|nestbox)+ (1 | RFID), data = final_data_glm)
summary(model3)
drop1(model3, test = "F")

model3b<-lmer(number_fledged ~ sex*latency + ageFinal + Count_may +(1|nestbox)+ (1 | RFID), data = final_data_glm)
summary(model3b)
drop1(model3, test = "F")
# ageFinal*latency removed 


model4<-lmer(number_fledged ~ sex+latency + ageFinal + Count_may +(1|nestbox)+ (1 | RFID), data = final_data_glm)
summary(model4)
performance::check_model(model4)
# different to what other studues have found

names(final_data_glm)
#drop non-significant interactions and rerun


summary_table5 <- 
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










#_________________________________________________-----
# how the year effects the reproductuve sucess 
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



