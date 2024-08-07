library(ggplot2)
library(ggExtra)
#____________________________----
# straight ----
time <- c("+","+","+","+","+","+","-","-","-","-","-","-")
species <- c(1,1,1,2,2,2,1,1,1,2,2,2)
Phenotype <- c(1,2,3,11,12,13,1,2,3,11,12,13)
        

df <- data.frame(time, species,Phenotype)
df$time <- as.factor(df$time)
df$species <- as.factor(df$species)

straight <-
  ggplot(df, aes(x = time, y = Phenotype, colour = species)) +
  theme_classic()+
  labs(x = "", 
       y = "Phenotype")+# labs names
  theme(legend.position="none", text = element_text(size = 16))+
  stat_summary(aes(group = species), fun.y = mean, geom = "path") +
  stat_summary(aes(color = species), fun.data = mean_cl_boot, geom = "errorbar", width = 0.1) +
  stat_summary(aes(color = species), fun.y = mean, geom = "point", size = 4) +
  geom_point(aes(color = species), size = 2)

#___________________________________----
#levels ----
time <- c("+","+","+","+","+","+","-","-","-","-","-","-")
species <- c(1,1,1,2,2,2,1,1,1,2,2,2)
Phenotype <- c(1,2,3,4,5,6,11,12,13,14,15,16)


df <- data.frame(time, species,Phenotype)
df$time <- as.factor(df$time)
df$species <- as.factor(df$species)

levels <-
  ggplot(df, aes(x = time, y = Phenotype, colour = species)) +
  theme_classic()+
  theme(legend.position="none", text = element_text(size = 16))+
  labs(y = "", 
       x = "Centred environmental gradient")+# labs names
  stat_summary(aes(group = species), fun.y = mean, geom = "path") +
  stat_summary(aes(color = species), fun.data = mean_cl_boot, geom = "errorbar", width = 0.1) +
  stat_summary(aes(color = species), fun.y = mean, geom = "point", size = 4) +
  geom_point(aes(color = species), size = 2)
#_________________________________-----
#cross ----
time <- c("+","+","+","+","+","+","-","-","-","-","-","-")
species <- c(1,1,1,2,2,2,1,1,1,2,2,2)
Phenotype <- c(1,2,3,13,14,15,13,14,15,1,2,3)

df <- data.frame(time, species,Phenotype)
df$time <- as.factor(df$time)
df$species <- as.factor(df$species)

cross <-
ggplot(df, aes(x = time, y = Phenotype, colour = species)) +
  theme_classic()+
  labs(x = "", 
       y = "")+# labs names
  stat_summary(aes(group = species), fun.y = mean, geom = "path") +
  stat_summary(aes(color = species), fun.data = mean_cl_boot, geom = "errorbar", width = 0.1) +
  stat_summary(aes(color = species), fun.y = mean, geom = "point", size = 4) +
  geom_point(aes(color = species), size = 2)


cross <-
ggplot(df, aes(x = time, y = value, colour = species)) +
  theme_classic()+
  theme(legend.position="none", text = element_text(size = 16))+
  labs(y = "", 
       x = "Centred environmental gradient")+# labs names
  stat_summary(aes(group = species), fun.y = mean, geom = "path") +
  stat_summary(aes(color = species), fun.data = mean_cl_boot, geom = "errorbar", width = 0.1) +
  stat_summary(aes(color = species), fun.y = mean, geom = "point", size = 4) +
  geom_point(aes(color = species), size = 2)
#_________________________________-----
#_____________----
# making the plot coming together ---- 

straight+levels+cross

test
