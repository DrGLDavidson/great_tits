library(ggplot2)

ggplot(df, aes(x = time, y = value)) +
  stat_summary(aes(group = species), fun.y = mean, geom = "path") +
  stat_summary(aes(color = species), fun.data = mean_cl_boot, geom = "errorbar", width = 0.1) +
  stat_summary(aes(color = species), fun.y = mean, geom = "point", size = 4) +
  geom_point(aes(color = species), size = 2)

