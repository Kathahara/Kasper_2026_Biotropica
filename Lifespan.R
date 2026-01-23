# Lifespan of Sunda clouded leopards

# Journal: Biotropica

# R script author: Katharina Kasper

# 2026-01-23


# Necessary file: ind_table.csv (records of individuals)


# R packages 

library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(magrittr)


# read data

df <- read.csv("ind_table.csv")




## plot minimum residence timelines


# format

df$recaptures <- as.numeric(df$recaptures)
df$captures = as.numeric(df$captures)
df$delta_time = as.numeric(df$delta_time)
df$time_from <- mdy(df$time_from) # dates
df$time_to <- mdy(df$time_to)
df <- df[order(df$time_from),] # order by increasing time span
df <- df %>% filter(delta_time > 0) # remove zero timelines


# plot

ggplot(df, aes(x = time_from, xend = time_to, y = reorder(name, time_from), yend = name, color = sex)) +
  geom_segment(size = 2.5) + 
  theme_bw() + 
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_blank()) +
  scale_color_manual(values = c("male" = "black", "female" = "grey"))




## assess sex-specific bias in detection and lifespan estimation 


# format

df$sex <- as.factor(df$sex)
# extra dataset excluding transients (below one week delta time)
df7 <- df %>% filter(as.numeric(delta_time) > 7) 


# Mann-Whitney U test

lifespan <- wilcox.test(delta_time ~ sex, data = df, exact = FALSE)
detection <- wilcox.test(captures ~ sex, data = df, exact = FALSE)
lifespan
detection
qnorm(lifespan$p.value/2) # Z value
qnorm(detection$p.value/2)

lifespan7 <- wilcox.test(delta_time ~ sex, data = df7, exact = FALSE)
detection7 <- wilcox.test(captures ~ sex, data = df7, exact = FALSE)
lifespan7
detection7
qnorm(lifespan7$p.value/2) # Z value
qnorm(detection7$p.value/2)


# Spearman’s rank correlation test

cor.test(df$delta_time, df$captures, method = "spearman", exact = FALSE)

cor.test(df7$delta_time, df7$captures, method = "spearman", exact = FALSE)



## plot against sex

# minimum residence times

df %>%  ggplot(aes(x=sex, y=delta_time)) +
  geom_violin(width=1,  color = "#dbd7d2", fill="#dbd7d2") +
  geom_boxplot(width=0.1, fill = "#dbd7d2", lwd = 0.5) + 
  scale_x_discrete(limits = c("Male", "Female")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

df7 %>%  ggplot(aes(x=sex, y=delta_time)) +
  geom_violin(width=1,  color = "#dbd7d2", fill="#dbd7d2") +
  geom_boxplot(width=0.1, fill = "#dbd7d2", lwd = 0.5) + 
  scale_x_discrete(limits = c("Male", "Female")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



## record plots

df %>%  ggplot(aes(x=sex, y=captures)) +
  geom_violin(width=1,  color = "#dbd7d2", fill="#dbd7d2") +
  geom_boxplot(width=0.1, fill = "#dbd7d2", lwd = 0.5) + 
  scale_x_discrete(limits = c("male", "female")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

df7 %>%  ggplot(aes(x=sex, y=captures)) +
  geom_violin(width=1,  color = "#dbd7d2", fill="#dbd7d2") +
  geom_boxplot(width=0.1, fill = "#dbd7d2", lwd = 0.5) + 
  scale_x_discrete(limits = c("male", "female")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())





