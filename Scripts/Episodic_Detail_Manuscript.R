#Analyses for Episodic Detail Manuscript 
library(dplyr)
library(tidyverse)
library(tidyr)
library(psych)
library(datawizard)



setwd("~/Desktop/Episodic Details Manuscript/")
data <- readRDS("data_long.rds")
#find the ID's that got lost in data wrangling
setwd("~/Desktop/Past/MSc Thesis/Thesis")
msc <- load("msc.data.R")
a <- data$ID

!(msc$ID %in% data$ID)
View(msc.data %>% filter(!(msc.data$ID %in% a)) %>% select())


####################### DESCRIPTIVES ####################### 
#DERS -> skewed
skewness(df$DERS)
hist(df$DERS)

#Avo -> skewed
skewness(df$Avo)
hist(df$Avo)

#Distribution of Emotional Details --> positively skewed
skewness(df$Det_Neg, na.rm=T)
hist(df$Det_Neg)
skewness(df$Det_Pos, na.rm=T)
hist(df$Det_Pos)

#Distribution of Vividness --> normal
skewness(df$Viv_Neg)
hist(df$Viv_Neg)
skewness(df$Viv_Pos)
hist(df$Viv_Pos)

#is allo less than 10%? No
hist(df$VP_Neg)
hist(df$VP_Pos)
df$VP_Neg <- as.factor(df$VP_Neg)
df$VP_Pos <- as.factor(df$VP_Pos)
table(df$VP_Neg) #24%
table(df$VP_Pos) #24%


