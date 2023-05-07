setwd("//Users/Rachel/Desktop/Perlman et al. 2023 Files")

## Libraries
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)
library(stringr)
library(outliers)
library(lme4)
library(lmerTest)

# Data:

df1 <- read.csv("df.Feed.csv",header=T,sep=",", fill=T)
df2 <- read.csv("df.Move.csv",header=T,sep=",", fill=T)
df3 <- read.csv("df.Cpep.csv",header=T,sep=",", fill=T)
df4 <- read.csv("df.CpepFeed30.csv",header=T,sep=",", fill=T)
df5 <- read.csv("df.FA.FB1.csv",header=T,sep=",", fill=T)
df6 <- read.csv("df.FA.FB2.csv",header=T,sep=",", fill=T)

#### ANALYSES 1. SEASONAL VARIATION IN BELOW-GROUND FEEDING --------------------


# Feeding represented the majority of the gelada diet.
mean(df1$PropFeed)
min(df1$PropFeed)
max(df1$PropFeed)

# Proportion of feeding time spend feeding above-ground and below-ground.
df.FB <- df5 %>% na.omit() %>% group_by(season)

# mean PropFA
mean(df5$PropFA)*100
min(df5$PropFA)*100
max(df5$PropFA)*100

# Below-ground feeding was negatively associated with cumulative rainfall.
model1 <- glmer.nb(FB ~ scale(Rain30) + MinT + offset(log(F.Total)) + (1|ID), data=df5)
summary(model1)

# 4 driest months:
df.dry <- df.FB %>% filter(season == "Dry")
summary(df.dry$PropFB)

# 4 wettest months:
df.wet <- df.FB %>% filter(season == "Wet")
summary(df.wet$PropFB)


#### ANALYSES 2. SEASONAL VARIATION IN FORAGING EFFORT AND ENERGETIC STATUS ----

# Sample Size: Focal data
sum(df1$total.obs)/60
mean(df1$total.obs)/60
sd(df1$total.obs)/60
df2.1 <- df2 %>% group_by(ID) %>% summarise(n=n())
mean(df2.1$n)
sd(df2.1$n)

# Sample Size: Urine samples
df3.1 <- df3 %>% group_by(ID) %>% summarise(n=n())
mean(df3.1$n)
sd(df3.1$n)
df3.1 <- df3 %>% group_by(ID, Month) %>% summarise(n=n())
mean(df3.1$n)
sd(df3.1$n)

# Time spent feeding is negatively associated with rainfall.
model2 <- glmer.nb(sum2 ~ Rain30 + MinT + offset(log(total.obs)) + (1|ID), data=df1)
summary(model2)

# Time spent moving is negatively associated with rainfall (significant).
model3 <- glmer.nb(sum ~ Rain30 + MinT + offset(log(total.obs)) + (1|ID), data=df2)
summary(model3)

# UCP is negatively associated with rainfall (significant).
df3$Time <- strptime(df3$Time, format="%I:%M")
df3$Time <- as.POSIXct(df3$Time)
model4 <- glmer(UCP ~  scale(Time)*scale(Rain30) + MinT + (1|ID), family = Gamma(link = "log"), data=df3)
summary(model4)

# UCP is positively associated with feeding below ground (significant).
df4$Time <- strptime(df4$Time, format="%I:%M")
df4$Time <- as.POSIXct(df4$Time)
model5 <- lmer(UCP ~ PropFB + scale(Time) + MinT + (1 | ID),  data=df4)
summary(model5)
