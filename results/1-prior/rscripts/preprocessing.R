# Prior paper
# Exp 2a (prior measured separately)
# establish prior probabilities for contents given one of two facts
# preprocessing.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(forcats)
library(dichromat)
theme_set(theme_bw())

# load raw data
d = read.csv("../data/experiment.csv")
nrow(d) #2090 (95 participants x 22 items)
names(d)
length(unique(d$workerid)) #95 participants

mean(d$Answer.time_in_minutes) #3.9
median(d$Answer.time_in_minutes) #3.3


d = d %>%
  dplyr::select(workerid,rt,prompt,itemType,itemNr,list,item,response,fact,slide_number_in_experiment,gender,american,age,language,comments,Answer.time_in_minutes)
nrow(d) #2090

# look at Turkers' comments
unique(d$comments)

# age and gender info
table(d$age) #21-75
median(d$age) #33
d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
#45 female, 50 male

### exclude non-American English speakers
length(unique(d$workerid))
length(which(is.na(d$language))) #no missing responses
table(d$language) 
d <- subset(d, (d$language != "hindi" & d$language != "female" & d$language != "russian"))
d = droplevels(d)
length(unique(d$workerid)) #92

length(which(is.na(d$american))) #0 (everybody responded)
table(d$american) 
d <- subset(d, d$american == "0")
d = droplevels(d)
length(unique(d$workerid)) #87 

## 95-87 = 8 participants excluded due to language reasons

## how often was each list completed?
table(d$list)

## exclude Turkers based on the two control stimuli
table(d$item)

# make relevant subsets
# filler/control 1 (high responses expected)
d.f1 <- subset(d, d$item == "F1")
d.f1 <- droplevels(d.f1)
nrow(d.f1) #87

# filler/control 2 (low responses expected)
d.f2 <- subset(d, d$item == "F2")
d.f2 <- droplevels(d.f2)
nrow(d.f2) #87

# data on both controls/fillers
d.f12 <- rbind(d.f1,d.f2)
nrow(d.f12) #174

# group mean on filler 1
round(mean(d.f1$response),2) #.86

# group mean on filler 2
round(mean(d.f2$response),2) #.03

ggplot(d.f12, aes(x=workerid,y=response)) +
  geom_point(aes(color = item)) +
  geom_text(aes(label=workerid), vjust = 1, cex= 5) +
  scale_y_continuous(breaks = pretty(d.f12$response, n = 10)) +
  ylab("Responses to fillers") +
  xlab("Participant")

# Turkers with mean response to filler 1 that is more than 2sd below group mean
# this is the exclusion criterion we decided on for the factivity paper
c.means = aggregate(response~workerid, data=d.f1, FUN="mean")
c.means$YMin = c.means$response - aggregate(response~workerid, data=d.f1, FUN="ci.low")$response
c.means$YMax = c.means$response + aggregate(response~workerid, data=d.f1, FUN="ci.high")$response
c.means

c.f1 <- c.means[c.means$response < (mean(c.means$response) - 2*sd(c.means$response)),]
c.f1
unique(length(c.f1$workerid)) #9 Turkers
mean(c.f1$response)

# Turkers with mean response to filler 2 that is more than 2sd above group mean
# this is the exclusion criterion we decided on for the factivity paper
c.means = aggregate(response~workerid, data=d.f2, FUN="mean")
c.means$YMin = c.means$response - aggregate(response~workerid, data=d.f2, FUN="ci.low")$response
c.means$YMax = c.means$response + aggregate(response~workerid, data=d.f2, FUN="ci.high")$response
c.means

c.f2 <- c.means[c.means$response > (mean(c.means$response) + 2*sd(c.means$response)),]
c.f2
unique(length(c.f2$workerid)) #3 Turkers
mean(c.f2$response)

# exclude the Turkers identified above
d <- subset(d, !(d$workerid %in% c.f1$workerid | d$workerid %in% c.f2$workerid))
d <- droplevels(d)
length(unique(d$workerid)) #75 Turkers remain

## 87 - 75 = 12 Turkers excluded due to controls

filler <- droplevels(subset(d,d$itemType == "F"))
nrow(filler)

ggplot(filler, aes(x=workerid,y=response)) +
  geom_point(aes(colour = item)) +
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  scale_y_continuous(breaks = pretty(filler$response, n = 10)) +
  ylab("Responses to controls") +
  xlab("'Good' Participant")

# exclude turkers who always clicked on roughly the same point on the scale 
# ie turkers whose variance in overall response distribution is more 
# than 2 sd below mean by-participant variance
names(d)
table(d$itemType)

variances = d %>%
  filter(itemType != "F") %>%
  group_by(workerid) %>%
  summarize(Variance = var(response)) %>%
  mutate(TooSmall = Variance < mean(Variance) - 2*sd(Variance))
variances

lowvarworkers = as.character(variances[variances$TooSmall,]$workerid)
summary(variances)
lowvarworkers # 1 turker 

lvw = d %>%
  filter(as.character(workerid) %in% lowvarworkers) %>%
  droplevels() %>%
  mutate(Participant = as.factor(as.character(workerid)))

ggplot(lvw,aes(x=Participant,y=response)) +
  geom_point()
# Turker 44 used the scale, just not the very high end

# nobody excluded 
#d <- droplevels(subset(d, !(d$workerid %in% lowvarworkers)))
length(unique(d$workerid)) #75 Turkers remain

# age and gender info
table(d$age) #21-75
median(d$age) #35
d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
#34 female, 41 male

# clean data = cd
cd = d
write.csv(cd, file = "../data/cd.csv")
head(cd)
nrow(cd) #1650 / 22 items = 75 participants

