# Prior paper Exp 2b
# Projection experiment with sensitivity to fact
# preprocessing.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(dichromat)
library(forcats)
library(RColorBrewer)
theme_set(theme_bw())

# load raw data
d = read.csv("../data/experiment.csv")
nrow(d) #7800 = 300 participants x 26 items
names(d)
length(unique(d$workerid)) #300 participants

mean(d$Answer.time_in_minutes) #7.1
median(d$Answer.time_in_minutes) #6

d = d %>%
  dplyr::select(workerid,rt,subjectGender,speakerGender,content,verb,fact,fact_type,contentNr,trigger_class,response,slide_number_in_experiment,age,language,assess,american,gender,comments,Answer.time_in_minutes,assignmentid)
nrow(d) #7800

# look at Turkers' comments
unique(d$comments)

# look at whether Turkers thought they understood the task
table(d$assess)

# age and gender info
length(which(is.na(d$age))) #0 missing values
table(d$age) #21-72
median(d$age,na.rm=TRUE) #36
d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
#145 female, 154 male, 1 undeclared

### exclude non-American English speakers
length(unique(d$workerid)) #300
length(which(is.na(d$language))) #no missing responses
table(d$language) 
d <- subset(d, (d$language != "" & d$language != "Russian" & d$language != "Ukrainian" & d$language != "Arabic" & d$language != "chinese" & d$language != "hungarian" & d$language != "spanish"))
d = droplevels(d)
length(unique(d$workerid)) #292 (8 Turkers excluded)

table(d$gender)
length(which(is.na(d$american))) #78 (3 people didn't respond)
table(d$american) 
# coding error in HTML file: m=yes, f=no
d <- subset(d, d$american == "m")
d = droplevels(d)
length(unique(d$workerid)) #277 (15 Turkers excluded, 23 excluded in total for language reasons)

## exclude Turkers based on 6 controls
names(d)
table(d$contentNr)
table(d$verb)

# make control data subset
c <- subset(d, d$verb == "control")
c <- droplevels(c)
nrow(c) #1662 / 6 controls = 277 Turkers

# group mean on controls
round(mean(c$response),2) #.21

ggplot(c, aes(x=workerid,y=response)) +
  geom_point(aes(colour = content),position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  scale_y_continuous(breaks = pretty(c$response, n = 10)) +
  ylab("Responses") +
  xlab("Participant")


# group means on each control
means = aggregate(response ~ contentNr, data=c, FUN="mean")
means
# contentNr  response
# 1  control1 0.1877256
# 2  control2 0.1822744
# 3  control3 0.1749458
# 4  control4 0.2440794
# 5  control5 0.1886282
# 6  control6 0.2560289

# Turkers with response means on controls more than 2sd above group mean
# this is the exclusion criterion we decided on for factivity paper
c.means = aggregate(response~workerid, data=c, FUN="mean")
c.means$YMin = c.means$response - aggregate(response~workerid, data=c, FUN="ci.low")$response
c.means$YMax = c.means$response + aggregate(response~workerid, data=c, FUN="ci.high")$response
c.means

c.g <- c.means[c.means$response > (mean(c.means$response) + 2*sd(c.means$response)),]
c.g 
unique(length(c.g$workerid)) #11 Turkers gave high response
mean(c.g$response) #.69

outliers <- subset(c, c$workerid %in% c.g$workerid)
outliers = droplevels(outliers)

# look at the responses to the controls that these "outlier" Turkers did

ggplot(outliers, aes(x=workerid,y=response)) +
  geom_point(aes(colour = contentNr)) +
  geom_text(aes(label=workerid), vjust = 1, cex= 5,position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=response), vjust = 2.5, cex= 5) +
  scale_y_continuous(breaks = pretty(outliers$response, n = 10)) +
  ylab("Responses") +
  xlab("Participants")


# responses here are supposed to be low but these Turkers
# gave high response to most control items

# exclude the Turker identified above
d <- subset(d, !(d$workerid %in% outliers$workerid))
d <- droplevels(d)
length(unique(d$workerid)) #266 Turkers remain (277 - 11)


# exclude turkers who always clicked on roughly the same point on the scale 
# ie turkers whose variance in overall response distribution is more 
# than 2 sd below mean by-participant variance
names(d)
table(d$verb)

variances = d %>%
  filter(verb != "control") %>%
  group_by(workerid) %>%
  summarize(Variance = var(response)) %>%
  mutate(TooSmall = Variance < mean(Variance) - 2*sd(Variance))
variances

lowvarworkers = as.character(variances[variances$TooSmall,]$workerid)
summary(variances)
lowvarworkers # 0 turker 

lvw = d %>%
  filter(as.character(workerid) %in% lowvarworkers) %>%
  droplevels() %>%
  mutate(Participant = as.factor(as.character(workerid)))

ggplot(lvw,aes(x=Participant,y=response)) +
  geom_point()

# nobody excluded 
#d <- droplevels(subset(d, !(d$workerid %in% lowvarworkers)))
length(unique(d$workerid)) #266 Turkers remain

# age and gender info
length(which(is.na(d$age))) #0 missing values
table(d$age) #21-72
median(d$age,na.rm=TRUE) #36
d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
#129 female, 136 male, 1 undeclared

# clean data
cd = d
write.csv(cd, "../data/cd.csv")
nrow(cd) #6916 / 26 items = 266 participants

