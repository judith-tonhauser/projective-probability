# Factives paper
# 5-projectivity-no-fact (certainty ratings, continuous task)
# preprocessing.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(tidybayes)
library(dplyr)
library(dichromat)
library(forcats)
library(ggrepel)
library(brms)
library(knitr)
theme_set(theme_bw())

# load raw data
d = read.csv("../experiment.csv")
nrow(d) #7800 = 300 participants x 26 items
names(d)
length(unique(d$workerid)) #300 participants

mean(d$Answer.time_in_minutes) #5.91
median(d$Answer.time_in_minutes) #5.17

d = d %>%
  select(workerid,rt,subjectGender,speakerGender,content,verb,contentNr,trigger_class,response,slide_number_in_experiment,age,language,assess,american,gender,comments,Answer.time_in_minutes)
nrow(d) #7800

# look at Turkers' comments
unique(d$comments)

# look at whether Turkers thought they understood the task
table(d$assess)

# age and gender info
length(which(is.na(d$age))) #52 missing values (2 Turkers)
table(d$age) #20-71
median(d$age,na.rm=TRUE) #36
d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
#136 female, 157 male, 2 other, 5 undeclared

### exclude non-American English speakers
length(unique(d$workerid)) #300
length(which(is.na(d$language))) #no missing responses
table(d$language) 
d <- droplevels(subset(d, (d$language != "" & d$language != "Gujarati" & d$language != "spanish" & d$language != "1" & d$language != "Chinese" & d$language != "Russian" & d$language != "Spanish")))
length(unique(d$workerid)) #292 (8 Turkers excluded)

# American English
length(which(is.na(d$american))) #26 (1 person didn't respond)
table(d$american) # 4 declared non-American English
d <- droplevels(subset(d, d$american == "y"))
length(unique(d$workerid)) #287 (5 Turkers excluded, 13 excluded in total for language reasons)

## exclude turkers who completed the experiment too quickly??
# times = d %>% 
#   select(Answer.time_in_minutes,workerid) %>% 
#   unique() %>%
#   mutate(VerySlow = Answer.time_in_minutes > mean(Answer.time_in_minutes) + 2*sd(Answer.time_in_minutes)) %>%
#   filter(!VerySlow) %>%
#   mutate(TooFast = Answer.time_in_minutes < mean(Answer.time_in_minutes) - 2*sd(Answer.time_in_minutes))
# summary(times)

## exclude Turkers based on non-projecting controls
names(d)
table(d$contentNr)
table(d$verb)

# make control data subset
c <- subset(d, d$verb == "control")
c <- droplevels(c)
nrow(c) #1722 / 6 controls = 287 Turkers

# group mean on controls
round(mean(c$response),2) #.14

ggplot(c, aes(x=workerid,y=response)) +
  geom_point(aes(colour = content),position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  scale_y_continuous(breaks = pretty(c$response, n = 10)) +
  ylab("Responses") +
  xlab("Participant")
ggsave(f="../graphs/raw-responses-to-controls.pdf",height=4,width=6.5)

# group means on individual controls
means = aggregate(response ~ contentNr, data=c, FUN="mean")
means
# contentNr response
# 1  control1    0.127
# 2  control2    0.149
# 3  control3    0.129
# 4  control4    0.123
# 5  control5    0.168
# 6  control6    0.169

# Turkers with response means on controls more than 2sd above group mean
c.means = aggregate(response~workerid, data=c, FUN="mean")
c.means$YMin = c.means$response - aggregate(response~workerid, data=c, FUN="ci.low")$response
c.means$YMax = c.means$response + aggregate(response~workerid, data=c, FUN="ci.high")$response
c.means

c.g <- c.means[c.means$response > (mean(c.means$response) + 2*sd(c.means$response)),]
c.g
unique(length(c.g$workerid)) #16 Turkers gave high responses
mean(c.g$response) #.67
min(c.g$response)

# how many unique Turkers did badly on the controls?
outliers <- droplevels(subset(c, c$workerid %in% c.g$workerid))
nrow(outliers) #96 / 6 control items = 16 Turkers
outliers[,c("workerid","response")]

# look at the responses to the controls that these "outlier" Turkers did
ggplot(outliers, aes(x=workerid,y=response)) +
  geom_point(aes(colour = contentNr)) +
  geom_text(aes(label=workerid), vjust = 1, cex= 5,position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=response), vjust = 2.5, cex= 5) +
  scale_y_continuous(breaks = pretty(outliers$response, n = 10)) +
  ylab("Responses") +
  xlab("Participants")
ggsave(f="../graphs/raw-responses-to-controls-by-outliers.pdf",height=6,width=10)

# mean response by outliers
o.means = aggregate(response~workerid, data=outliers, FUN="mean")
o.means$YMin = o.means$response - aggregate(response~workerid, data=outliers, FUN="ci.low")$response
o.means$YMax = o.means$response + aggregate(response~workerid, data=outliers, FUN="ci.high")$response
o.means

# responses here are supposed to be low but these Turkers
# gave consistently high responses across the control items

# exclude the 16 Turkers identified above
d <- droplevels(subset(d, !(d$workerid %in% outliers$workerid)))
length(unique(d$workerid)) #271 Turkers remain (287 - 16)

# exclude turkers who always clicked on roughly the same point on the scale 
# ie turkers whose variance in overall response distribution is more 
# than 2 sd below mean by-participant variance
variances = d %>%
  filter(verb != "control") %>%
  group_by(workerid) %>%
  summarize(Variance = var(response)) %>%
  mutate(TooSmall = Variance < mean(Variance) - 2*sd(Variance))

lowvarworkers = as.character(variances[variances$TooSmall,]$workerid)
summary(variances)
lowvarworkers # 5 turkers consistently clicked on roughly the same point on the scale

lvw = d %>%
  filter(as.character(workerid) %in% lowvarworkers) %>%
  droplevels() %>%
  mutate(Participant = as.factor(as.character(workerid)))

ggplot(lvw,aes(x=Participant,y=response)) +
  geom_point()

# exclude the 5 Turkers identified above
d <- droplevels(subset(d, !(d$workerid %in% lowvarworkers)))
length(unique(d$workerid)) #266 Turkers remain

# exclude workers with factive mean smaller than or equal to control mean
# fcmeans = d %>%
#   filter(verb %in% c("control","be_annoyed","see","discover","reveal","know")) %>%
#   mutate(ItemType = ifelse(verb == "control","control","factive")) %>%
#   group_by(workerid,ItemType) %>%
#   summarize(Mean = mean(response)) %>%
#   spread(ItemType,Mean) %>%
#   mutate(FCDiff = factive - control)
# 
# negfcdiffworkers = fcmeans[fcmeans$FCDiff <= 0,]$workerid
# length(negfcdiffworkers) # 2 turkers

# exclude the 2 Turkers identified above
# d <- droplevels(subset(d, !(d$workerid %in% negfcdiffworkers)))
# length(unique(d$workerid)) #264 Turkers remain


# posfcdiffs = fcmeans %>%
#   filter(FCDiff > 0)
# 
# SD = sd(posfcdiffs$FCDiff)
# Mean = mean(posfcdiffs$FCDiff)
# Threshold = .1#Mean - 2.5*SD
# 
# posfcdiffs = posfcdiffs %>%
#   mutate(SmallDiff = FCDiff < Threshold) 
# summary(posfcdiffs)
# 
# smallfcdiffworkers = posfcdiffs[posfcdiffs$SmallDiff,]$workerid

# resulting group mean on controls
c <- droplevels(subset(d, d$verb == "control"))
round(mean(c$response),2) #.11

# change cd verb names to match veridicality names
d = d %>%
  mutate(verb=recode(verb, control = "MC", annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform"))

# save clean data
cd = d
write.csv(cd, "../data/cd.csv")
nrow(cd) #6916 / 26 items = 266 participants

# age and gender of remaining participants
table(cd$age) #20-71
length(which(is.na(cd$age))) # 0 missing values
median(cd$age,na.rm=TRUE) #36

cd %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
#118 female, 143 male, 2 other, 3 undeclared
