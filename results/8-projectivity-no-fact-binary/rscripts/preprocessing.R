# Factives paper
# 8-projectivity-no-fact-binary (certainty ratings, binary task)
# preprocessing.R
# (Exp 1b in Language paper)

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(dplyr)
library(dichromat)
library(forcats)
library(ggrepel)
theme_set(theme_bw())

# load raw data
d = read.csv("../data/experiment_noreps.csv")
head(d)
nrow(d) #13650 = 525 participants x 26 items
names(d)
length(unique(d$workerid)) #525 participants

summary(d$Answer.time_in_minutes)

ggplot(d, aes(x=Answer.time_in_minutes)) +
  geom_histogram()

summary(d)

mean(d$Answer.time_in_minutes) 
median(d$Answer.time_in_minutes)

d = d %>%
  select(workerid,rt,content,subjectGender,speakerGender,verb,utterance,contentNr,trigger_class,response,slide_number_in_experiment,age,language,assess,american,gender,comments,Answer.time_in_minutes)
nrow(d) #13650

# recode some names and add nResponse column (numeric variant of yes/no response)
d = d %>%
  mutate(verb=recode(verb, control = "MC", annoyed = "be annoyed", be_right_that = "be right", inform_Sam = "inform"),
         nResponse = ifelse(response == "Yes",1,0))

# look at Turkers' comments
unique(d$comments)

# look at whether Turkers thought they understood the task
table(d$assess)

# age and gender info
length(which(is.na(d$age))) #78 missing values (3 Turkers)
table(d$age) #18-81 (plus two nonsense values)
median(d$age,na.rm=TRUE) #37 (nonsense values not excluded)
table(d$gender)
#272 female, 318 male, 2 undeclared

# plot ages
ages = unique(d[,c("workerid","age"),])
ggplot(ages %>% filter(age < 130),aes(x=age)) +
  geom_histogram()

### exclude non-American English speakers
length(unique(d$workerid)) #525
length(which(is.na(d$language))) #no missing responses
table(d$language) 
d <- droplevels(subset(d, (d$language != "United States" & d$language != "African" & d$language != "" & d$language != "Spanish" & d$language != "0" & d$language != "Arabic" & d$language != "1" & d$language != "British English" & d$language != "german" & d$language != "Vietnamese" & d$language != "4" & d$language != "chinese" & d$language != "Italian" & d$language != "Korean")))
length(unique(d$workerid)) #506 (19 Turkers excluded)

# American English
length(which(is.na(d$american))) #0 (0 Turkers didn't respond)
table(d$american) #624 = 26 x 24 did not declare American English
d <- droplevels(subset(d, d$american == 0))
length(unique(d$workerid)) #482 (24 Turkers excluded)

# 43 Turkers excluded for not self-declared speakers of American English

## exclude turkers who completed the experiment too quickly?

times = d %>%
  select(Answer.time_in_minutes,workerid) %>%
  unique() %>%
  mutate(VerySlow = Answer.time_in_minutes > mean(Answer.time_in_minutes) + 2*sd(Answer.time_in_minutes)) %>%
  filter(!VerySlow) %>%
  mutate(TooFast = Answer.time_in_minutes < mean(Answer.time_in_minutes) - 2*sd(Answer.time_in_minutes))
summary(times)
# nobody excluded

# exclude participants who did the experiment in under 1 minute
# table(d$Answer.time_in_minutes) 
# unique(d[d$Answer.time_in_minutes < 1.5,c("workerid","Answer.time_in_minutes")])
# d <- droplevels(subset(d, d$Answer.time_in_minutes >= 1))
# length(unique(d$workerid)) # 530 participants (11 Turkers excluded)

min(d$Answer.time_in_minutes) #.99
# it does not seem reasonable to exclude participants for doing the experiment in under 1 minute
# because clicking through takes about 40 seconds

# nobody excluded for speed of doing experiment

## exclude Turkers based on non-projecting controls
names(d)
table(d$contentNr)
table(d$verb)

# make control data subset
c <- subset(d, d$verb == "MC")
c <- droplevels(c)
head(c)
nrow(c) #2892 / 6 controls = 482 Turkers

# proportion of "no" responses to controls
c %>%
  count(response) %>%
  mutate(prop =  n / sum(n)) %>%
  filter(response == "No")

# proportion of "no" responses to controls by participant
controlresponses = c %>%
  count(workerid, response) %>%
  group_by(workerid) %>%
  filter(response == "No")

ggplot(controlresponses, aes(x=n)) +
  geom_histogram()

table(controlresponses$n)
#  1   2   3   4   5   6 
# 10  11   5   6  10 426
# most Turkers got all 6 controls right (= no)

# identify outlier Turkers, defined as Turkers who responded "yes/1" to more than one of the 6 controls
# i.e., whose Prop is larger than .1666 (which is the Proportion for getting one control wrong)
outlier_Turkers = c %>%
  group_by(workerid) %>%
  summarize(Prop = mean(nResponse)) %>%
  filter(Prop > 0.17)
outlier_Turkers

# remove participants who gave "yes/1" response to at least one control
d <- droplevels(subset(d, !(d$workerid %in% outlier_Turkers$workerid)))
length(unique(d$workerid)) #436 Turkers (482-436 = 46 excluded)

# exclude turkers who always clicked "No"

# noclickers = d %>%
#   filter(verb != "MC") %>%
#   group_by(workerid) %>%
#   summarize(Proportion=mean(nResponse))
# 
# ggplot(noclickers, aes(x=Proportion)) +
#   geom_histogram()

# # people who always said No:
# noclickers %>%
#   filter(Proportion == 0)
# 
# d[d$workerid == 495,c("verb","response")]
# 
# 

# exclude workers with factive mean smaller than or equal to control mean
# given that control mean is 0, this means that they only responded "no/0" to factives

# fcmeans = d %>%
#   filter(verb %in% c("MC","be_annoyed","see","discover","reveal","know")) %>%
#   mutate(ItemType = ifelse(verb == "MC","MC","factive")) %>%
#   group_by(workerid,ItemType) %>%
#   summarize(Mean = mean(nResponse)) %>%
#   spread(ItemType,Mean) %>%
#   mutate(FCDiff = factive - MC)
# fcmeans
# 
# negfcdiffworkers = fcmeans[fcmeans$FCDiff <= 0,]$workerid
# length(negfcdiffworkers) # 10 turkers
# 
# # exclude the 10 Turkers identified above
# d <- droplevels(subset(d, !(d$workerid %in% negfcdiffworkers)))
# length(unique(d$workerid)) #471 Turkers remain (482-11 = 471)
# table(d$nResponse)

# clean data
cd = d
write.csv(cd, "../data/cd.csv")
nrow(cd) #11336 / 26 items = 436 participants

# age info
table(cd$age) #18-81
length(which(is.na(cd$age))) # 26 missing values = 1 Turker
median(cd$age,na.rm=TRUE) #37
cd %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
#207 female, 223 male, 2 other, 4 NA

