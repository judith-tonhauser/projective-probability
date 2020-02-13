# Factives paper
# 6-veridicality2-binary (contradictoriness diagnostic, binary)
# preprocessing.R

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
library(brms)
theme_set(theme_bw())

# load raw data
d = read.csv("../data/experiment_noreps.csv")
head(d)
nrow(d) #12040 = 430 participants x 28 items
names(d)
length(unique(d$workerid)) #430 participants

mean(d$Answer.time_in_minutes) #5.79
median(d$Answer.time_in_minutes) #4.93

summary(d)

d = d %>%
  select(workerid,rt,content,subjectGender,speakerGender,verb,contentNr,trigger_class,response,slide_number_in_experiment,age,language,american,gender,comments,Answer.time_in_minutes)
nrow(d) #12040

# recode some names and add nResponse column (numeric variant of yes/no response)
table(d$verb)

d = d %>%
  mutate(verb=recode(verb, control_good = "non-contrd. C", control_bad = "contradictory C", annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform"),
         nResponse = ifelse(response == "Yes",1,0))

head(d)

# look at Turkers' comments
unique(d$comments)


# age and gender info
length(which(is.na(d$age))) #28 missing values (1 Turker)
table(d$age) #18-73
median(d$age,na.rm=TRUE) #36
d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
# 218 female, 212 male

### exclude non-American English speakers
length(unique(d$workerid)) #430
length(which(is.na(d$language))) #no missing responses
table(d$language) 
d$language <- trimws(d$language)
d <- droplevels(subset(d, (d$language != "https://worker.mturk.com/projects/3QKP95RWQZKZ26DSGGU04VYNWZJX5H/tasks/accept_random" & d$language != "polish" & d$language != "Arabic" & d$language != "Korean" & d$language != "" & d$language != "german" & d$language != "Spanish" & d$language != "chinese" & d$language != "1" & d$language != "United States" & d$language != "spanish" & d$language != "West Virginia" & d$language != "Arabic " & d$language != "Tamil" & d$language != "Chinese" & d$language != "Turkish")))
length(unique(d$workerid)) #412 (18 Turkers excluded)

# American English
length(which(is.na(d$american))) #0
table(d$american) 
d <- droplevels(subset(d, d$american == "0"))
length(unique(d$workerid)) #400 (12 Turkers excluded)

# 30 Turkers excluded for language reasons

# ## exclude turkers who completed the experiment too quickly?
# 
# times = d %>%
#   select(Answer.time_in_minutes,workerid) %>%
#   unique() %>%
#   mutate(VerySlow = Answer.time_in_minutes > mean(Answer.time_in_minutes) + 2*sd(Answer.time_in_minutes)) %>%
#   filter(!VerySlow) %>%
#   mutate(TooFast = Answer.time_in_minutes < mean(Answer.time_in_minutes) - 2*sd(Answer.time_in_minutes))
# summary(times)
# table(times$TooFast)
# nobody did the experiment too fast

# exclude participants who did the experiment in under 1 minute
# table(d$Answer.time_in_minutes) 
# d <- droplevels(subset(d, d$Answer.time_in_minutes >= 1))
# length(unique(d$workerid)) # 495 participants (67 Turkers excluded)

min(d$Answer.time_in_minutes) #1.3

## exclude Turkers based on controls
names(d)
table(d$contentNr)
table(d$verb)

# make control data subsets
# control_good = non-contradictory controls ('no' / 0 expected)
# control_bad = contradictory controls ('yes' / 1 expected)

c.bad <- subset(d, d$verb == "contradictory C")
c.bad <- droplevels(c.bad)
head(c.bad)
nrow(c.bad) #1600 / 4 controls = 400 Turkers

c.good <- subset(d, d$verb == "non-contrd. C")
c.good <- droplevels(c.good)
head(c.good)
nrow(c.good) #1600

controlresponses.bad = c.bad %>%
count(workerid, response) %>%
  group_by(workerid) %>%
  filter(response == "Yes")

ggplot(controlresponses.bad, aes(x=n)) +
  geom_histogram()
# most Turkers only said Yes to the 4 contradictory controls

controlresponses.good = c.good %>%
  count(workerid, response) %>%
  group_by(workerid) %>%
  filter(response == "No")

ggplot(controlresponses.good, aes(x=n)) +
  geom_histogram()
# most Turkers only said No to the 4 non-contradictory controls

# remove participants who gave "yes" response to at least one non-contradictory (good) control
# or "No" response to more than one contradictory (bad) control
# outliers_bad: Prop smaller than .74 means that they got more than one wrong
outliers_bad = c.bad %>%
  group_by(workerid) %>%
  summarize(Prop = mean(nResponse)) %>%
  filter(Prop < .74)
outliers_bad
nrow(outliers_bad) #19

outliers_good = c.good %>%
  group_by(workerid) %>%
  summarize(Prop = mean(nResponse)) %>%
  filter(Prop != 0)
outliers_good 
nrow(outliers_good) #75

d <- droplevels(subset(d, !(d$workerid %in% outliers_bad$workerid) | !(d$workerid %in% outliers_good$workerid)))
length(unique(d$workerid)) #386 Turkers (400-386 = 14 excluded)

# clean data
cd = d
write.csv(cd, "../data/cd.csv")
nrow(cd) #10808 / 28 items = 386 participants

# age info
table(cd$age) #18-73
length(which(is.na(cd$age))) # 28 missing values (1 Turker)
median(cd$age,na.rm=TRUE) #37
cd %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
# 193 female 193 male

# load clean data for analysis ----
cd = read.csv("../data/cd.csv")
nrow(cd) #8456 (302 Turkers)
summary(cd)

