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
  mutate(verb=recode(verb, control_good = "noncontrd. C", control_bad = "contradictory C", annoyed = "be annoyed", be_right_that = "be right", inform_Sam = "inform"),
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

c.good <- subset(d, d$verb == "noncontrd. C")
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

# exclusion criterion: remove participants who got more than one answer wrong on the 8 controls
# outliers_bad: Prop 1 means all correct, Prop smaller than .75 means that they got more than 1 wrong
# outliers_good: Prop 0 means all correct, Prop larger than .25 means that they got more than 1 wrong
outliers_bad.2plus = c.bad %>%
  group_by(workerid) %>%
  summarize(Prop = mean(nResponse)) %>%
  filter(Prop < .75)
outliers_bad.2plus
nrow(outliers_bad.2plus) #19 (participant who got more than one wrong; remaining have 0 or 1 wrong)

ggplot(outliers_bad.2plus, aes(x=Prop)) +
  geom_histogram()
#4 people got 4 wrong, 3 people got 3 wrong, 12 people got 2 wrong

outliers_bad.1 = c.bad %>%
  group_by(workerid) %>%
  summarize(Prop = mean(nResponse)) %>%
  filter(Prop == .75)
outliers_bad.1
nrow(outliers_bad.1) #37 (participants who got exactly one wrong)

outliers_good.2plus = c.good %>%
  group_by(workerid) %>%
  summarize(Prop = mean(nResponse)) %>%
  filter(Prop > .25)
outliers_good.2plus 
nrow(outliers_good.2plus) #26 (participant who got more than one wrong; remaining have 0 or 1 wrong)

ggplot(outliers_good.2plus, aes(x=Prop)) +
  geom_histogram()
# 9 people got 4 wrong, 11 people got 3 wrong, 6 people got 2 wrong

outliers_good.1 = c.good %>%
  group_by(workerid) %>%
  summarize(Prop = mean(nResponse)) %>%
  filter(Prop == .25)
outliers_good.1 
nrow(outliers_good.1) #49 (participants who got exactly one wrong)

# remove 19+26=45 participants who are in either outliers_bad.2plus or outliers_good.2plus, because they definitely have more than 
# 1 wrong across the 8 controls
d <- droplevels(subset(d, !(d$workerid %in% outliers_bad.2plus$workerid) & !(d$workerid %in% outliers_good.2plus$workerid)))
length(unique(d$workerid)) #363 Turkers (400-363 = 37 excluded)

# now remove participants who are in both outliers_bad.1 and outliers_good.1, because they also have more than one wrong
# across the 8 controls

outliers_bad.1$workerid
outliers_good.1$workerid

d <- droplevels(subset(d, !(d$workerid %in% outliers_bad.1$workerid & d$workerid %in% outliers_good.1$workerid)))
length(unique(d$workerid)) #353 Turkers (10 excluded who are in both sets)

# 47 excluded based on controls

# clean data
cd = d
write.csv(cd, "../data/cd.csv")
nrow(cd) #9884 / 28 items = 353 participants

# age info
table(cd$age) #18-73
length(which(is.na(cd$age))) # 28 missing values (1 Turker)
median(cd$age,na.rm=TRUE) #37
cd %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
# 180 female 173 male

