# Factives paper
# 7-veridicality3-binary (inference diagnostic, binary)
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
theme_set(theme_bw())

# load raw data
d = read.csv("../data/experiment_noreps.csv")
head(d)
nrow(d) #12068 = 431 participants x 28 items
names(d)
length(unique(d$workerid)) #431 participants

mean(d$Answer.time_in_minutes) #5.04
median(d$Answer.time_in_minutes) #4.3

d = d %>%
  select(workerid,rt,content,subjectGender,speakerGender,verb,contentNr,trigger_class,response,slide_number_in_experiment,age,language,american,gender,comments,Answer.time_in_minutes)
nrow(d) #12068

# recode some names and add nResponse column (numeric variant of yes/no response)
table(d$verb)

d = d %>%
  mutate(verb=recode(verb, control_good = "entailing C", control_bad = "non-ent. C", annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform"),
         nResponse = ifelse(response == "Yes",1,0))

head(d)

table(d$contentNr,d$nResponse)
# control_good = entailing controls
# control_bad = non-entailing controls

# look at Turkers' comments
unique(d$comments)


# age and gender info
length(which(is.na(d$age))) #56 missing values (2 Turkers)
table(d$age) #18-73 (plus one funny answer)
median(d$age,na.rm=TRUE) #37
d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
#218 female, 212 male, 1 non-declared

### exclude non-American English speakers
length(unique(d$workerid)) #431
length(which(is.na(d$language))) #no missing responses
table(d$language) 
d$language <- trimws(d$language)
d <- droplevels(subset(d, (d$language != "United States" & d$language != "" & d$language != "Arabic" & d$language != "British English" & d$language != "Bulgarian" & d$language != "Chinese" & d$language != "French" & d$language != "hindi" & d$language != "Italian" & d$language != "Korean" & d$language != "Nepali" & d$language != "spanish" & d$language != "Turkish" & d$language != "United States " & d$language != "Vietnamese" & d$language != "yes")))
length(unique(d$workerid)) #410 (21 Turkers excluded)

# American English
length(which(is.na(d$american))) #0
table(d$american) 
d <- droplevels(subset(d, d$american == "0"))
length(unique(d$workerid)) #396 (14 Turkers excluded)

# 35 Turkers excluded for language reasons

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

min(d$Answer.time_in_minutes) #.98

## exclude Turkers based on controls
names(d)
table(d$contentNr)
table(d$verb)

# make control data subsets
# control_bad = non-ent. controls ('no' / 0 expected)
# control_good = entailing controls ('yes' / 1 expected)

c.bad <- subset(d, d$verb == "non-ent. C")
c.bad <- droplevels(c.bad)
head(c.bad)
nrow(c.bad) #1584 / 4 controls = 396 Turkers

c.good <- subset(d, d$verb == "entailing C")
c.good <- droplevels(c.good)
head(c.good)
nrow(c.good) #1584

controlresponses.bad = c.bad %>%
  count(workerid, response) %>%
  group_by(workerid) %>%
  filter(response == "No")

ggplot(controlresponses.bad, aes(x=n)) +
  geom_histogram()
# most Turkers only said No to the 4 non-entailing controls

controlresponses.good = c.good %>%
  count(workerid, response) %>%
  group_by(workerid) %>%
  filter(response == "Yes")

ggplot(controlresponses.good, aes(x=n)) +
  geom_histogram()
# most Turkers only said Yes to the 4 entailing controls

# remove participants who gave "yes" response to a non-entailing (bad) controls
# or "No" response to more than one entailing (good) controls
# outliers_good: Prop smaller than .74 means that they got more than one wrong
outliers_bad = c.bad %>%
  group_by(workerid) %>%
  summarize(Prop = mean(nResponse)) %>%
  filter(Prop != 0)
outliers_bad
nrow(outliers_bad) #39

outliers_good = c.good %>%
  group_by(workerid) %>%
  summarize(Prop = mean(nResponse)) %>%
  filter(Prop < .74)
outliers_good 
nrow(outliers_good) #6

d <- droplevels(subset(d, !(d$workerid %in% outliers_bad$workerid) | !(d$workerid %in% outliers_good$workerid)))
length(unique(d$workerid)) #356 Turkers (396-356 = 40 excluded)

# clean data
cd = d
write.csv(cd, "../data/cd.csv")
nrow(cd) #9968 / 28 items = 356 participants

# age info
table(cd$age) #18-73 (699 not considered)
length(which(is.na(cd$age))) # 56 missing values (2 Turkers)
median(cd[cd$age < 200,]$age,na.rm=TRUE) #38 (699 not considered)
cd %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
#184 female, 171 male

