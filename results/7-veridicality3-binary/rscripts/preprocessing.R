# Factives paper
# 7-veridicality3-binary (inference diagnostic, binary)
# preprocessing.R
# (Exp 2b in Language paper)

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
  mutate(verb=recode(verb, control_good = "entailing C", control_bad = "non-ent. C", annoyed = "be annoyed", be_right_that = "be right", inform_Sam = "inform"),
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
length(unique(d$workerid))

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


# exclusion criterion: remove participants who got more than one answer wrong on the 8 controls
# outliers_good: Prop 1 means all correct, Prop smaller than .75 means that they got more than 1 wrong
# outliers_bad: Prop 0 means all correct, Prop larger than .25 means that they got more than 1 wrong
outliers_good.2plus = c.good %>%
  group_by(workerid) %>%
  summarize(Prop = mean(nResponse)) %>%
  filter(Prop < .75)
outliers_good.2plus
nrow(outliers_good.2plus) #6 (participant who got more than one wrong; remaining have 0 or 1 wrong)

ggplot(outliers_good.2plus, aes(x=Prop)) +
  geom_histogram()
#4 people got 2 wrong, 2 got 3 wrong

outliers_good.1 = c.good %>%
  group_by(workerid) %>%
  summarize(Prop = mean(nResponse)) %>%
  filter(Prop == .75)
outliers_good.1
nrow(outliers_good.1) #24 (participants who got exactly one wrong)

outliers_bad.2plus = c.bad %>%
  group_by(workerid) %>%
  summarize(Prop = mean(nResponse)) %>%
  filter(Prop > .25)
outliers_bad.2plus 
nrow(outliers_bad.2plus) #18 (participant who got more than one wrong; remaining have 0 or 1 wrong)

ggplot(outliers_bad.2plus, aes(x=Prop)) +
  geom_histogram()
# 5 people got 4 wrong, 6 people got 3 wrong, 7 people got 2 wrong

outliers_bad.1 = c.bad %>%
  group_by(workerid) %>%
  summarize(Prop = mean(nResponse)) %>%
  filter(Prop == .25)
outliers_bad.1 
nrow(outliers_bad.1) #21 (participants who got exactly one wrong)

# remove participants who are in either outliers_bad.2plus or outliers_good.2plus, because they definitely have more than 
# 1 wrong across the 8 controls
d <- droplevels(subset(d, !(d$workerid %in% outliers_bad.2plus$workerid) & !(d$workerid %in% outliers_good.2plus$workerid)))
length(unique(d$workerid)) #376 Turkers (396 - 376 = 20 Turkers excluded)

# now remove participants who are in both outliers_bad.1 and outliers_good.1, because they also have more than one wrong
# across the 8 controls

outliers_bad.1$workerid
outliers_good.1$workerid

d <- droplevels(subset(d, !(d$workerid %in% outliers_bad.1$workerid & d$workerid %in% outliers_good.1$workerid)))
length(unique(d$workerid)) #375 Turkers (376 - 375 = 1 excluded)

# 21 excluded based on controls

# clean data
cd = d
write.csv(cd, "../data/cd.csv")
nrow(cd) #10500 / 28 items = 375 participants

# age info
table(cd$age) #18-73 (699 not considered)
length(which(is.na(cd$age))) # 56 missing values (2 Turkers)
median(cd[cd$age < 200,]$age,na.rm=TRUE) #38 (699 not considered)
cd %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
#187 female, 187 male, 1 undeclared

