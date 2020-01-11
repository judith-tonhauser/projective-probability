# Prior probability work
# 7-veridicality3-binary (inference diagnostic)

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


## NO NEED TO RUN THIS FIRST BIT IF YOU JUST WANT TO LOAD CLEAN DATA. 
## SEARCH FOR "load clean data for analysis"

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
length(unique(d$workerid)) #396 (16 Turkers excluded)

# 37 Turkers excluded for language reasons

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

# remove participants who gave "yes" response to at least one non-contradictory control
# or "No" response to at least one contradictory control
outliers_bad = c.bad %>%
  group_by(workerid) %>%
  summarize(Prop = mean(nResponse)) %>%
  filter(Prop != 0)
outliers_bad
nrow(outliers_bad) #39

outliers_good = c.good %>%
  group_by(workerid) %>%
  summarize(Prop = mean(nResponse)) %>%
  filter(Prop != 1)
outliers_good 
nrow(outliers_good) #30

d <- droplevels(subset(d, !(d$workerid %in% outliers_bad$workerid) & !(d$workerid %in% outliers_good$workerid)))
length(unique(d$workerid)) #341 Turkers (396-341 = 55 excluded)

# clean data
cd = d
write.csv(cd, "../data/cd.csv")
nrow(cd) #9548 / 28 items = 341 participants

# load clean data for analysis ----
cd = read.csv("../data/cd.csv")
nrow(cd) #9548 

# age info
table(cd$age) #18-73
length(which(is.na(cd$age))) # 56 missing values (2 Turkers)
median(cd$age,na.rm=TRUE) #38
cd %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
#180 female, 161 male

# how many participants said 'no' to which predicates?
str(cd$response)
cd %>% 
  select(workerid,response,verb) %>% 
  unique() %>% 
  group_by(verb) %>% 
  summarise(sum(response == "No")) %>%
  print(n=40)

# plots ----

# mean projectivity by predicate, including the main clause controls
prop = cd %>%
  group_by(verb) %>%
  summarize(Mean = mean(nResponse), CILow = ci.low(nResponse), CIHigh = ci.high(nResponse)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(verb),Mean))
prop
levels(prop$verb)

# define colors for the predicates
cols = data.frame(V=levels(prop$verb))

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate"),"VNF",
                       ifelse(cols$V %in% c("entailing C", "non-ent. C"),"MC","V")))))

levels(cols$V)
cols$V <- factor(cols$V, levels = cols[order(as.character(prop$verb)),]$V, ordered = TRUE)

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "MC","black","tomato1"))))


# plot of proportions

prop$VeridicalityGroup = as.factor(
  ifelse(prop$verb %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(prop$verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(prop$verb  %in% c("be_right","demonstrate"),"VNF",
                       ifelse(prop$verb  %in% c("entailing C", "non-ent. C"),"MC","V")))))

ggplot(prop, aes(x=verb, y=Mean, fill=VeridicalityGroup)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  geom_jitter(data=cd,aes(y=nResponse),color="gray40",alpha=.2,fill="black") +
  scale_fill_manual(values=c("darkorchid","black","gray60","tomato1","dodgerblue")) +
  guides(fill=FALSE) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position="top") +
  ylab("Proporition of 'yes (def. follows)' answers") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) 
ggsave("../graphs/proportion-by-predicate-variability-individual.pdf",height=4,width=7)

# plot Turker's 'no' responses for predicates up to "demonstrate" ----
table(cd$verb)

# subset by relevant 13 predicates
no <- droplevels(subset(cd,verb == "demonstrate" | verb == "confess" |verb == "reveal" |
                                    verb == "establish" | verb == "admit" |verb == "acknowledge" |
                                    verb == "be_annoyed" | verb == "confirm" | verb == "discover" | 
                                    verb == "know" | verb == "see" | verb == "be_right" | verb == "prove"))

table(no$verb)
str(no$verb)

# calculate mean response, for ordering
prop = no %>%
  group_by(verb) %>%
  summarize(Mean = mean(nResponse)) %>%
  mutate(verb = fct_reorder(as.factor(verb),Mean))
prop
levels(prop$verb)


no <- merge(no,prop,by="verb")
head(no)
no$verb <- reorder(no$verb,no$Mean)
levels(no$verb)


length(unique(no[no$response == "No",]$workerid)) #80 different workers


# plot

no$VeridicalityGroup = as.factor(
  ifelse(no$verb %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(no$verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(no$verb  %in% c("be_right","demonstrate"),"VNF",
                       ifelse(no$verb  %in% c("entailing C", "non-ent. C"),"MC","V")))))

# define colors for the predicates
cols = data.frame(V=levels(no$verb))

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate"),"VNF",
                       ifelse(cols$V %in% c("entailing C", "non-ent. C"),"MC","V")))))

levels(cols$V)
cols$V <- factor(cols$V, levels = cols[order(as.character(prop$verb)),]$V, ordered = TRUE)

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "MC","black","tomato1"))))


# how many 'no' responses did the Turkers give
no.part = no %>%
  group_by(workerid) %>%
  summarise(Sum = sum(response == "No")) %>%
  print(n=100) %>%
  mutate(workerid = fct_reorder(as.factor(as.character(workerid)),Sum))

levels(no.part$workerid)

# reorder participants
no <- merge(no,no.part,by="workerid")
head(no)
no$workerid <- reorder(no$workerid,-no$Sum)

#no$workerid <- factor(no$workerid, levels = no[order(as.character(no.part$workerid)),]$workerid, ordered = TRUE)

# plot 'no' responses by predicate and Turker ID
levels(no$verb)

ggplot(no[no$response == "No",], aes(x=verb, y=workerid, fill=VeridicalityGroup)) +
  geom_point(shape=21,stroke=.5,size=2.5,color="black",show.legend = FALSE) +
  scale_fill_manual(values=c("darkorchid","tomato1","dodgerblue")) +
  ylab("Participant ID") +
  xlab("Predicate") +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors))
ggsave("../graphs/no-entailment-responses-by-pred-and-worker.pdf",height=4,width=7)

## models -----
# library(emmeans)
library(lme4)
# library(languageR)
library(brms)
# JD CODE STARTS HERE
# TL;DR: all verbs are different from bad controls
cd <- read.csv(file="../data/cd.csv", header=TRUE, sep=",")

# Bayesian mixed effects regression to test whether ratings differ by predicate from good controls
cd$workerid = as.factor(as.character(cd$workerid))

# plotting slider ratings suggests we should use a zoib model
ggplot(cd, aes(x=response)) +
  geom_histogram(stat="count")

# relevel verb so you're not taking entailing controls as reference level, because that won't converge (no variance). instead, take "most entailing" predicate as determined by proportion of "Yes" -- "prove"
d = cd %>%
  droplevels() %>%
  mutate(verb = fct_relevel(verb,"prove")) %>%
  mutate(nResponse = ifelse(response == "Yes", 1, 0)) # in case you run brm model

# removed the by-content intercepts and slopes for verb because it was taking TOO DAMN LONG and appeared to get stuck and not converge, and the reason for this is simply that the data are extreme and there is basically no item variability to speak of. barely any subject variability, too.
m <- glmer(response ~ verb + (1|workerid), data=d, family="binomial")
# m <- glm(response ~ verb, data=d, family="binomial")
summary(m)

# Generalized linear mixed model fit by maximum likelihood (Laplace
#                                                           Approximation) [glmerMod]
# Family: binomial  ( logit )
# Formula: response ~ verb + (1 | workerid)
# Data: d
# 
# AIC      BIC   logLik deviance df.resid 
# 2766.2   2931.0  -1360.1   2720.2     9525 
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -31.0574   0.0000   0.0151   0.0871  11.0917 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# workerid (Intercept) 4.665    2.16    
# Number of obs: 9548, groups:  workerid, 341
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        7.8600     1.0103   7.780 7.24e-15 ***
#   verbacknowledge   -2.7949     1.0519  -2.657  0.00788 ** 
#   verbadmit         -2.9769     1.0463  -2.845  0.00444 ** 
#   verbannounce      -5.8638     1.0155  -5.774 7.72e-09 ***
#   verbbe_annoyed    -2.4563     1.0647  -2.307  0.02105 *  
#   verbbe_right      -0.4467     1.2817  -0.349  0.72743    
# verbconfess       -4.0361     1.0263  -3.933 8.40e-05 ***
#   verbconfirm       -2.1371     1.0806  -1.978  0.04795 *  
#   verbdemonstrate   -4.5122     1.0216  -4.417 1.00e-05 ***
#   verbdiscover      -1.5784     1.1203  -1.409  0.15887    
# verbentailing C   14.3657   589.8114   0.024  0.98057    
# verbestablish     -3.0819     1.0435  -2.954  0.00314 ** 
#   verbhear          -9.4853     1.0194  -9.305  < 2e-16 ***
#   verbinform        -5.3768     1.0168  -5.288 1.24e-07 ***
#   verbknow          -1.5548     1.1225  -1.385  0.16600    
# verbnon-ent. C   -30.5959  1005.3148  -0.030  0.97572    
# verbpretend      -13.4723     1.1138 -12.095  < 2e-16 ***
#   verbreveal        -3.4087     1.0360  -3.290  0.00100 ** 
#   verbsay           -7.4570     1.0145  -7.350 1.98e-13 ***
#   verbsee           -1.3088     1.1469  -1.141  0.25381    
# verbsuggest      -10.9062     1.0301 -10.588  < 2e-16 ***
#   verbthink        -12.1308     1.0517 -11.534  < 2e-16 ***

allm = allFit(m)

is.OK <- sapply(allm,is,"merMod")  ##  failed, others succeeded
allm.OK <- allm[is.OK]
allm.OK
lapply(allm.OK,function(x) x@optinfo$conv$lme4$messages)

summary(allm) # bobyqa exited ok, so we can interpret model above
summary(m)


# fit <- run_model(brm(nResponse ~ verb + (1|workerid) + (1|item), data=cd, family=bernoulli()), path = "../models/predict-response-from-verb-no-slopes.Rds")
# tmp <- readRDS('../models/predict-response-from-verb-no-slopes.Rds.Rds')
# summary(tmp) #did not converge

# fit <- run_model(brm(nResponse ~ verb + (verb|workerid) + (1|item), data=cd, family=bernoulli()), path = "../models/predict-response-from-verb-no-slopes")
# tmp <- readRDS('../models/predict-response-from-verb-with-slope.Rds')
# summary(tmp) #did not converge

