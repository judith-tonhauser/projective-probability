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
  scale_fill_manual(values=c("darkorchid","black","gray60","tomato1","dodgerblue")) +
  guides(fill=FALSE) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position="top") +
  ylab("Proporition of 'yes (def. follows)' answers") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) 
ggsave("../graphs/proportion-by-predicate-variability.pdf",height=4,width=7)

## models -----
library(emmeans)
library(lme4)
library(languageR)
library(brms)

table(cd$verb)
cd$verb = relevel(cd$verb,ref="entailing C")
cd$item = as.factor(paste(cd$verb,cd$content))

## Bayesian models ----

run_model <- function(expr, path, reuse = TRUE) {
  path <- paste0(path, ".Rds")
  if (reuse) {
    fit <- suppressWarnings(try(readRDS(path), silent = TRUE))
  }
  if (is(fit, "try-error")) {
    fit <- eval(expr)
    saveRDS(fit, file = path)
  }
  fit
}

fit <- run_model(brm(nResponse ~ verb + (1|workerid) + (1|item), data=cd, family=bernoulli()), path = "../models/predict-response-from-verb-no-slopes.Rds")
tmp <- readRDS('../models/predict-response-from-verb-no-slopes.Rds.Rds')
summary(tmp) #did not converge

fit <- run_model(brm(nResponse ~ verb + (verb|workerid) + (1|item), data=cd, family=bernoulli()), path = "../models/predict-response-from-verb-no-slopes")
tmp <- readRDS('../models/predict-response-from-verb-with-slope.Rds')
summary(tmp) #did not converge

