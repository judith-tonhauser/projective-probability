# Prior probability work
# 6-veridicality2-binary (contradictoriness diagnostic)

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

## NO NEED TO RUN THIS FIRST BIT IF YOU JUST WANT TO LOAD CLEAN DATA. 
## SEARCH FOR "load clean data for analysis"

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

# remove participants who gave "yes" response to at least one non-contradictory control
# or "No" response to at least one contradictory control
outliers_bad = c.bad %>%
  group_by(workerid) %>%
  summarize(Prop = mean(nResponse)) %>%
  filter(Prop != 1)
outliers_bad
nrow(outliers_bad) #56

outliers_good = c.good %>%
  group_by(workerid) %>%
  summarize(Prop = mean(nResponse)) %>%
  filter(Prop != 0)
outliers_good 
nrow(outliers_good) #75

d <- droplevels(subset(d, !(d$workerid %in% outliers_bad$workerid) & !(d$workerid %in% outliers_good$workerid)))
length(unique(d$workerid)) #302 Turkers (98 excluded)

# clean data
cd = d
write.csv(cd, "../data/cd.csv")
nrow(cd) #8456 / 26 items = 302 participants

# load clean data for analysis ----
cd = read.csv("../data/cd.csv")
nrow(cd) #8456 (302 Turkers)

# age info
table(cd$age) #18-73
length(which(is.na(cd$age))) # 28 missing values (1 Turker)
median(cd$age,na.rm=TRUE) #37
cd %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
# 150 female 152 male

# plots ----

# proportion of projective answers by predicate, including the main clause controls
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
                       ifelse(cols$V %in% c("contradictory C", "non-contrd. C"),"MC","V")))))

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
                       ifelse(prop$verb  %in% c("contradictory C", "non-contrd. C"),"MC","V")))))


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
  ylab("Proporition of 'yes (contrd.)' answers") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) 
ggsave("../graphs/proportion-by-predicate-variability-individual.pdf",height=4,width=7)


# JD CODE STARTS HERE
# TL;DR: all verbs are different from bad controls
cd <- read.csv(file="../data/cd.csv", header=TRUE, sep=",")

# Bayesian mixed effects regression to test whether ratings differ by predicate from good controls
cd$workerid = as.factor(as.character(cd$workerid))

# plotting slider ratings suggests we should use a zoib model
ggplot(cd, aes(x=response)) +
  geom_histogram(stat="count")

# exclude bad controls from analysis -- they're not relevant, right?
d = cd %>%
  # filter(verb != "control_bad") %>%
  droplevels() %>%
  # mutate(verb = fct_relevel(verb,"contradictory C")) %>%
  mutate(verb = fct_relevel(verb,"be_right")) %>%
  mutate(nResponse = ifelse(response == "Yes", 1, 0))

# JD removed the by-content intercepts and slopes for verb because it was taking TOO DAMN LONG and appeared to get stuck and not converge, and the reason for this is simply that the data are extreme and there is basically no item variability to speak of. barely any subject variability, too. had to 
m <- glmer(response ~ verb + (1|workerid), data=d, family="binomial")
# m <- glm(response ~ verb, data=d, family="binomial")
summary(m)

allm = allFit(m)

is.OK <- sapply(allm,is,"merMod")  ##  failed, others succeeded
allm.OK <- allm[is.OK]
allm.OK
lapply(allm.OK,function(x) x@optinfo$conv$lme4$messages)

summary(allm) # bobyqa exited ok, so we can interpret model above
summary(m)


m <- brm(nResponse~verb + (1|workerid), data=d, family=bernoulli())

# no neeed to run this multiple times
saveRDS(m,file="../data/bernoulli-model.rds")

summary(m) # see summary printed below
