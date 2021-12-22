# Factives paper
# 2-veridicality2 (contradictoriness ratings, continuous task)
# preprocessing.R

# Sally: "Dan knows that Sophia got a tattoo, but she didn't."
# Is Sally's utterance contradictory?

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(tidybayes)
library(dichromat)
library(brms)
library(knitr)
theme_set(theme_bw())

# load raw data
d = read.csv("../experiment.csv")
nrow(d) #8400 = 300 participants x 28 items
names(d)
length(unique(d$workerid)) #300 participants

mean(d$Answer.time_in_minutes) #6.4
median(d$Answer.time_in_minutes) #5.2

d = d %>%
  select(workerid,rt,subjectGender,speakerGender,content,verb,contentNr,trigger_class,response,slide_number_in_experiment,age,language,asses,american,gender,comments,Answer.time_in_minutes)
nrow(d) #8400

# look at Turkers' comments
unique(d$comments)

# how did Turkers assess their performance?
table(d$asses)
# Did you understand the task?
#Confused   No      Yes 
#700        364     7280

# age and gender info
length(which(is.na(d$age))) #0
table(d$age) #18-72
median(d$age,na.rm=TRUE) #35 
d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
# 137 female, 162 male, 1 other

### exclude non-American English speakers
length(unique(d$workerid)) #300
length(which(is.na(d$language))) #no missing responses
table(d$language) 
d <- subset(d, (d$language != "" & d$language != "chinese" & d$language != "Portuguese"))
d = droplevels(d)
length(unique(d$workerid)) #295 (5 Turkers excluded)

length(which(is.na(d$american))) #0 (everybody responded)
table(d$american) 
d <- subset(d, d$american == "0")
d = droplevels(d)
length(unique(d$workerid)) #281 (19 Turkers excluded for language reasons)

## exclude Turkers based on controls
names(d)
table(d$contentNr)
table(d$verb)

# make relevant data subsets
# control_bad: contradictory controls (YES = 1)
c.bad <- subset(d, d$verb == "control_bad")
c.bad <- droplevels(c.bad)
nrow(c.bad) #1124 / 4 contradictory controls = 281 Turkers

# control_good: noncontradictory controls (NO = 0)
c.good <- subset(d, d$verb == "control_good")
c.good <- droplevels(c.good)
nrow(c.good) #1124

# all controls
c <- rbind(c.bad,c.good)
nrow(c) #2248

# group mean on contradictory controls
round(mean(c.bad$response),2) #.94

# group mean on noncontradictory controls
round(mean(c.good$response),2) #.08

ggplot(c.bad, aes(x=workerid,y=response)) +
  geom_point(aes(colour = content),position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  scale_y_continuous(breaks = pretty(c.bad$response, n = 10)) +
  ylab("Responses") +
  xlab("Participant")

ggplot(c.good, aes(x=workerid,y=response)) +
  geom_point(aes(colour = content),position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  scale_y_continuous(breaks = pretty(c.good$response, n = 10)) +
  ylab("Response") +
  xlab("Participant")

# group means on individual controls
means = aggregate(response ~ contentNr, data=c.bad, FUN="mean")
means
#    contentNr  response
#1 control_bad1 0.9397509
#2 control_bad2 0.9323132
#3 control_bad3 0.9256940
#4 control_bad4 0.9566548

means = aggregate(response ~ contentNr, data=c.good, FUN="mean")
means
#contentNr   response
#1 control_good1 0.16964413 "Zack believes that I'm married, but I'm actually single"
#2 control_good2 0.05313167
#3 control_good3 0.05120996
#4 control_good4 0.05811388

# all controls
c <- rbind(c.bad,c.good)
nrow(c) #2248 / 8 items = 281 Turkers

# Turkers with response means on noncontradictory controls (0) more than 2sd above group mean
cg.means = aggregate(response~workerid, data=c.good, FUN="mean")
cg.means$YMin = cg.means$response - aggregate(response~workerid, data=c.good, FUN="ci.low")$response
cg.means$YMax = cg.means$response + aggregate(response~workerid, data=c.good, FUN="ci.high")$response
cg.means

c.g <- cg.means[cg.means$response > (mean(cg.means$response) + 2*sd(cg.means$response)),]
c.g
unique(length(c.g$workerid)) #12 Turkers gave high responses
mean(c.g$response) #.6

# Turkers with response means on contradictory controls (1) more than 2sd below group mean
cb.means = aggregate(response~workerid, data=c.bad, FUN="mean")
cb.means$YMin = cb.means$response - aggregate(response~workerid, data=c.bad, FUN="ci.low")$response
cb.means$YMax = cb.means$response + aggregate(response~workerid, data=c.bad, FUN="ci.high")$response
cb.means

c.b <- cb.means[cb.means$response < (mean(cb.means$response) - 2*sd(cb.means$response)),]
c.b
unique(length(c.b$workerid)) #15 Turkers gave low responses
mean(c.b$response) #.58

# how many unique Turkers did badly on the controls?
outliers <- subset(c, c$workerid %in% c.g$workerid | c$workerid %in% c.b$workerid)
outliers = droplevels(outliers)
nrow(outliers) #144 / 8 control items = 18 Turkers
table(outliers$workerid)

# look at the responses to the controls that these "outlier" Turkers did
# outliers to noncontradictory items (0)
outliers.g <- subset(c.good, c.good$workerid %in% c.g$workerid)
outliers.g = droplevels(outliers.g)
nrow(outliers.g) #48 / 4 control items = 12 Turkers

ggplot(outliers.g, aes(x=workerid,y=response)) +
  geom_point(aes(colour = contentNr)) +
  geom_text(aes(label=workerid), vjust = 1, cex= 5,position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=response), vjust = 2.5, cex= 5) +
  scale_y_continuous(breaks = pretty(outliers.g$response, n = 10)) +
  ylab("Responses") +
  xlab("Participants")
# responses here are supposed to be 0 (noncontradictory) but these 9 Turkers
# gave consistently high responses across the control items

# outliers to contradictory items (1)
outliers.b <- subset(c.bad, c.bad$workerid %in% c.b$workerid)
outliers.b = droplevels(outliers.b)
nrow(outliers.b) #60 / 4 control items = 15 Turkers

ggplot(outliers.b, aes(x=workerid,y=response)) +
  geom_point(aes(colour = contentNr)) +
  geom_text(aes(label=workerid), vjust = 1, cex= 5,position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=response), vjust = 2.5, cex= 5) +
  scale_y_continuous(breaks = pretty(outliers.b$response, n = 10)) +
  ylab("Responses") +
  xlab("Participants")
# responses here are supposed to be 1 (contradictory) but these 7 Turkers
# gave consistently low responses across the control items 

# exclude the 18 Turkers identified above
d <- subset(d, !(d$workerid %in% outliers$workerid))
d <- droplevels(d)
length(unique(d$workerid)) #263 Turkers remain (18 excluded for problems with control items)

# exclude turkers who always clicked on roughly the same point on the scale 
# for the target items
# ie turkers whose variance in overall response distribution is more 
# than 2 sd below mean by-participant variance
variances = d %>%
  filter(verb != "control_good" & verb != "control_bad") %>%
  group_by(workerid) %>%
  summarize(Variance = var(response)) %>%
  mutate(TooSmall = Variance < mean(Variance) - 2*sd(Variance))
head(variances)
summary(variances)

lowvarworkers = as.character(variances[variances$TooSmall,]$workerid)
summary(variances)
lowvarworkers # 0 turkers consistently clicked on roughly the same point on the scale


# new group means
c.bad <- droplevels(subset(d, d$verb == "control_bad"))
nrow(c.bad) #1052 / 4 controls = 263 Turkers
c.good <- droplevels(subset(d, d$verb == "control_good"))
nrow(c.good) #1052
round(mean(c.bad$response),2) #.96
round(mean(c.good$response),2) #.06

# change the name of the predicates
table(d$verb)
d$verb <- gsub("be_right_that","be right",d$verb)
d$verb <- gsub("inform_Sam","inform",d$verb)
d$verb <- gsub("annoyed","be annoyed",d$verb)
d$verb <- gsub("control_good","noncontrd. C",d$verb)
d$verb <- gsub("control_bad","contradictory C",d$verb)

# save clean data = cd
cd = d
write.csv(cd, "../data/cd.csv")
nrow(cd) #7364 / 28 items = 263 participants

# age and gender info of remaining participants
table(cd$age) #18-72
length(which(is.na(cd$age))) #0 missing values
median(cd$age,na.rm=TRUE) #36
cd %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
# 126 female, 136 male, 1 other

