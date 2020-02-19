# Factives paper
# 4-veridicality3 (inference ratings, continuous task)
# preprocessing.R

# What is true: Dan knows that Sophia got a tattoo.
# Does it follow that Sophia got a tattoo?

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

mean(d$Answer.time_in_minutes) #6.2
median(d$Answer.time_in_minutes) #5.1

d = d %>%
  select(workerid,rt,subjectGender,speakerGender,content,verb,contentNr,trigger_class,response,slide_number_in_experiment,age,language,asses,american,gender,comments,Answer.time_in_minutes)
nrow(d) #8400

# look at Turkers' comments
unique(d$comments)

# how did Turkers assess their performance?
table(d$asses)
# Did you understand the task?
#Confused   No      Yes 
#252      280     7840 

# age and gender info
length(which(is.na(d$age))) #0
table(d$age) #19-69
median(d$age,na.rm=TRUE) #36
d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
# 152 female, 148 male

### exclude non-American English speakers
length(unique(d$workerid)) #300
length(which(is.na(d$language))) #no missing responses
table(d$language) 
d <- subset(d, (d$language != "" & d$language != "Russian" & d$language != "hindi" & d$language != "turkish" & d$language != "Tagalog" & d$language != "Ukrainian"))
d = droplevels(d)
length(unique(d$workerid)) #294 (6 Turkers excluded)

length(which(is.na(d$american))) #0 (everybody responded)
table(d$american) 
d <- subset(d, d$american == "0")
d = droplevels(d)
length(unique(d$workerid)) #286 (14 Turkers excluded for language reasons)

## exclude Turkers based on controls
names(d)
table(d$contentNr)
table(d$verb)

# make relevant data subsets
# control_bad: p does not follow (YES = 1)
c.bad <- droplevels(subset(d, d$verb == "control_bad"))
nrow(c.bad) #1144 / 4 contradictory controls = 286 Turkers

# control_good: p does follow (NO = 0)
c.good <- droplevels(subset(d, d$verb == "control_good"))
nrow(c.good) #1144

# all controls
c <- rbind(c.bad,c.good)
nrow(c) #2288

# group mean on "no follow" controls
round(mean(c.bad$response),2) #.05

# group mean on "follow" controls
round(mean(c.good$response),2) #.94

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
# contentNr   response
# 1 control_bad1 0.02716783
# 2 control_bad2 0.07160839
# 3 control_bad3 0.04961538
# 4 control_bad4 0.04412587

means = aggregate(response ~ contentNr, data=c.good, FUN="mean")
means
# contentNr  response
# 1 control_good1 0.9409790
# 2 control_good2 0.9503147
# 3 control_good3 0.9517832
# 4 control_good4 0.9356993

# all controls
c <- rbind(c.bad,c.good)
nrow(c) #2288 / 8 items = 286 Turkers

# Turkers with response means on "follow" (1) more than 2sd below group mean
cg.means = aggregate(response~workerid, data=c.good, FUN="mean")
cg.means$YMin = cg.means$response - aggregate(response~workerid, data=c.good, FUN="ci.low")$response
cg.means$YMax = cg.means$response + aggregate(response~workerid, data=c.good, FUN="ci.high")$response
cg.means

c.g <- cg.means[cg.means$response < (mean(cg.means$response) - 2*sd(cg.means$response)),]
c.g
unique(length(c.g$workerid)) #14 Turkers gave low responses
mean(c.g$response) #.66

# Turkers with response means on "no follow" controls (0) more than 2sd above group mean
cb.means = aggregate(response~workerid, data=c.bad, FUN="mean")
cb.means$YMin = cb.means$response - aggregate(response~workerid, data=c.bad, FUN="ci.low")$response
cb.means$YMax = cb.means$response + aggregate(response~workerid, data=c.bad, FUN="ci.high")$response
cb.means

c.b <- cb.means[cb.means$response > (mean(cb.means$response) + 2*sd(cb.means$response)),]
c.b
unique(length(c.b$workerid)) #19 Turkers gave low responses
mean(c.b$response) #.29

# how many unique Turkers did badly on the controls?
outliers <- subset(c, c$workerid %in% c.g$workerid | c$workerid %in% c.b$workerid)
outliers = droplevels(outliers)
nrow(outliers) #216 / 8 control items = 27 Turkers
table(outliers$workerid)

# look at the responses to the controls that these "outlier" Turkers did
# outliers to "no follow" items (0)
outliers.g <- subset(c.good, c.good$workerid %in% c.g$workerid)
outliers.g = droplevels(outliers.g)
nrow(outliers.g) #56 / 4 control items = 14 Turkers

ggplot(outliers.g, aes(x=workerid,y=response)) +
  geom_point(aes(colour = contentNr)) +
  geom_text(aes(label=workerid), vjust = 1, cex= 5,position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=response), vjust = 2.5, cex= 5) +
  scale_y_continuous(breaks = pretty(outliers.g$response, n = 10)) +
  ylab("Responses") +
  xlab("Participants")
# responses here are supposed to be 0 but these 14 Turkers
# gave consistently higher responses across the control items

# outliers to "follow" items (1)
outliers.b <- subset(c.bad, c.bad$workerid %in% c.b$workerid)
outliers.b = droplevels(outliers.b)
nrow(outliers.b) #76 / 4 control items = 19 Turkers

ggplot(outliers.b, aes(x=workerid,y=response)) +
  geom_point(aes(colour = contentNr)) +
  geom_text(aes(label=workerid), vjust = 1, cex= 5,position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=response), vjust = 2.5, cex= 5) +
  scale_y_continuous(breaks = pretty(outliers.b$response, n = 10)) +
  ylab("Responses") +
  xlab("Participants")
# responses here are supposed to be 1 but these 19 Turkers
# gave consistently lower responses across the control items 

# exclude the 27 Turkers identified above
d <- subset(d, !(d$workerid %in% outliers$workerid))
d <- droplevels(d)
length(unique(d$workerid)) #259 Turkers remain (27 excluded for problems with control items)


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

# lvw = d %>%
#   filter(as.character(workerid) %in% lowvarworkers) %>%
#   droplevels() %>%
#   mutate(Participant = as.factor(as.character(workerid)))
# 
# ggplot(lvw,aes(x=Participant,y=response)) +
#   geom_point()

# exclude the 0 Turkers identified above
# d <- droplevels(subset(d, !(d$workerid %in% lowvarworkers)))
# length(unique(d$workerid)) #266 Turkers remain

# # exclude workers with factive and veridical non-factive mean smaller than or equal 
# # to control_bad mean (where p definitely doesn't follow)
# fcmeans = d %>%
#   filter(verb %in% c("control_good","be_annoyed","see","discover","reveal","know")) %>%
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

# new group means
c.bad <- droplevels(subset(d, d$verb == "control_bad"))
nrow(c.bad) #1036 / 4 contradictory controls = 259 Turkers
c.good <- droplevels(subset(d, d$verb == "control_good"))
nrow(c.good) #1036
round(mean(c.bad$response),2) #.03
round(mean(c.good$response),2) #.96

# change the name of the predicates
table(d$verb)
d$verb <- gsub("be_right_that","be_right",d$verb)
d$verb <- gsub("inform_Sam","inform",d$verb)
d$verb <- gsub("annoyed","be_annoyed",d$verb)
d$verb <- gsub("control_good","entailing C",d$verb)
d$verb <- gsub("control_bad","non-ent. C",d$verb)
d$verb <- as.factor(d$verb)

# save clean data
cd = d
write.csv(cd, "../data/cd.csv")
nrow(cd) #7252 / 28 items = 259 participants

# age and gender info of remaining participants
table(cd$age) #19-69
length(which(is.na(cd$age))) #0 missing values
median(cd$age,na.rm=TRUE) #36
cd %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
# 132 female, 128 male
