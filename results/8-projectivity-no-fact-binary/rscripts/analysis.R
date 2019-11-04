# Prior probability work
# 8-projectivity-no-fact-binary

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
  mutate(verb=recode(verb, control = "MC", annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform"),
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
# because clicking through takes about 40 minutes

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

# identify outlier Turkers, defined as Turkers who responded "yes/1" to at least one control
outlier_Turkers = c %>%
  group_by(workerid) %>%
  summarize(Prop = mean(nResponse)) %>%
  filter(Prop != 0)
outlier_Turkers

# remove participants who gave "yes/1" response to at least one control
d <- droplevels(subset(d, !(d$workerid %in% outlier_Turkers$workerid)))
length(unique(d$workerid)) #426 Turkers (482-426 = 56 excluded)

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
nrow(cd) #11076 / 26 items = 426 participants

# load clean data for analysis ----
cd = read.csv("../data/cd.csv")
nrow(cd) #11076

# age info
table(cd$age) #18-81
length(which(is.na(cd$age))) # 26 missing values = 1 Turker
median(cd$age,na.rm=TRUE) #37
table(cd$gender)
#200 female, 220 male, 2 other

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
                       ifelse(cols$V %in% c("MC"),"MC","V")))))

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
                       ifelse(prop$verb  %in% c("MC"),"MC","V")))))

levels(prop$VeridicalityGroup)

ggplot(prop, aes(x=verb, y=Mean, fill=VeridicalityGroup)) +
  #geom_point(color="black", size=4) +
  #geom_point(shape=21,fill="gray60",data=subjmeans, alpha=.1, color="gray40") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  #geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  scale_fill_manual(values=c("darkorchid","black","gray60","tomato1","dodgerblue")) +
  guides(fill=FALSE) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position="top") +
  ylab("Proportion of 'yes (certain)' answers") +
  xlab("Predicate") 
ggsave("../graphs/proportion-by-predicate-variability.pdf",height=4,width=7)

## non-Bayesian models ----
library(emmeans)
library(lme4)

summary(cd)

# create item as combination of predicate and complement clause
cd$item = as.factor(paste(cd$verb,cd$content))

# reorder verb by mean
prop = cd %>%
  group_by(verb) %>%
  summarize(Mean = mean(nResponse)) %>%
  mutate(verb = fct_reorder(as.factor(verb),Mean))
prop

cd$verb <- factor(cd$verb, levels = unique(levels(prop$verb)))
levels(cd$verb) 

# pairwise comparison
str(cd$nResponse)
str(cd$verb)
cd$verb <- as.factor(cd$verb)
str(cd$workerid)
cd$workerid <- as.factor(cd$workerid)

model = lmer(nResponse ~ verb + (1|workerid) + (1|item), data=cd, REML=F)
summary(model)

comparison = emmeans(model, pairwise~verb,adjust="tukey")
options(max.print=10000)
comparison

# be right, think, say, pretend, suggest are non-factive

# MC - be_right             -0.028553 0.0198 Inf  -1.441 0.9979 
# MC - think                -0.042164 0.0198 Inf  -2.127 0.8612 
# MC - say                  -0.063633 0.0198 Inf  -3.214 0.1446 
# MC - pretend              -0.068021 0.0198 Inf  -3.436 0.0764 
# MC - suggest              -0.068209 0.0198 Inf  -3.446 0.0740 
# MC - prove                -0.121643 0.0198 Inf  -6.134 <.0001 
# MC - confirm              -0.161629 0.0198 Inf  -8.168 <.0001 
# MC - establish            -0.188046 0.0198 Inf  -9.501 <.0001 
# MC - demonstrate          -0.305406 0.0198 Inf -15.416 <.0001 
# MC - announce             -0.571689 0.0198 Inf -28.883 <.0001 
# MC - confess              -0.574585 0.0198 Inf -29.002 <.0001 
# MC - admit                -0.673383 0.0198 Inf -34.014 <.0001 
# MC - reveal               -0.696777 0.0198 Inf -35.226 <.0001 
# MC - acknowledge          -0.785824 0.0198 Inf -39.696 <.0001 
# MC - hear                 -0.811492 0.0198 Inf -40.990 <.0001 
# MC - discover             -0.836268 0.0198 Inf -42.275 <.0001 
# MC - see                  -0.864274 0.0198 Inf -43.669 <.0001 
# MC - inform               -0.898770 0.0198 Inf -45.450 <.0001 
# MC - be_annoyed           -0.918678 0.0198 Inf -46.424 <.0001 
# MC - know                 -0.931394 0.0198 Inf -47.032 <.0001 


# be_right - prove          -0.093090 0.0221 Inf  -4.211 0.0045 
# be_right - confirm        -0.133076 0.0221 Inf  -6.031 <.0001 
# be_right - establish      -0.159493 0.0221 Inf  -7.226 <.0001 
# be_right - demonstrate    -0.276853 0.0221 Inf -12.534 <.0001 
# be_right - announce       -0.543135 0.0221 Inf -24.608 <.0001 
# be_right - confess        -0.546032 0.0221 Inf -24.720 <.0001 
# be_right - admit          -0.644830 0.0221 Inf -29.210 <.0001 
# be_right - reveal         -0.668223 0.0221 Inf -30.291 <.0001 
# be_right - acknowledge    -0.757271 0.0221 Inf -34.305 <.0001 
# be_right - hear           -0.782939 0.0221 Inf -35.466 <.0001 
# be_right - discover       -0.807715 0.0221 Inf -36.612 <.0001 
# be_right - see            -0.835721 0.0221 Inf -37.866 <.0001 
# be_right - inform         -0.870217 0.0221 Inf -39.456 <.0001 
# be_right - be_annoyed     -0.890125 0.0221 Inf -40.335 <.0001 
# be_right - know           -0.902841 0.0221 Inf -40.887 <.0001 
# think - say               -0.021470 0.0221 Inf  -0.972 1.0000 
# think - pretend           -0.025857 0.0221 Inf  -1.171 0.9999 
# think - suggest           -0.026045 0.0221 Inf  -1.179 0.9999 
# think - prove             -0.079479 0.0221 Inf  -3.593 0.0461 
# think - confirm           -0.119465 0.0221 Inf  -5.410 <.0001 
# think - establish         -0.145882 0.0221 Inf  -6.605 <.0001 
# think - demonstrate       -0.263242 0.0221 Inf -11.910 <.0001 
# think - announce          -0.529525 0.0221 Inf -23.976 <.0001 
# think - confess           -0.532421 0.0221 Inf -24.089 <.0001 
# think - admit             -0.631220 0.0221 Inf -28.576 <.0001 
# think - reveal            -0.654613 0.0221 Inf -29.656 <.0001 
# think - acknowledge       -0.743660 0.0221 Inf -33.668 <.0001 
# think - hear              -0.769328 0.0221 Inf -34.829 <.0001 
# think - discover          -0.794104 0.0221 Inf -35.973 <.0001 
# think - see               -0.822111 0.0221 Inf -37.226 <.0001 
# think - inform            -0.856606 0.0221 Inf -38.815 <.0001 
# think - be_annoyed        -0.876514 0.0221 Inf -39.695 <.0001 
# think - know              -0.889230 0.0221 Inf -40.247 <.0001 
# say - pretend             -0.004387 0.0221 Inf  -0.199 1.0000 
# say - suggest             -0.004575 0.0221 Inf  -0.207 1.0000 
# say - prove               -0.058009 0.0221 Inf  -2.626 0.5091 
# say - confirm             -0.097995 0.0221 Inf  -4.443 0.0017 
# say - establish           -0.124413 0.0221 Inf  -5.639 <.0001 
# say - demonstrate         -0.241772 0.0221 Inf -10.951 <.0001 
# say - announce            -0.508055 0.0221 Inf -23.030 <.0001 
# say - confess             -0.510952 0.0221 Inf -23.143 <.0001 
# say - admit               -0.609750 0.0221 Inf -27.634 <.0001 
# say - reveal              -0.633143 0.0220 Inf -28.715 <.0001 
# say - acknowledge         -0.722190 0.0221 Inf -32.732 <.0001 
# say - hear                -0.747859 0.0221 Inf -33.894 <.0001 
# say - discover            -0.772634 0.0221 Inf -35.039 <.0001 
# say - see                 -0.800641 0.0221 Inf -36.294 <.0001 
# say - inform              -0.835137 0.0220 Inf -37.884 <.0001 
# say - be_annoyed          -0.855044 0.0221 Inf -38.765 <.0001 
# say - know                -0.867761 0.0221 Inf -39.318 <.0001 
# pretend - suggest         -0.000188 0.0221 Inf  -0.009 1.0000 
# pretend - prove           -0.053622 0.0221 Inf  -2.427 0.6656 
# pretend - confirm         -0.093608 0.0221 Inf  -4.244 0.0039 
# pretend - establish       -0.120025 0.0221 Inf  -5.440 <.0001 
# pretend - demonstrate     -0.237385 0.0221 Inf -10.752 <.0001 
# pretend - announce        -0.503668 0.0221 Inf -22.830 <.0001 
# pretend - confess         -0.506564 0.0221 Inf -22.943 <.0001 
# pretend - admit           -0.605363 0.0221 Inf -27.434 <.0001 
# pretend - reveal          -0.628756 0.0221 Inf -28.515 <.0001 
# pretend - acknowledge     -0.717803 0.0221 Inf -32.532 <.0001 
# pretend - hear            -0.743472 0.0221 Inf -33.694 <.0001 
# pretend - discover        -0.768247 0.0221 Inf -34.839 <.0001 
# pretend - see             -0.796254 0.0221 Inf -36.094 <.0001 
# pretend - inform          -0.830749 0.0220 Inf -37.683 <.0001 
# pretend - be_annoyed      -0.850657 0.0221 Inf -38.564 <.0001 
# pretend - know            -0.863374 0.0221 Inf -39.118 <.0001 
# suggest - prove           -0.053434 0.0221 Inf  -2.419 0.6717 
# suggest - confirm         -0.093420 0.0221 Inf  -4.236 0.0041 
# suggest - establish       -0.119837 0.0221 Inf  -5.433 <.0001 
# suggest - demonstrate     -0.237197 0.0221 Inf -10.745 <.0001 
# suggest - announce        -0.503480 0.0221 Inf -22.825 <.0001 
# suggest - confess         -0.506377 0.0221 Inf -22.939 <.0001 
# suggest - admit           -0.605175 0.0221 Inf -27.431 <.0001 
# suggest - reveal          -0.628568 0.0220 Inf -28.511 <.0001 
# suggest - acknowledge     -0.717615 0.0221 Inf -32.529 <.0001 
# suggest - hear            -0.743284 0.0221 Inf -33.691 <.0001 
# suggest - discover        -0.768059 0.0220 Inf -34.836 <.0001 
# suggest - see             -0.796066 0.0221 Inf -36.092 <.0001 
# suggest - inform          -0.830561 0.0220 Inf -37.681 <.0001 
# suggest - be_annoyed      -0.850469 0.0221 Inf -38.563 <.0001 
# suggest - know            -0.863186 0.0221 Inf -39.116 <.0001 
# prove - confirm           -0.039986 0.0221 Inf  -1.811 0.9679 
# prove - establish         -0.066403 0.0221 Inf  -3.006 0.2437 
# prove - demonstrate       -0.183763 0.0221 Inf  -8.313 <.0001 
# prove - announce          -0.450046 0.0221 Inf -20.373 <.0001 
# prove - confess           -0.452942 0.0221 Inf -20.488 <.0001 
# prove - admit             -0.551741 0.0221 Inf -24.972 <.0001 
# prove - reveal            -0.575134 0.0221 Inf -26.049 <.0001 
# prove - acknowledge       -0.664181 0.0221 Inf -30.063 <.0001 
# prove - hear              -0.689850 0.0221 Inf -31.224 <.0001 
# prove - discover          -0.714625 0.0221 Inf -32.365 <.0001 
# prove - see               -0.742632 0.0221 Inf -33.620 <.0001 
# prove - inform            -0.777127 0.0221 Inf -35.206 <.0001 
# prove - be_annoyed        -0.797035 0.0221 Inf -36.087 <.0001 
# prove - know              -0.809751 0.0221 Inf -36.641 <.0001 
# confirm - establish       -0.026417 0.0221 Inf  -1.198 0.9999 
# confirm - demonstrate     -0.143777 0.0221 Inf  -6.515 <.0001 
# confirm - announce        -0.410060 0.0221 Inf -18.595 <.0001 
# confirm - confess         -0.412956 0.0221 Inf -18.712 <.0001 
# confirm - admit           -0.511754 0.0221 Inf -23.202 <.0001 
# confirm - reveal          -0.535148 0.0220 Inf -24.280 <.0001 
# confirm - acknowledge     -0.624195 0.0221 Inf -28.302 <.0001 
# confirm - hear            -0.649864 0.0221 Inf -29.465 <.0001 
# confirm - discover        -0.674639 0.0220 Inf -30.607 <.0001 
# confirm - see             -0.702646 0.0221 Inf -31.865 <.0001 
# confirm - inform          -0.737141 0.0220 Inf -33.452 <.0001 
# confirm - be_annoyed      -0.757049 0.0220 Inf -34.336 <.0001 
# confirm - know            -0.769765 0.0221 Inf -34.892 <.0001 
# establish - demonstrate   -0.117360 0.0221 Inf  -5.317 <.0001 
# establish - announce      -0.383642 0.0221 Inf -17.393 <.0001 
# establish - confess       -0.386539 0.0221 Inf -17.511 <.0001 
# establish - admit         -0.485337 0.0221 Inf -22.000 <.0001 
# establish - reveal        -0.508730 0.0220 Inf -23.076 <.0001 
# establish - acknowledge   -0.597778 0.0221 Inf -27.098 <.0001 
# establish - hear          -0.623446 0.0221 Inf -28.260 <.0001 
# establish - discover      -0.648222 0.0220 Inf -29.402 <.0001 
# establish - see           -0.676228 0.0221 Inf -30.660 <.0001 
# establish - inform        -0.710724 0.0220 Inf -32.246 <.0001 
# establish - be_annoyed    -0.730632 0.0221 Inf -33.130 <.0001 
# establish - know          -0.743348 0.0221 Inf -33.687 <.0001 
# demonstrate - announce    -0.266283 0.0221 Inf -12.063 <.0001 
# demonstrate - confess     -0.269180 0.0221 Inf -12.185 <.0001 
# demonstrate - admit       -0.367978 0.0221 Inf -16.667 <.0001 
# demonstrate - reveal      -0.391371 0.0221 Inf -17.739 <.0001 
# demonstrate - acknowledge -0.480418 0.0221 Inf -21.762 <.0001 
# demonstrate - hear        -0.506087 0.0221 Inf -22.923 <.0001 
# demonstrate - discover    -0.530862 0.0221 Inf -24.061 <.0001 
# demonstrate - see         -0.558869 0.0221 Inf -25.320 <.0001 
# demonstrate - inform      -0.593364 0.0221 Inf -26.901 <.0001 
# demonstrate - be_annoyed  -0.613272 0.0221 Inf -27.788 <.0001 
# demonstrate - know        -0.625989 0.0221 Inf -28.347 <.0001 
# announce - confess        -0.002897 0.0221 Inf  -0.131 1.0000 
# announce - admit          -0.101695 0.0221 Inf  -4.610 0.0008 
# announce - reveal         -0.125088 0.0220 Inf  -5.674 <.0001 
# announce - acknowledge    -0.214135 0.0221 Inf  -9.707 <.0001 
# announce - hear           -0.239804 0.0221 Inf -10.870 <.0001 
# announce - discover       -0.264579 0.0220 Inf -12.001 <.0001 
# announce - see            -0.292586 0.0221 Inf -13.266 <.0001 
# announce - inform         -0.327081 0.0220 Inf -14.840 <.0001 
# announce - be_annoyed     -0.346989 0.0221 Inf -15.734 <.0001 
# announce - know           -0.359706 0.0221 Inf -16.301 <.0001 
# confess - admit           -0.098798 0.0221 Inf  -4.475 0.0014 
# confess - reveal          -0.122191 0.0221 Inf  -5.538 <.0001 
# confess - acknowledge     -0.211239 0.0221 Inf  -9.568 <.0001 
# confess - hear            -0.236907 0.0221 Inf -10.730 <.0001 
# confess - discover        -0.261682 0.0221 Inf -11.860 <.0001 
# confess - see             -0.289689 0.0221 Inf -13.124 <.0001 
# confess - inform          -0.324185 0.0221 Inf -14.697 <.0001 
# confess - be_annoyed      -0.344093 0.0221 Inf -15.591 <.0001 
# confess - know            -0.356809 0.0221 Inf -16.157 <.0001 
# admit - reveal            -0.023393 0.0220 Inf  -1.061 1.0000 
# admit - acknowledge       -0.112440 0.0221 Inf  -5.096 0.0001 
# admit - hear              -0.138109 0.0221 Inf  -6.259 <.0001 
# admit - discover          -0.162884 0.0221 Inf  -7.387 <.0001 
# admit - see               -0.190891 0.0221 Inf  -8.653 <.0001 
# admit - inform            -0.225387 0.0220 Inf -10.224 <.0001 
# admit - be_annoyed        -0.245294 0.0221 Inf -11.121 <.0001 
# admit - know              -0.258011 0.0221 Inf -11.690 <.0001 
# reveal - acknowledge      -0.089047 0.0220 Inf  -4.039 0.0091 
# reveal - hear             -0.114716 0.0220 Inf  -5.203 <.0001 
# reveal - discover         -0.139491 0.0220 Inf  -6.330 <.0001 
# reveal - see              -0.167498 0.0220 Inf  -7.598 <.0001 
# reveal - inform           -0.201993 0.0220 Inf  -9.169 <.0001 
# reveal - be_annoyed       -0.221901 0.0220 Inf -10.067 <.0001 
# reveal - know             -0.234618 0.0221 Inf -10.638 <.0001 
# acknowledge - hear        -0.025669 0.0221 Inf  -1.163 0.9999 
# acknowledge - discover    -0.050444 0.0220 Inf  -2.288 0.7659 
# acknowledge - see         -0.078451 0.0221 Inf  -3.556 0.0521 
# acknowledge - inform      -0.112946 0.0220 Inf  -5.124 0.0001 
# acknowledge - be_annoyed  -0.132854 0.0221 Inf  -6.024 <.0001 
# acknowledge - know        -0.145570 0.0221 Inf  -6.596 <.0001 
# hear - discover           -0.024775 0.0221 Inf  -1.124 0.9999 
# hear - see                -0.052782 0.0221 Inf  -2.393 0.6914 
# hear - inform             -0.087278 0.0220 Inf  -3.959 0.0124 
# hear - be_annoyed         -0.107185 0.0221 Inf  -4.859 0.0002 
# hear - know               -0.119902 0.0221 Inf  -5.433 <.0001 
# discover - see            -0.028007 0.0220 Inf  -1.270 0.9996 
# discover - inform         -0.062502 0.0220 Inf  -2.837 0.3502 
# discover - be_annoyed     -0.082410 0.0220 Inf  -3.739 0.0280 
# discover - know           -0.095127 0.0221 Inf  -4.313 0.0029 
# see - inform              -0.034496 0.0220 Inf  -1.565 0.9939 
# see - be_annoyed          -0.054403 0.0221 Inf  -2.467 0.6346 
# see - know                -0.067120 0.0221 Inf  -3.042 0.2240 
# inform - be_annoyed       -0.019908 0.0220 Inf  -0.903 1.0000 
# inform - know             -0.032624 0.0220 Inf  -1.480 0.9970 
# be_annoyed - know         -0.012717 0.0221 Inf  -0.576 1.0000 

## Bayesian models ----

head(cd)

# brms model 
cd$verb = relevel(cd$verb,ref="MC")
cd$item = as.factor(paste(cd$verb,cd$content))

model.brms.proj.b = brm(nResponse ~ verb + (1|workerid) + (1|item), data=cd, family=bernoulli())
summary(model.brms.proj.b) #did not converge

model.proj.b = glmer(nResponse ~ verb + (1+verb|workerid) + (1|item), nAGQ=0, data=cd,family = binomial)
summary(model.proj.b) # did not converge without nAGQ=0

model.proj.b = glmer(response ~ verb + (1|workerid) + (1|item), data=cd, family = binomial)
summary(model.proj.b) # did not converge
