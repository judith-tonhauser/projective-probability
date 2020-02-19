# Factives paper
# 7-veridicality3-binary (inference diagnostic, binary)
# graphs.R

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


# load clean data for analysis ----
cd = read.csv("../data/cd.csv")
nrow(cd) #10500

# plots ----

# recode control labels to be more readable
cd = cd %>%
  mutate(verb = fct_recode(verb,"non-entailing"="non-ent. C","entailing"="entailing C"))

# mean projectivity by predicate, including the main clause control
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
                       ifelse(cols$V %in% c("entailing", "non-entailing"),"MC","V")))))

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
                       ifelse(prop$verb  %in% c("entailing", "non-entailing"),"MC","V")))))

prop = prop %>%
  mutate(VeridicalityGroup = fct_relevel(VeridicalityGroup, "MC","NF","VNF","V","F"))

# to handle jitter:
cd = cd %>%
  mutate(jittery = case_when(nResponse == 1 ~ .8,
                             nResponse == 0 ~ .2))

ggplot(prop, aes(x=verb, y=Mean, fill=VeridicalityGroup, shape=VeridicalityGroup)) +
  geom_point(stroke=.5,size=2.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  geom_jitter(data=cd,aes(y=jittery),shape=1,color="gray40",alpha=.2,fill="black",height=.2,width=.3) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,color="black") +
  geom_point(stroke=.5,size=2.5,color="black") +
  scale_shape_manual(values=c(21,22,25,24,23),labels=c("control","non-veridical\nnon-factive","veridical\nnon-factive","optionally\nfactive","factive"),name="Predicate type") +
  scale_fill_manual(values=c("black","gray60","dodgerblue","tomato1","darkorchid"),labels=c("control","non-veridical\nnon-factive","veridical\nnon-factive","optionally\nfactive","factive"),name="Predicate type") +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
  ylab("Proportion of 'yes (def. follows)' ratings") +
  xlab("Predicate") 
ggsave("../graphs/proportion-by-predicate-variability-individual.pdf",height=4.5,width=7)


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
# TL;DR: all verbs are different from bad control
cd <- read.csv(file="../data/cd.csv", header=TRUE, sep=",")

# Bayesian mixed effects regression to test whether ratings differ by predicate from good control
cd$workerid = as.factor(as.character(cd$workerid))

# plotting slider ratings suggests we should use a zoib model
ggplot(cd, aes(x=response)) +
  geom_histogram(stat="count")

# relevel verb so you're not taking entailing control as reference level, because that won't converge (no variance). instead, take "most entailing" predicate as determined by proportion of "Yes" -- "prove"
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

