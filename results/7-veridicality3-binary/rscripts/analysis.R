# Factives paper
# 7-veridicality3-binary (inference diagnostic, binary)
# analysis.R

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
nrow(cd) #9968

# how many participants said 'no' to which predicates?
str(cd$response)
cd %>% 
  select(workerid,response,verb) %>% 
  unique() %>% 
  group_by(verb) %>% 
  summarise(sum(response == "No")) %>%
  print(n=40)


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

