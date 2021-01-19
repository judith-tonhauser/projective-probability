# experiment investigating prior and projection
# contents of complements of 20 predicates
# analysis.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
library(tidyverse)
library(lme4)
library(lmerTest)

# load helper functions
source('helpers.R')

# load data
d = read_csv("../data/cd.csv")
nrow(d) #1650 = 22 items x 75 participants
names(d)

table(d$prompt)

# prompt is item, change workerid to factor
d$item = as.factor(d$prompt)
d$workerid = as.factor(as.character(d$workerid))

table(d$itemType)

# exclude fillers
d_nomc = droplevels(subset(d, itemType != "F")) %>% 
  mutate(prior_type = as.factor(as.character(d_nomc$itemType)))
nrow(d_nomc) #1500/20 trials = 75 participants

# set lower probability fact as reference level of prior_type
contrasts(d_nomc$prior_type) = c(1,0)

# analysis: does high/low prob fact predict actual prior ratings?
m.prior = lmer(response ~ prior_type + (1+prior_type|item) + (1+prior_type|workerid), data=d_nomc, REML=F)
summary(m.prior)
