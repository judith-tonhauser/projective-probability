# Factives paper
# 6-veridicality2-binary (contradictoriness diagnostic, binary)
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
library(brms)
theme_set(theme_bw())

# load clean data for analysis ----
cd = read.csv("../data/cd.csv")
nrow(cd) #10808 (386 Turkers)
summary(cd)


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
