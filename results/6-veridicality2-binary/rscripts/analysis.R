# Factives paper
# 6-veridicality2-binary (contradictoriness diagnostic, binary)
# analysis.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('../../helpers.R')

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
nrow(cd) #9884 (353 Turkers)
summary(cd)


## models -----
head(cd)

# brms model 
cd$verb = relevel(cd$verb,ref="contradictory C")
cd$item = as.factor(paste(cd$verb,cd$content))
cd$workerid = as.factor(as.character(cd$workerid))

model.brms.proj.b = brm(nResponse ~ verb + (1|workerid) + (1|item), data=cd, family=bernoulli(), cores = 4, control=list(max_treedepth = 15,adapt_delta=.95)) # increase adapt_delta (from default .8) to decrease learning rate
summary(model.brms.proj.b) 
saveRDS(model.brms.proj.b, "../models/brm_model.rds")

# create LaTeX table
mcmcReg(model.brms.proj.b, pars = "b_", file="../models/brm_output.tex")

# the way to do pairwise comparisons, if we want them (but all probs of differing from contradictory control are 1):
q = c(q_know_entailing = "verbknow = 0",
      q_see_entailing = "verbsee = 0",
      q_discover_entailing = "verbdiscover = 0",
      q_confirm_entailing = "verbconfirm = 0",
      q_be_annoyed_entailing = "verbbe_annoyed = 0")
q_answer = hypothesis(model.brms.proj.b, q)
q_answer
plot(q_answer)
prop.table(table(q_answer$samples$H1 < 0)) 
prop.table(table(q_answer$samples$H2 < 0)) 
prop.table(table(q_answer$samples$H3 < 0)) 
prop.table(table(q_answer$samples$H4 < 0)) 
prop.table(table(q_answer$samples$H5 < 0)) 


# to load saved model:
model.brms.proj.b = readRDS(model.brms.proj.b, "../models/brm_model.rds")


