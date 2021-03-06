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
library(brms)
theme_set(theme_bw())


# load clean data for analysis ----
cd = read.csv("../data/cd.csv")
nrow(cd) #10500

# how many participants said 'no' to which predicates?
str(cd$response)
cd %>% 
  select(workerid,response,verb) %>% 
  unique() %>% 
  group_by(verb) %>% 
  summarise(sum(response == "No")) %>%
  print(n=40)


## models -----
head(cd)

# brms model 
cd$verb = relevel(cd$verb,ref="entailing C")
cd$item = as.factor(paste(cd$verb,cd$content))
cd$workerid = as.factor(as.character(cd$workerid))

model.brms.proj.b = brm(nResponse ~ verb + (1|workerid) + (1|item), data=cd, family=bernoulli(), cores = 4, control=list(max_treedepth = 15,adapt_delta=.95)) # increase adapt_delta (from default .8) to decrease learning rate
summary(model.brms.proj.b) 
saveRDS(model.brms.proj.b, "../models/brm_model.rds")

# create LaTeX table
mcmcReg(model.brms.proj.b, pars = "b_", file="../models/brm_output.tex")

# the way to do pairwise comparisons, if we want them:
q = c(q_know_entailing = "verbknow = 0",
      q_see_entailing = "verbsee = 0",
      q_discover_entailing = "verbdiscover = 0",
      q_confirm_entailing = "verbconfirm = 0",
      q_be_annoyed_entailing = "verbbe_annoyed = 0")
q_answer = hypothesis(model.brms.proj.b, q)
q_answer
plot(q_answer)
prop.table(table(q_answer$samples$H1 < 0)) # p(know < entailing) = .69, ie, not very high
prop.table(table(q_answer$samples$H2 < 0)) # p(see < entailing) = .81, ie, high but not within the 95% credible interval
prop.table(table(q_answer$samples$H3 < 0)) # p(discover < entailing) = .81, ie, high but not within the 95% credible interval
prop.table(table(q_answer$samples$H4 < 0)) # p(confirm < entailing) = .949, ie, very high but still not within the 95% credible interval
prop.table(table(q_answer$samples$H5 < 0)) # p(be_annoyed < entailing) = .999, ie, very high, clear evidence that it's different


# to load saved model:
model.brms.proj.b = readRDS(model.brms.proj.b, "../models/brm_model.rds")