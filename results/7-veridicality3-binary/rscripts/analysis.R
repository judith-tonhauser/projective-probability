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

model.brms.proj.b = brm(nResponse ~ verb + (1|workerid) + (1|item), data=cd, family=bernoulli(), cores = 4, control=list(max_treedepth = 15))
summary(model.brms.proj.b) 
saveRDS(model.brms.proj.b, "../models/brm_model.rds")

mcmcReg(model.brms.proj.b, pars = "b_", file="../models/brm_output.tex")

# the way to do pairwise comparisons, if we want them:
q = c(q_think_be_right = "verbthink - verbbe_right = 0",
      q_pretend_be_right = "verbpretend - verbbe_right = 0",
      q_prove_be_right = "verbprove - verbbe_right = 0",
      q_know_be_right = "verbknow - verbbe_right = 0",
      q_be_right_MC = "verbbe_right = 0")
q_answer = hypothesis(model.brms.proj.b, q)
q_answer
plot(q_answer)
prop.table(table(q_answer$samples$H1 > 0)) # p(think > be_right = .72, very low)
prop.table(table(q_answer$samples$H2 > 0)) # p(prove > be_right = .997, very high)
prop.table(table(q_answer$samples$H3 > 0)) # p(pretend > be_right = 1, very high)
prop.table(table(q_answer$samples$H4 > 0)) # p(know > be_right = 1, very high)
prop.table(table(q_answer$samples$H5 > 0))

# to load saved model:
model.brms.proj.b = readRDS(model.brms.proj.b, "../models/brm_model.rds")