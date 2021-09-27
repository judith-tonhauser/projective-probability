# Factives paper
# 8-projectivity-no-fact-binary (certainty ratings, binary task)
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
library(emmeans)
library(lme4)
library(BayesPostEst)
theme_set(theme_bw())

# load clean data for analysis ----
cd = read.csv("../data/cd.csv")
nrow(cd) #11336

# how many participants said 'no' to which predicates?
str(cd$response)
cd %>% 
  select(workerid,response,verb) %>% 
  unique() %>% 
  group_by(verb) %>% 
  summarise(sum(response == "No")) %>%
  print(n=40)

# who are the workers who responded 'no' to the predicates up to demonstrate?
summary(cd)
table(cd$verb)
not.entailed <- filter(cd, (verb == "demonstrate" | verb == "confess" |verb == "reveal" |
           verb == "establish" | verb == "admit" |verb == "acknowledge" |
           verb == "be_annoyed" | verb == "confirm" | verb == "discover" | 
           verb == "know" | verb == "see" | verb == "be_right" | verb == "prove") &  response == "No")
table(not.entailed$verb)  


not.entailed <- droplevels(subset(cd,verb == "demonstrate" | verb == "confess" |verb == "reveal" |
                                    verb == "establish" | verb == "admit" |verb == "acknowledge" |
                                  verb == "be_annoyed" | verb == "confirm" | verb == "discover" | 
                                    verb == "know" | verb == "see" | verb == "be_right" | verb == "prove"))
not.entailed <- droplevels(subset(not.entailed,response == "No"))



cd %>% 
  filter(verb == "demonstrate") %>% 
  select(workerid,response,verb) %>% 
  unique() %>% 
  group_by(verb) %>% 
  summarise(sum(response == "No")) %>%
  print(n=40)

## non-Bayesian models ----
summary(cd)

# create item as combination of predicate and complement clause
# cd$item = as.factor(paste(cd$verb,cd$content))

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

model = glmer(response ~ verb + (1|workerid) + (1|content), data=cd, family = "binomial")
summary(model)
# model didn't converge
saveRDS(model, "../models/glmer_model.rds")

# to load saved model:
model = readRDS(model, "../models/glmer_model.rds")

comparison = emmeans(model, pairwise~verb,adjust="tukey")
options(max.print=10000)
comparison


## Bayesian models ----
head(cd)

# brms model 
cd$verb = relevel(cd$verb,ref="MC")
cd$item = as.factor(paste(cd$verb,cd$content))

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
model.brms.proj.b = readRDS(file="../models/brm_model.rds")

# model comparison between binary factivity and predicate-specific predictor model. to this end, classify MC controls as "non-factive"

# create factivity variable (collapse MC controls into non-factive category, to stack deck against yourself)
cd = cd %>% 
  mutate(predicate_type = as.factor(case_when(verb %in% c("know", "reveal","see","discover","be_annoyed") ~ "factive",
                                              # verb == "MC" ~ "control",
                                              TRUE ~ "non-factive")))
cd = cd %>% 
  mutate(predicate_type = fct_relevel(predicate_type,"non-factive")) %>% 
  droplevels()

model.brms.proj.binary.b = brm(nResponse ~ predicate_type + (1|workerid) + (1|item), data=cd, family=bernoulli(), cores = 4, control=list(max_treedepth = 15))
summary(model.brms.proj.binary.b) 
saveRDS(model.brms.proj.binary.b, "../models/brm_model_binary.rds")


# to load saved model:
model.brms.proj.binary.b = readRDS(file="../models/brm_model_binary.rds")

mcmcReg(model.brms.proj.binary.b, pars = "b_", file="../models/brm_output_binary.tex")

cd = cd %>% 
  mutate(predicate_type_ternary = as.factor(case_when(verb %in% c("know", "reveal","see","discover","be_annoyed") ~ "factive",
                                                      verb %in% c("think","say","pretend","suggest","MC") ~ "non-factive",
                                              # verb == "MC" ~ "control",
                                              TRUE ~ "opt-factive")))
cd = cd %>% 
  mutate(predicate_type_ternary = fct_relevel(predicate_type_ternary,"non-factive","opt-factive")) %>% 
  droplevels()

model.brms.proj.ternary.b = brm(nResponse ~ predicate_type_ternary + (1|workerid) + (1|item), data=cd, family=bernoulli(), cores = 4, control=list(max_treedepth = 15))
summary(model.brms.proj.ternary.b) 
saveRDS(model.brms.proj.ternary.b, "../models/brm_model_ternary.rds")

# model comparison
model.brms.proj.binary.b = add_criterion(model.brms.proj.binary.b, "waic")
# model comparison
model.brms.proj.ternary.b = add_criterion(model.brms.proj.ternary.b, "waic")
model.brms.proj.b = add_criterion(model.brms.proj.b, "waic")

# look at absolute waic
waic(model.brms.proj.b) # 6444.5, lower -> better
waic(model.brms.proj.ternary.b) # 6711.3 -> worse
waic(model.brms.proj.binary.b) # 6719.0, higher -> worse 

loo_compare(model.brms.proj.binary.b, model.brms.proj.b, criterion = "waic")

# to do waic calculations by hand:
# ll = model.brms.proj.b %>% 
#   log_lik() %>% 
#   as_tibble()
# 
# dfmean = ll %>% 
#   exp() %>% 
#   summarise_all(mean) %>% 
#   gather(key,means) %>% 
#   select(means) %>% 
#   log()
# 
# lppd = dfmean %>% 
#   sum()
# 
# dfvar = ll %>% 
#   summarise_all(var) %>% 
#   gather(key, vars) %>% 
#   select(vars)
# 
# pwaic = dfvar %>% 
#   sum()
# 
# pwaic
# -2 * (lppd -pwaic)