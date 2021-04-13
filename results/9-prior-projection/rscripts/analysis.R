# Prior paper Exp 1 (prior and projection, within-participant design)
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
d = read.csv("../data/cd.csv")
nrow(d) #7436
names(d)

d$item = as.factor(paste(d$short_trigger,d$content))
d$workerid = as.factor(as.character(d$workerid))

# exclude main clause controls
d_nomc = droplevels(subset(d, short_trigger != "MC"))
nrow(d_nomc) #5720 / 286 turkers = 20 target stimuli per Turker per block

# what's the min and max number of unique predicate/content combinations?
min(table(d_nomc$short_trigger,d_nomc$content)) # 5
max(table(d_nomc$short_trigger,d_nomc$content)) # 25


# set lower probability fact as reference level of prior_type
contrasts(d_nomc$prior_type) = c(1,0)

# analysis 1: prior ----
# does high/low prob fact predict actual prior ratings? 
m.prior = lmer(prior ~ prior_type + (1+prior_type|item) + (1+prior_type|workerid), data=d_nomc, REML=F)
summary(m.prior)
# prior_type1   0.45166    0.01451 376.16696   31.12   <2e-16 ***

# analysis 1a: does block order predict prior ratings beyond high/low prob fact?
# center fixed effects predictors first to reduce collinearity
d_nomc = cbind(d_nomc,myCenter(d_nomc[,c("prior_type","block_proj")]))

m.prior.block = lmer(prior ~ cprior_type*cblock_proj + (1+cprior_type+cblock_proj|item) + (1+cprior_type|workerid), data=d_nomc, REML=F)
summary(m.prior.block)
# there was a main effect but no interaction of block order, such that if the prior block occurred second, ratings were lower (by .03 on average, ie negligible):
# Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)               0.46072    0.00597 402.59268  77.172  < 2e-16 ***
# cprior_type              -0.45170    0.01450 376.28553 -31.159  < 2e-16 ***
# cblock_proj              -0.03278    0.01055 278.24978  -3.108  0.00208
# cprior_type:cblock_proj  -0.03060    0.02666 285.66661  -1.147  0.25214    


# analysis 2: projection (categorical priors) ----
# does high/low prob fact predict projection ratings?
m.proj = lmer(projective ~ prior_type + (1|item) + (1+prior_type|workerid), data=d_nomc, REML=F)
summary(m.proj)
# prior_type1   0.13666    0.01116 286.55761   12.24   <2e-16 ***
ranef(m.proj)

# analysis 2a: does block order predict projection ratings beyond high/low prob fact?

# do not rerun this code, if already run for analysis 1a
# center fixed effects predictors first to reduce collinearity
# d_nomc = cbind(d_nomc,myCenter(d_nomc[,c("prior_type","block_proj")]))

m.proj.block = lmer(projective ~ cprior_type*cblock_proj + (1+cblock_proj|item) + (1+cprior_type|workerid), data=d_nomc, REML=F)
summary(m.proj.block)
# no effect of block order
# Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)               0.470775   0.011252 551.065309  41.839   <2e-16 ***
# cprior_type              -0.136584   0.011155 286.653961 -12.244   <2e-16 ***
# cblock_proj               0.022431   0.016179 287.263276   1.386    0.167    
# cprior_type:cblock_proj  -0.005968   0.022352 284.088611  -0.267    0.790  

# analysis 2b: does the prior effect hold independently of predicate?
m.proj.pred = lmer(projective ~ cprior_type*short_trigger + (1|content) + (1+cprior_type|workerid), data=d_nomc, REML=F)
summary(m.proj.pred)
# answer: yes! lots of main effects of predicate, but no significant interactions with prior type (except for marginal interaction for know, p < .1, but not to be taken seriously)

# analysis 3: projection (group level priors) ----
# does group level prior rating predict projection?
summary(d_nomc)

priormeans = d_nomc %>% 
  group_by(content, prior_type) %>% 
  summarise(prior_mean = mean(prior))
nrow(priormeans) # 40 means, sanity check

d_nomc = d_nomc %>% 
  left_join(priormeans, by=c("content","prior_type"))

m.proj.group = lmer(projective ~ prior_mean + (1|item) + (1+prior_mean|workerid), data=d_nomc, REML=F)
summary(m.proj.group)
# prior_mean    0.30507    0.02424 298.33374   12.58   <2e-16 ***

# analysis 4: projection (individual prior ratings) ----
# does individual prior rating predict projection, 
# and does it do so better than categorical high/low prior predictor and group prior mean?
m.proj.ind = lmer(projective ~ prior + (1|item) + (1+prior|workerid), data=d_nomc, REML=F)
summary(m.proj.ind)
# prior         0.27546    0.01988 298.66260   13.85   <2e-16 ***
  
summary(m.proj.group)
summary(m.proj)

BIC(m.proj.ind) #2290.507
BIC(m.proj.group) #2585.635
BIC(m.proj) #2653.924

m.proj.ind.plus = lmer(projective ~ prior + prior_type + (1|item) + (1+prior|workerid), data=d_nomc, REML=F)
summary(m.proj.ind.plus)

anova(m.proj.ind,m.proj.ind.plus)
anova(m.proj,m.proj.ind.plus)
# both the BIC comparison and the likelihood ratio comparison indicate that 
# the individual-level prior model is better than the population-level or categorical one

