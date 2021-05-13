# Prior paper Exp 2b
# Projection experiment with sensitivity to fact
# analysis.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(lme4)
library(lmerTest)

# load clean data for analysis 
cd = read.csv("../data/cd.csv")
nrow(cd) #6916
names(cd)

# load prior means from Exp 2a
pmeans = read.csv("../../1-prior/data/prior_means.csv")
pmeans$content = pmeans$event
pmeans$fact_type = pmeans$itemType
head(pmeans)
table(pmeans$fact_type) #H, L

# change fact_type, predicate names, get rid of MCs

table(cd$fact_type) #factH, factL
str(cd$fact_type)

table(cd$verb)
str(cd$verb)

cd = cd %>%
  mutate(verb=dplyr::recode(verb, annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform")) %>% 
  filter(verb != "control") %>% 
  mutate(fact_type=dplyr::recode(fact_type, factH = "H", factL = "L")) %>%
  droplevels()

nrow(cd) #5320 target items
table(cd$fact_type) #H, L

str(cd$fact_type)
str(pmeans$fact_type)

# what's the min and max number of unique predicate/content combinations?
table(cd$verb,cd$content)
min(table(cd$verb,cd$content)) # 4
max(table(cd$verb,cd$content)) # 27
mean(table(cd$verb,cd$content)) #13.3

# add prior means to dataset
cd = cd %>% 
  left_join(pmeans,by=c("content","fact_type"))
summary(cd)

# define items
cd$item = as.factor(paste(cd$verb,cd$content))

# set lower probability fact as reference level of fact_type
cd$fact_type = as.factor(as.character(cd$fact_type))
contrasts(cd$fact_type) = c(1,0)

# analysis 1a ----
# does high/low prob fact predict projection ratings?
m.proj.cat = lmer(response ~ fact_type + (1+fact_type|item) + (1+fact_type|workerid), data=cd, REML=F)
summary(m.proj.cat)
# fact_type1    0.17869    0.01395 283.24370   12.81   <2e-16 ***

# analysis 1b ----
# does prior mean predict projection ratings?
m.proj = lmer(response ~ Mean + (1+Mean|item) + (1+Mean|workerid), data=cd, REML=F)
summary(m.proj)
# Mean          0.33658    0.02536 265.60585   13.27   <2e-16 ***

# the BIC of the categorical model is higher than that of the group-level means model
BIC(m.proj.cat)
BIC(m.proj) 

# footnote 5: analyses with 28 Turkers excluded who also took Exp 2a ----
exclude = read.csv("../data/excluded.assid.csv")
nrow(exclude) #28
#View(exclude)

str(cd$item)

cd = cd %>%
  filter(!assignmentid %in% exclude$assignmentid)
length(unique(cd$workerid)) #241 (25 Turkers excluded, 3 must have already been excluded in preprocessing)

# set lower probability fact as reference level of fact_type
cd$fact_type = as.factor(as.character(cd$fact_type))
contrasts(cd$fact_type) = c(1,0)

# analysis 1a: does high/low prob fact predict projection ratings?
m.proj.cat = lmer(response ~ fact_type + (1+fact_type|item) + (1+fact_type|workerid), data=cd, REML=F)
summary(m.proj.cat)
# fact_type1    0.1729     0.0142 258.3665   12.17   <2e-16 ***

# analysis 1b: does prior mean predict projection ratings?
m.proj = lmer(response ~ Mean + (1+Mean|item) + (1+Mean|workerid), data=cd, REML=F)
summary(m.proj)
# Mean          0.32512    0.02594 244.65971   12.53   <2e-16 ***

# the BIC of the categorical model is still higher than that of the group-level means model
BIC(m.proj.cat)
BIC(m.proj) 

