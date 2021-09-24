# Factives paper
# 8-projectivity-no-fact-binary (certainty ratings, categorical task)
# mixture.R

# for inspiration, used tutorial from, among other places: 
# https://pages.mtu.edu/~shanem/psy5220/daily/Day19/modelbasedclustering.html

# We attempt to find clusters of predicates by treating a participant's response vector as a mixture of binomials. The optimal number of clusters is 3, which don't cleanly separate the a priori proposed factive, non-factive, and optionally factive classes, but trend towards the a priori classification.

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
require(tidyverse)
library(flexmix)
theme_set(theme_bw())

set.seed(123)
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 

# load clean data  ----
cd = read.csv("../data/cd.csv") %>% 
  mutate(verb=as.factor(verb))


## BIG LOOP THAT DOES GAUSSIAN AND BETA MIXTURE MODEL ANALYSIS IN ONE
wide = cd %>% 
  filter(verb != "MC") %>% 
  droplevels() %>% 
  select(workerid,verb,nResponse) %>% 
  pivot_wider(names_from=c("workerid"),values_from=c("nResponse"))
tomodel = as.matrix(wide[, -(1:1)])

# fit model with two components (do we retrieve factive v non-factive groups?)
model2 <- flexmix(tomodel ~ 1, k = 2, model = FLXMCmvbinary())
results = data.frame(verb=wide$verb,cluster=clusters(model2)) %>% 
  arrange(cluster)

# verb cluster
# 1        prove       1
# 2      suggest       1
# 3      pretend       1
# 4        think       1
# 5      confirm       1
# 6     be_right       1
# 7  demonstrate       1
# 8    establish       1
# 9          say       1
# 10    announce       1
# 11 acknowledge       2
# 12         see       2
# 13        know       2
# 14       admit       2
# 15    discover       2
# 16      reveal       2
# 17        hear       2
# 18     confess       2
# 19  be_annoyed       2
# 20      inform       2

models <- stepFlexmix(tomodel ~ 1, k = 1:20, model = FLXMCmvbinary(), nrep = 100, drop = FALSE)
plot(models)

bestmodel <- getModel(models, "AIC")
bestmodel
summary(bestmodel)
clusters(bestmodel)
parameters(bestmodel)

results = data.frame(verb=wide$verb,cluster=clusters(bestmodel))

results %>% 
  arrange(cluster)

# verb cluster
# 1        prove       1
# 2      suggest       1
# 3      pretend       1
# 4        think       1
# 5      confirm       1
# 6     be_right       1
# 7  demonstrate       1
# 8    establish       1
# 9          say       1
# 10       admit       2
# 11      reveal       2
# 12     confess       2
# 13    announce       2
# 14 acknowledge       3
# 15         see       3
# 16        know       3
# 17    discover       3
# 18        hear       3
# 19  be_annoyed       3
# 20      inform       3

plot(bestmodel)

matplot(parameters(bestmodel), type = "o", main = "Projection prob by cluster of predicates for each participant")
