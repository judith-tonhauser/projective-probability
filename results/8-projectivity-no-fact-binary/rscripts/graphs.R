# Factives paper
# 8-projectivity-no-fact-binary (certainty ratings, binary task)
# graphs.R

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
nrow(cd) #11336


# mean projectivity by predicate, including the main clause controls
prop = cd %>%
  group_by(verb) %>%
  summarize(Mean = mean(nResponse), CILow = ci.low(nResponse), CIHigh = ci.high(nResponse)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(verb),Mean))
prop
levels(prop$verb)

# define colors for the predicates
cols = data.frame(V=levels(prop$verb))

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate"),"VNF",
                       ifelse(cols$V %in% c("MC"),"MC","V")))))

levels(cols$V)
cols$V <- factor(cols$V, levels = cols[order(as.character(prop$verb)),]$V, ordered = TRUE)

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "MC","black","tomato1"))))

cols$Colors

# plot of proportions

prop$VeridicalityGroup = as.factor(
  ifelse(prop$verb %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(prop$verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(prop$verb  %in% c("be_right","demonstrate"),"VNF",
                       ifelse(prop$verb  %in% c("MC"),"MC","V")))))

prop = prop %>%
  mutate(VeridicalityGroup = fct_relevel(VeridicalityGroup, "MC","NF","VNF","V","F"))
levels(prop$VeridicalityGroup)
# "F"   "MC"  "NF"  "V"   "VNF"

# factive: 23 (raute)
# optionally factive: 24 (triangle up)
# veridical non-factive: 25 (triangle down)
# non-veridical non-factive: 22 (square)
# MC: 21 (circle)

# NF, V, VNF, MC, F
# 25, 24, 22, 21, 23
# "gray60","tomato1","dodgerblue","black",darkorchid"

# to handle jitter:
cd = cd %>%
  mutate(jittery = case_when(nResponse == 1 ~ .8,
                             nResponse == 0 ~ .2))

ggplot(prop, aes(x=verb, y=Mean, fill=VeridicalityGroup, shape=VeridicalityGroup)) +
  # geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,color="black") +
  geom_point(stroke=.5,size=2.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  geom_jitter(data=cd,aes(y=jittery),shape=1,color="gray40",alpha=.2,fill="black",height=.2,width=.3) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,color="black") +
  geom_point(stroke=.5,size=2.5,color="black") +
  scale_shape_manual(values=c(21,25,22,24,23),labels=c("main clause\ncontrols","non-veridical\nnon-factive","veridical\nnon-factive","optionally\nfactive","factive"),name="Predicate type") +
  scale_fill_manual(values=c("black","gray60","dodgerblue","tomato1","darkorchid"),labels=c("main clause\ncontrols","non-veridical\nnon-factive","veridical\nnon-factive","optionally\nfactive","factive"),name="Predicate type") +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position="bottom") +
  #theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
  ylab("Proportion of 'yes (certain)' ratings") +
  xlab("Predicate") 
ggsave("../graphs/proportion-by-predicate-variability.pdf",height=4.5,width=7)
