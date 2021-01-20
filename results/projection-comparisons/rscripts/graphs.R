# Prior paper
# compare certainty ratings across experiments

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
library(cowplot)
theme_set(theme_bw())

# load projection data for analysis ----

# Exp 2b in prior paper 
d_proj3 = read.csv("../../3-projectivity/data/cd.csv") %>%
  mutate(verb=recode(verb, control = "MC", annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform"))
names(d_proj3)

# Exp 1a in factives paper (TD)
d_proj5 = read.csv("../../5-projectivity-no-fact/data/cd.csv") %>%
  mutate(verb=recode(verb, control = "MC", annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform"))

# Exp 1 in prior paper
d_proj9 = read.csv("../../9-prior-projection/data/cd.csv") %>%
  rename(verb = short_trigger)
names(d_proj9)

# sanity check
nrow(d_proj3) #6916 / 26 = 266 participants
nrow(d_proj5) #6916 / 26 = 266 participants (yes, same number of participants!)
nrow(d_proj9) #14872 / 52 (20+20+6+6) = 286 participants

# calculate projection means ----

str(d_proj3$verb)
str(d_proj5$verb)
str(d_proj9$verb)

p_means3 = d_proj3 %>%
  group_by(verb) %>%
  summarize(Mean_3 = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMinM_3 = Mean_3 - CILow, YMaxM_3 = Mean_3 + CIHigh, verb = fct_reorder(as.factor(verb),Mean_3)) %>%
  select(-CILow,-CIHigh)
levels(p_means3$verb)
str(p_means3$verb)
View(p_means3) #.18-.62

p_means5 = d_proj5 %>%
  group_by(verb) %>%
  summarize(Mean_5 = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMinM_5 = Mean_5 - CILow, YMaxM_5 = Mean_5 + CIHigh, verb = fct_reorder(as.factor(verb),p_means3$Mean_3)) %>%
  select(-CILow,-CIHigh)
levels(p_means5$verb)
str(p_means5$verb)
View(p_means5) #.11-.88

p_means9 = d_proj9 %>%
  group_by(verb) %>%
  summarize(Mean_9 = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMinM_9 = Mean_9 - CILow, YMaxM_9 = Mean_9 + CIHigh, verb = fct_reorder(as.factor(verb),p_means3$Mean_3)) %>%
  select(-CILow,-CIHigh)
levels(p_means9$verb)
str(p_means9$verb)
View(p_means9) #.35-.60

# bind the data ----
p_means_35 = p_means3 %>%
  left_join(p_means5,by="verb")
View(p_means_35)

p_means_39 = p_means3 %>%
  left_join(p_means9,by="verb")
View(p_means_39)

p_means_59 = p_means5 %>%
  left_join(p_means9,by="verb")

# certainty ratings
# prior paper Exp 1: p_means9
# prior paper Exp 2b: p_means3
# factives paper Exp 1a: p_means5


## plot: Exp 2b (p_means3) against Exp 1a in TD (p_means5) ----

plot <- ggplot(p_means_35, aes(x=Mean_3, y=Mean_5)) +
  geom_errorbarh(aes(xmin=YMinM_3,xmax=YMaxM_3),width=0,color="blue") +
  geom_errorbar(aes(ymin=YMinM_5,ymax=YMaxM_5),width=0,color="blue") +
  geom_point(stroke=.5,size=2.5,color="blue") +
  geom_text_repel(data=p_means_35,aes(x=Mean_3,y=Mean_5,label=verb),size = 3,segment.size=1,segment.color="black",segment.alpha=.2,nudge_x=.4,nudge_y=-.1,force=1) +
  # scale_alpha(range = c(.3,1)) +
  #scale_fill_manual(values=c("darkorchid","black","gray60","tomato1","dodgerblue")) +
  #scale_shape_manual(values=rev(c(23, 24, 25, 22, 21)), labels=rev(c("factive","optionally\nfactive","veridical\nnon-factive","non-veridical\nnon-factive","controls")),name="Predicate type") +
  #scale_fill_manual(values=rev(c("darkorchid","tomato1","dodgerblue","gray60","black")), labels=rev(c("factive","optionally\nfactive","veridical\nnon-factive","non-veridical\nnon-factive","controls")),name="Predicate type") +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  ylab("Mean certainty rating (Exp 2b)") +
  xlab("Mean certainty rating (Exp 1a TD)") +
  theme(legend.position = "bottom") +
  coord_fixed(ratio = 1) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  scale_x_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1"))
  #xlim(c(0,1)) +
  #ylim(c(0,1))
plot
ggsave("../graphs/projection-comparison-35.pdf",height=3,width=3)

corr_projectivity = p_means_35 %>%
  summarize(Cor=cor(Mean_3,Mean_5,method="spearman"))
corr_projectivity #.986

## plot: Exp 2b (p_means3) against Exp 1 (p_means9) ----

plot <- ggplot(p_means_39, aes(x=Mean_9, y=Mean_3)) +
  geom_errorbarh(aes(xmin=YMinM_9,xmax=YMaxM_9),width=0,color="blue") +
  geom_errorbar(aes(ymin=YMinM_3,ymax=YMaxM_3),width=0,color="blue") +
  geom_point(stroke=.5,size=2.5,color="blue") +
  geom_text_repel(data=p_means_39,aes(x=Mean_9,y=Mean_3,label=verb),size = 3,segment.size=1,segment.color="black",segment.alpha=.2,nudge_x=.4,nudge_y=-.1,force=1) +
  # scale_alpha(range = c(.3,1)) +
  #scale_fill_manual(values=c("darkorchid","black","gray60","tomato1","dodgerblue")) +
  #scale_shape_manual(values=rev(c(23, 24, 25, 22, 21)), labels=rev(c("factive","optionally\nfactive","veridical\nnon-factive","non-veridical\nnon-factive","controls")),name="Predicate type") +
  #scale_fill_manual(values=rev(c("darkorchid","tomato1","dodgerblue","gray60","black")), labels=rev(c("factive","optionally\nfactive","veridical\nnon-factive","non-veridical\nnon-factive","controls")),name="Predicate type") +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  ylab("Mean certainty rating (Exp 1)") +
  xlab("Mean certainty rating (Exp 2b)") +
  theme(legend.position = "bottom") +
  coord_fixed(ratio = 1) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  scale_x_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1"))
  #xlim(c(0,1)) +
  #ylim(c(0,1))
plot
ggsave("../graphs/projection-comparison-39.pdf",height=3,width=3)

corr_projectivity = p_means_39 %>%
  summarize(Cor=cor(Mean_3,Mean_9,method="spearman"))
corr_projectivity #.971

## plot: Exp 1 (p_means9) against Exp 1a in TD (p_means5) ----

plot <- ggplot(p_means_59, aes(x=Mean_9, y=Mean_5)) +
  geom_errorbarh(aes(xmin=YMinM_9,xmax=YMaxM_9),width=0,color="blue") +
  geom_errorbar(aes(ymin=YMinM_5,ymax=YMaxM_5),width=0,color="blue") +
  geom_point(stroke=.5,size=2.5,color="blue") +
  geom_text_repel(data=p_means_59,aes(x=Mean_9,y=Mean_5,label=verb),size = 3,segment.size=1,segment.color="black",segment.alpha=.2,nudge_x=.4,nudge_y=-.1,force=1) +
  # scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  # scale_alpha(range = c(.3,1)) +
  #scale_fill_manual(values=c("darkorchid","black","gray60","tomato1","dodgerblue")) +
  #scale_shape_manual(values=rev(c(23, 24, 25, 22, 21)), labels=rev(c("factive","optionally\nfactive","veridical\nnon-factive","non-veridical\nnon-factive","controls")),name="Predicate type") +
  #scale_fill_manual(values=rev(c("darkorchid","tomato1","dodgerblue","gray60","black")), labels=rev(c("factive","optionally\nfactive","veridical\nnon-factive","non-veridical\nnon-factive","controls")),name="Predicate type") +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  ylab("Mean certainty rating (Exp 1 TD)") +
  xlab("Mean certainty rating (Exp 1)") +
  theme(legend.position = "bottom") +
  coord_fixed(ratio = 1) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  scale_x_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) 
  #xlim(c(0,1)) +
  #ylim(c(0,1))
plot
ggsave("../graphs/projection-comparison-59.pdf",height=3,width=3)

corr_projectivity = p_means_59 %>%
  summarize(Cor=cor(Mean_9,Mean_5,method="spearman"))
corr_projectivity #.974



