# Auxiliary analyses for lexical semantics of clause-embedding predicates

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

# Plot mean projection against mean veridicality ----

# load data

# load clean inference entailment data
d_inf_nb = read.csv("../../4-veridicality3/data/cd.csv") %>%
  filter(verb != "non-ent. C" & verb != "entailing C") %>%
  group_by(verb) %>%
  summarize(Mean_Inf = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMinM_Inf = Mean_Inf - CILow, YMaxM_Inf = Mean_Inf + CIHigh, verb = fct_reorder(as.factor(verb),Mean_Inf)) %>%
  select(-CILow,-CIHigh)
summary(d_inf_nb)

table(d_inf_nb$verb)


# load clean contradictoriness entailment
d_contr_nb = read.csv("../../2-veridicality2/data/cd.csv") %>%
  filter(verb != "noncontrd. C" & verb != "contradictory C") %>%
  group_by(verb) %>%
  summarize(Mean_Contr = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMinM_Contr = Mean_Contr - CILow, YMaxM_Contr = Mean_Contr + CIHigh, verb = fct_reorder(as.factor(verb),d_inf_nb$Mean_Inf)) %>%
  select(-CILow,-CIHigh)
summary(d_contr_nb)

# load projection data
d_proj = read.csv("../../5-projectivity-no-fact/data/cd.csv") %>%
  filter(verb != "MC") %>%
  group_by(verb) %>%
  summarize(Mean_Proj = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMinM_Proj = Mean_Proj - CILow, YMaxM_Proj = Mean_Proj + CIHigh, verb = fct_reorder(as.factor(verb),d_inf_nb$Mean_Inf)) %>%
  select(-CILow,-CIHigh)
table(d_proj$verb)

# plot inference means against projection means

tmp = d_proj %>%
  left_join(d_inf_nb)
summary(tmp)

ggplot(tmp, aes(x=Mean_Inf, y=Mean_Proj)) +
  geom_point(stroke=.5,size=2.5,color="black") +
  geom_text_repel(aes(label = verb), point.padding = 0.5,
                  segment.color = 'grey50') +
  geom_errorbarh(aes(xmin=YMinM_Inf,xmax=YMaxM_Inf),width=0) +
  geom_errorbar(aes(ymin=YMinM_Proj,ymax=YMaxM_Proj),width=0) +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  xlab("Mean veridicality rating (inference)") +
  ylab("Mean certainty rating") +
  theme(legend.position = "bottom") +
  coord_fixed(ratio = 1) +
  xlim(c(0,1)) +
  ylim(c(0,1))
ggsave("../graphs/mean-inf-against-mean-proj.pdf",height=6,width=6)

# plot contradictoriness means against projection means

tmp = d_proj %>%
  left_join(d_contr_nb)
summary(tmp)

ggplot(tmp, aes(x=Mean_Contr, y=Mean_Proj)) +
  geom_point(stroke=.5,size=2.5,color="black") +
  geom_text_repel(aes(label = verb), point.padding = 0.5,
                  segment.color = 'grey50') +
  geom_errorbarh(aes(xmin=YMinM_Contr,xmax=YMaxM_Contr),width=0) +
  geom_errorbar(aes(ymin=YMinM_Proj,ymax=YMaxM_Proj),width=0) +
  
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  xlab("Mean veridicality rating (contradictoriness)") +
  ylab("Mean certainty rating") +
  theme(legend.position = "bottom") +
  coord_fixed(ratio = 1) +
  xlim(c(0,1)) +
  ylim(c(0,1))
ggsave("../graphs/mean-contr-against-mean-proj.pdf",height=6,width=6)

  

# Does not-at-issueness predict veridicality? (aux analysis) -----
# (wrinkle: nai data from interrogatives, veridicality data from unembedded)

# load clean data for analysis ----

# load clean at-issueness data (from Glossa paper), calculate by-verb mean
d_ai = read.csv("../../../../../1factive-verbs/attitude_preds_projection/results/main/exp3/data/d.csv") %>%
  rename("verb" = "short_trigger") %>%
  filter(verb != "MC") %>%
  group_by(verb) %>%
  summarize(Mean_AI = mean(ai), CILow = ci.low(ai), CIHigh = ci.high(ai)) %>%
  mutate(YMinM_AI = Mean_AI - CILow, YMaxM_AI = Mean_AI + CIHigh) %>%
  select(-CILow,-CIHigh)
summary(d_ai)
  
names(d_ai)
table(d_ai$verb)
  
# load clean inference entailment data
d_inf_b = read.csv("../../7-veridicality3-binary/data/cd.csv") %>%
  mutate(nResponse = ifelse(response == "Yes",1,0)) %>%
  filter(verb != "non-ent. C" & verb != "entailing C") %>%
  group_by(verb) %>%
  summarize(Prop_Inf = mean(nResponse), CILow = ci.low(nResponse), CIHigh = ci.high(nResponse)) %>%
  mutate(YMinP_Inf = Prop_Inf - CILow, YMaxP_Inf = Prop_Inf + CIHigh, verb = fct_reorder(as.factor(verb),d_ai$Mean_AI)) %>%
  select(-CILow,-CIHigh)
summary(d_inf_b)
  
d_inf_nb = read.csv("../../4-veridicality3/data/cd.csv") %>%
  filter(verb != "non-ent. C" & verb != "entailing C") %>%
  group_by(verb) %>%
  summarize(Mean_Inf = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMinM_Inf = Mean_Inf - CILow, YMaxM_Inf = Mean_Inf + CIHigh, verb = fct_reorder(as.factor(verb),d_ai$Mean_AI)) %>%
  select(-CILow,-CIHigh)
summary(d_inf_nb)

table(d_inf_b$verb)

names(d_inf_nb)
table(d_inf_nb$verb)

# load clean contradictoriness entailment
d_contr_b = read.csv("../../6-veridicality2-binary/data/cd.csv") %>%
  mutate(nResponse = ifelse(response == "Yes",1,0)) %>%
  filter(verb != "noncontrd. C" & verb != "contradictory C") %>%
  group_by(verb) %>%
  summarize(Prop_Contr = mean(nResponse), CILow = ci.low(nResponse), CIHigh = ci.high(nResponse)) %>%
  mutate(YMinP_Contr = Prop_Contr - CILow, YMaxP_Contr = Prop_Contr + CIHigh, verb = fct_reorder(as.factor(verb),d_ai$Mean_AI)) %>%
  select(-CILow,-CIHigh)
summary(d_contr_b)

d_contr_nb = read.csv("../../2-veridicality2/data/cd.csv") %>%
  filter(verb != "noncontrd. C" & verb != "contradictory C") %>%
  group_by(verb) %>%
  summarize(Mean_Contr = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMinM_Contr = Mean_Contr - CILow, YMaxM_Contr = Mean_Contr + CIHigh, verb = fct_reorder(as.factor(verb),d_ai$Mean_AI)) %>%
  select(-CILow,-CIHigh)
summary(d_contr_nb)

names(d_contr_b)
names(d_contr_nb)

table(d_contr_b$verb)
table(d_contr_nb$verb)

# plot ai means against inference means

tmp = d_ai %>%
  left_join(d_inf_nb)
summary(tmp)

cor = tmp %>%
  summarize(Cor=cor(Mean_AI,Mean_Inf,method="spearman"))
cor #-.13

ggplot(tmp, aes(x=Mean_AI, y=Mean_Inf)) +
  geom_point(stroke=.5,size=2.5,color="black") +
  geom_text_repel(aes(label = verb), point.padding = 0.5,
                   segment.color = 'grey50') +
  geom_errorbarh(aes(xmin=YMinM_AI,xmax=YMaxM_AI),width=0) +
  geom_errorbar(aes(ymin=YMinM_Inf,ymax=YMaxM_Inf),width=0) +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  xlab("Mean not-at-issueness rating") +
  ylab("Mean inference rating") +
  theme(legend.position = "bottom") +
  coord_fixed(ratio = 1) +
  xlim(c(0,1)) +
  ylim(c(0,1))
ggsave("../graphs/mean-ai-against-mean-inference.pdf",height=3,width=3)

# plot ai means against contradictory means

tmp = d_ai %>%
  left_join(d_contr_nb)
summary(tmp)

cor = tmp %>%
  summarize(Cor=cor(Mean_AI,Mean_Contr,method="spearman"))
cor #-.24

ggplot(tmp, aes(x=Mean_AI, y=Mean_Contr)) +
  geom_point(stroke=.5,size=2.5,color="black") +
  geom_text_repel(aes(label = verb), point.padding = 0.5,
                   segment.color = 'grey50') +
  geom_errorbarh(aes(xmin=YMinM_AI,xmax=YMaxM_AI),width=0) +
  geom_errorbar(aes(ymin=YMinM_Contr,ymax=YMaxM_Contr),width=0) +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  xlab("Mean not-at-issueness rating") +
  ylab("Mean contradictoriness rating") +
  theme(legend.position = "bottom") +
  coord_fixed(ratio = 1) +
  xlim(c(0,1)) +
  ylim(c(0,1))
ggsave("../graphs/mean-ai-against-mean-contradictoriness.pdf",height=3,width=3)

# plot ai means against inference props

tmp = d_ai %>%
  left_join(d_inf_b)
summary(tmp)

cor = tmp %>%
  summarize(Cor=cor(Mean_AI,Prop_Inf,method="spearman"))
cor #-.09

ggplot(tmp, aes(x=Mean_AI, y=Prop_Inf)) +
  geom_point(stroke=.5,size=2.5,color="black") +
  geom_text_repel(aes(label = verb), point.padding = 0.5,
                   segment.color = 'grey50') +
  geom_errorbarh(aes(xmin=YMinM_AI,xmax=YMaxM_AI),width=0) +
  geom_errorbar(aes(ymin=YMinP_Inf,ymax=YMaxP_Inf),width=0) +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  xlab("Mean not-at-issueness rating") +
  ylab("Proportion of contradictoriness rating") +
  theme(legend.position = "bottom") +
  coord_fixed(ratio = 1) +
  xlim(c(0,1)) +
  ylim(c(0,1))
ggsave("../graphs/mean-ai-against-prop-inference.pdf",height=3,width=3)

# plot ai means against contradictory props

tmp = d_ai %>%
  left_join(d_contr_b)
summary(tmp)

cor = tmp %>%
  summarize(Cor=cor(Mean_AI,Prop_Contr,method="spearman"))
cor #-.27

ggplot(tmp, aes(x=Mean_AI, y=Prop_Contr)) +
  geom_point(stroke=.5,size=2.5,color="black") +
  geom_text_repel(aes(label = verb), point.padding = 0.5,
                   segment.color = 'grey50') +
  geom_errorbarh(aes(xmin=YMinM_AI,xmax=YMaxM_AI),width=0) +
  geom_errorbar(aes(ymin=YMinP_Contr,ymax=YMaxP_Contr),width=0) +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  xlab("Mean not-at-issueness rating") +
  ylab("Proportion of contradictoriness rating") +
  theme(legend.position = "bottom") +
  coord_fixed(ratio = 1) +
  xlim(c(0,1)) +
  ylim(c(0,1))
ggsave("../graphs/mean-ai-against-prop-contradictoriness.pdf",height=3,width=3)


