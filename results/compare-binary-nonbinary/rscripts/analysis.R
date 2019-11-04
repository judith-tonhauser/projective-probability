# Prior probability work
# compare binary and gradient

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

# load clean projectivity data for analysis
d_proj_b = read.csv("../../8-projectivity-no-fact-binary/data/cd.csv")
d_proj_nb = read.csv("../../5-projectivity-no-fact/data/cd.csv") %>%
  mutate(verb=recode(verb, control = "MC", annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform"))

# load clean inference entailment data for analysis
d_inf_b = read.csv("../../7-veridicality3-binary/data/cd.csv") %>%
  mutate(nResponse = ifelse(response == "Yes",1,0))
d_inf_nb = read.csv("../../4-veridicality3/data/cd.csv")
table(d_inf_nb$verb)

# load clean contradictoriness entailment data for analysis
d_contr_b = read.csv("../../6-veridicality2-binary/data/cd.csv") %>%
  mutate(nResponse = ifelse(response == "Yes",1,0))
d_contr_nb = read.csv("../../2-veridicality2/data/cd.csv")
table(d_contr_nb$verb)

# compare the two non-binary entailment diagnostics
einf_means = d_inf_nb %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMinM = Mean - CILow, YMaxM = Mean + CIHigh) %>%
  mutate(verb=recode(verb, control_bad = "non-ent. C", control_good = "entailing C", annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform")) %>%
  select(-CILow,-CIHigh)
einf_means 

einf_means2 = d_contr_nb %>%
  group_by(verb) %>%
  summarize(Mean2 = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMinM2 = Mean2 - CILow, YMaxM2 = Mean2 + CIHigh) %>%
  mutate(verb=recode(verb, control_bad = "non-ent. C", control_good = "entailing C", annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform")) %>%
  select(-CILow,-CIHigh)
einf_means2 

e_inf_contr = einf_means %>%
  left_join(einf_means2, by = "verb") %>%
  mutate(VeridicalityGroup = factor(case_when(
    verb %in% c("know", "discover", "reveal", "see", "be_annoyed") ~ "F", 
    verb %in% c("pretend", "think", "suggest", "say") ~ "NF", 
    verb %in% c("be_right","demonstrate") ~ "VNF",
    verb %in% c("entailing C","non-ent. C") ~ "control",
    TRUE ~ "V")))
e_inf_contr
levels(e_inf_contr$VeridicalityGroup)

ggplot(e_inf_contr, aes(x=Mean, y=Mean2, fill=VeridicalityGroup)) +
  geom_errorbar(aes(ymin=YMinM,ymax=YMaxM),width=0) +
  geom_errorbarh(aes(xmin=YMinM2,xmax=YMaxM2),width=0) +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  # scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  # scale_alpha(range = c(.3,1)) +
  scale_fill_manual(values=c("black","darkorchid","gray60","tomato1","dodgerblue")) +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  # guides(fill=FALSE) +
  # theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
  # color=cols$Colors)) +
  # theme(legend.position="top") +
  ylab("Proportion of 'yes (def. follows)' ratings") +
  xlab("Mean inference rating") +
  guides(fill=FALSE) +
  coord_fixed(ratio = 1) +
  xlim(c(0,1)) +
  ylim(c(0,1))
ggsave("../graphs/entailment-inference.pdf",height=3,width=3)


corr_inf_contr = e_inf_contr %>%
  filter(VeridicalityGroup != "control") %>%
  summarize(Cor=cor(Mean,Mean2,method="spearman"))
corr_inf_contr

# projectivity ----

# for projectivity data, plot proportions against mean slider ratings
p_prop = d_proj_b %>%
  group_by(verb) %>%
  summarize(Prop = mean(nResponse), CILow = ci.low(nResponse), CIHigh = ci.high(nResponse)) %>%
  mutate(YMinP = Prop - CILow, YMaxP = Prop + CIHigh, verb = fct_reorder(as.factor(verb),Prop))

p_means = d_proj_nb %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMinM = Mean - CILow, YMaxM = Mean + CIHigh) %>%
  select(-CILow,-CIHigh)
levels(p_means$verb)

pd = p_prop %>%
  left_join(p_means) %>%
  mutate(VeridicalityGroup = factor(case_when(
    verb %in% c("know", "discover", "reveal", "see", "be_annoyed") ~ "F", 
    verb %in% c("pretend", "think", "suggest", "say") ~ "NF", 
    verb %in% c("be_right","demonstrate") ~ "VNF",
    verb %in% c("MC") ~ "MC",
    TRUE ~ "V")))

ggplot(pd, aes(x=Mean, y=Prop, fill=VeridicalityGroup)) +
  geom_errorbar(aes(ymin=YMinP,ymax=YMaxP),width=0) +
  geom_errorbarh(aes(xmin=YMinM,xmax=YMaxM),width=0) +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  # scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  # scale_alpha(range = c(.3,1)) +
  scale_fill_manual(values=c("darkorchid","black","gray60","tomato1","dodgerblue")) +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  ylab("Proportion of 'yes (certain)' ratings") +
  xlab("Mean certainty rating") +
  guides(fill=FALSE) +
  coord_fixed(ratio = 1) +
  xlim(c(0,1)) +
  ylim(c(0,1))
ggsave("../graphs/projectivity.pdf",height=3,width=3)

corr_projectivity = pd %>%
  filter(verb != "MC") %>%
  summarize(Cor=cor(Prop,Mean,method="spearman"))
corr_projectivity

# inference diagnostic ----

# for inference entailment data, plot proportions against mean slider ratings
einf_prop = d_inf_b %>%
  group_by(verb) %>%
  summarize(Prop = mean(nResponse), CILow = ci.low(nResponse), CIHigh = ci.high(nResponse)) %>%
  mutate(YMinP = Prop - CILow, YMaxP = Prop + CIHigh, verb = fct_reorder(as.factor(verb),Prop))
einf_prop

einf_means = d_inf_nb %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMinM = Mean - CILow, YMaxM = Mean + CIHigh) %>%
  mutate(verb=recode(verb, control_bad = "non-ent. C", control_good = "entailing C", annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform")) %>%
  select(-CILow,-CIHigh)
einf_means 

einfd = einf_prop %>%
  left_join(einf_means, by = "verb") %>%
  mutate(VeridicalityGroup = factor(case_when(
    verb %in% c("know", "discover", "reveal", "see", "be_annoyed") ~ "F", 
    verb %in% c("pretend", "think", "suggest", "say") ~ "NF", 
    verb %in% c("be_right","demonstrate") ~ "VNF",
    verb %in% c("entailing C","non-ent. C") ~ "control",
    TRUE ~ "V")))
einfd
levels(einfd$VeridicalityGroup)

ggplot(einfd, aes(x=Mean, y=Prop, fill=VeridicalityGroup)) +
  geom_errorbar(aes(ymin=YMinP,ymax=YMaxP),width=0) +
  geom_errorbarh(aes(xmin=YMinM,xmax=YMaxM),width=0) +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  # scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  # scale_alpha(range = c(.3,1)) +
  scale_fill_manual(values=c("black","darkorchid","gray60","tomato1","dodgerblue")) +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  # guides(fill=FALSE) +
  # theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
  # color=cols$Colors)) +
  # theme(legend.position="top") +
  ylab("Proportion of 'yes (def. follows)' ratings") +
  xlab("Mean inference rating") +
  guides(fill=FALSE) +
  coord_fixed(ratio = 1) +
  xlim(c(0,1)) +
  ylim(c(0,1))
ggsave("../graphs/entailment-inference.pdf",height=3,width=3)


corr_inference = einfd %>%
  filter(VeridicalityGroup != "control") %>%
  summarize(Cor=cor(Prop,Mean,method="spearman"))
corr_inference

# contradictoriness diagnostic ----

# for contradictoriness entailment data, plot proportions against mean slider ratings
econtr_prop = d_contr_b %>%
  group_by(verb) %>%
  summarize(Prop = mean(nResponse), CILow = ci.low(nResponse), CIHigh = ci.high(nResponse)) %>%
  mutate(YMinP = Prop - CILow, YMaxP = Prop + CIHigh, verb = fct_reorder(as.factor(verb),Prop))
econtr_prop

econtr_means = d_contr_nb %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMinM = Mean - CILow, YMaxM = Mean + CIHigh) %>%
  mutate(verb=recode(verb, control_good = "non-contrd. C", control_bad = "contradictory C", annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform")) %>%
  select(-CILow,-CIHigh)
econtr_means

length(levels(econtr_prop$verb)) == length(levels(econtr_means$verb))
setdiff(levels(econtr_prop$verb), levels(econtr_means$verb))
setdiff(levels(econtr_means$verb), levels(econtr_prop$verb))
# join warning below appears to be due to factors having different level orders

econtrd = econtr_prop %>%
  left_join(econtr_means, by= "verb") %>%
  mutate(VeridicalityGroup = factor(case_when(
    verb %in% c("know", "discover", "reveal", "see", "be_annoyed") ~ "F", 
    verb %in% c("pretend", "think", "suggest", "say") ~ "NF", 
    verb %in% c("be_right","demonstrate") ~ "VNF",
    verb %in% c("contradictory C","non-contrd. C") ~ "control",
    TRUE ~ "V")))
econtrd
levels(econtrd$VeridicalityGroup)

ggplot(econtrd, aes(x=Mean, y=Prop, fill=VeridicalityGroup)) +
  geom_errorbar(aes(ymin=YMinP,ymax=YMaxP),width=0) +
  geom_errorbarh(aes(xmin=YMinM,xmax=YMaxM),width=0) +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  # scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  # scale_alpha(range = c(.3,1)) +
  scale_fill_manual(values=c("black","darkorchid","gray60","tomato1","dodgerblue")) +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  # guides(fill=FALSE) +
  # theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
  # color=cols$Colors)) +
  # theme(legend.position="top") +
  ylab("Proportion of 'yes (contrd.)' ratings") +
  xlab("Mean contradictoriness rating") +
  guides(fill=FALSE) +
  coord_fixed(ratio = 1) +
  xlim(c(0,1)) +
  ylim(c(0,1))
ggsave("../graphs/entailment-contradictory.pdf",height=3,width=3)

  
corr_contradict = econtrd %>%
  filter(VeridicalityGroup != "control") %>%
  summarize(Cor=cor(Prop,Mean,method="spearman"))
corr_contradict
