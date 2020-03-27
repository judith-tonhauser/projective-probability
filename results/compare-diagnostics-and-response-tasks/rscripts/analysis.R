# Factives paper
# compare findings of binary and gradient response tasks
# compare findings of different entailment diagnostics

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


# projection: binary vs continuous ----

# for projectivity data, plot proportions against mean slider ratings
p_prop = d_proj_b %>%
  group_by(verb) %>%
  summarize(Prop = mean(nResponse), CILow = ci.low(nResponse), CIHigh = ci.high(nResponse)) %>%
  mutate(YMinP = Prop - CILow, YMaxP = Prop + CIHigh, verb = fct_reorder(as.factor(verb),Prop))
#View(p_prop)
levels(p_prop$verb)

p_means = d_proj_nb %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMinM = Mean - CILow, YMaxM = Mean + CIHigh, verb = fct_reorder(as.factor(verb),p_prop$Prop)) %>%
  select(-CILow,-CIHigh)
#View(p_means)
levels(p_means$verb)

pd = p_prop %>%
  left_join(p_means) %>%
  mutate(VeridicalityGroup = factor(case_when(
    verb %in% c("know", "discover", "reveal", "see", "be_annoyed") ~ "F", 
    verb %in% c("pretend", "think", "suggest", "say") ~ "NF", 
    verb %in% c("be_right","demonstrate") ~ "VNF",
    verb %in% c("MC") ~ "MC",
    TRUE ~ "V")))
#View(pd)
levels(pd$VeridicalityGroup)


pd$VeridicalityGroup <- factor(pd$VeridicalityGroup, levels =rev(c("F","V","VNF","NF","MC")))

pp <- ggplot(pd, aes(x=Mean, y=Prop, fill=VeridicalityGroup,shape=VeridicalityGroup)) +
  geom_errorbar(aes(ymin=YMinP,ymax=YMaxP),width=0) +
  geom_errorbarh(aes(xmin=YMinM,xmax=YMaxM),width=0) +
  geom_point(stroke=.5,size=2.5,color="black") +
  # scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  # scale_alpha(range = c(.3,1)) +
  #scale_fill_manual(values=c("darkorchid","black","gray60","tomato1","dodgerblue")) +
  scale_shape_manual(values=rev(c(25, 24, 22, 21, 23)),
                     labels=rev(c("veridical\nnon-factive","optionally\nfactive","non-veridical\nnon-factive","controls","factive")),name="Predicate type") +
  scale_fill_manual(values=rev(c("dodgerblue","tomato1","gray60","black","darkorchid")),
                    labels=rev(c("veridical\nnon-factive","optionally\nfactive","non-veridical\nnon-factive","controls","factive")),name="Predicate type") +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  ylab("Proportion of 'yes (certain)' ratings") +
  xlab("Mean certainty rating") +
  theme(legend.position = "bottom") +
  coord_fixed(ratio = 1) +
  xlim(c(0,1)) +
  ylim(c(0,1))
pp
ggsave("../graphs/projectivity.pdf",height=3,width=3)

corr_projectivity = pd %>%
  filter(verb != "MC") %>%
  summarize(Cor=cor(Prop,Mean,method="spearman"))
corr_projectivity #.983

# inference diagnostic: binary versus continuous ----

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

# factive: 23 (raute)
# optionally factive (V): 24 (triangle up)
# veridical non-factive: 25 (triangle down)
# non-veridical non-factive: 22 (square)
# MC: 21 (circle)

pe <- ggplot(einfd, aes(x=Mean, y=Prop, fill=VeridicalityGroup,shape=VeridicalityGroup)) +
  geom_errorbar(aes(ymin=YMinP,ymax=YMaxP),width=0) +
  geom_errorbarh(aes(xmin=YMinM,xmax=YMaxM),width=0) +
  geom_point(stroke=.5,size=2.5,color="black") +
  # scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  # scale_alpha(range = c(.3,1)) +
  scale_shape_manual(values=rev(c(21, 25, 24, 22, 23))) +
  scale_fill_manual(values=rev(c("black","dodgerblue","tomato1","gray60","darkorchid"))) +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  # guides(fill=FALSE) +
  # theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
  # color=cols$Colors)) +
  # theme(legend.position="top") +
  ylab("Proportion of 'yes (def. follows)' ratings") +
  xlab("Mean inference rating") +
  theme(legend.position = "none") +
  coord_fixed(ratio = 1) +
  xlim(c(0,1)) +
  ylim(c(0,1))
ggsave("../graphs/entailment-inference.pdf",height=3,width=3)


corr_inference = einfd %>%
  filter(VeridicalityGroup != "control") %>%
  summarize(Cor=cor(Prop,Mean,method="spearman"))
corr_inference #.989

# contradictoriness diagnostic: binary versus continuous ----

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

# factive: 23 (raute)
# optionally factive (V): 24 (triangle up)
# veridical non-factive: 25 (triangle down)
# non-veridical non-factive: 22 (square)
# MC: 21 (circle)

pc <- ggplot(econtrd, aes(x=Mean, y=Prop, fill=VeridicalityGroup,shape=VeridicalityGroup)) +
  geom_errorbar(aes(ymin=YMinP,ymax=YMaxP),width=0) +
  geom_errorbarh(aes(xmin=YMinM,xmax=YMaxM),width=0) +
  geom_point(stroke=.5,size=2.5,color="black") +
  # scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  # scale_alpha(range = c(.3,1)) +
  scale_shape_manual(values=rev(c(21, 25, 24, 22, 23))) +
  scale_fill_manual(values=rev(c("black","dodgerblue","tomato1","gray60","darkorchid"))) +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  # guides(fill=FALSE) +
  # theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
  # color=cols$Colors)) +
  # theme(legend.position="top") +
  ylab("Proportion of 'yes (contrd.)' ratings") +
  xlab("Mean contradictoriness rating") +
  theme(legend.position = "none") +
  coord_fixed(ratio = 1) +
  xlim(c(0,1)) +
  ylim(c(0,1))
ggsave("../graphs/entailment-contradictory.pdf",height=3,width=3)

  
corr_contradict = econtrd %>%
  filter(VeridicalityGroup != "control") %>%
  summarize(Cor=cor(Prop,Mean,method="spearman"))
corr_contradict #.985

# create joint binary/continuous comparison plot for appendix ----

prow <- plot_grid( pp + theme(legend.position="none"),
                   pe + theme(legend.position="none"),
                   pc + theme(legend.position="none"),
                   align = 'vh',
                   #labels = c("A", "B", "C"),
                   hjust = -1,
                   nrow = 1
)

legend <- get_legend(pp + guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom"))

p <- plot_grid(prow, legend, ncol = 1, rel_heights = c(1, .1))
ggsave("../graphs/joint-comparison-plot.pdf",height=4,width=9)

# compare the two gradient (non-binary = nb) entailment diagnostics ----
table(d_inf_nb$verb)
table(d_contr_nb$verb)

einf_means = d_inf_nb %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMinM = Mean - CILow, YMaxM = Mean + CIHigh) %>%
  mutate(verb=recode(verb, "non-ent. C" = "non-entailing", "entailing C" = "entailing")) %>%
  select(-CILow,-CIHigh)
einf_means 

einf_means2 = d_contr_nb %>%
  group_by(verb) %>%
  summarize(Mean2 = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMinM2 = Mean2 - CILow, YMaxM2 = Mean2 + CIHigh) %>%
  mutate(verb=recode(verb, "non-contrd. C" = "non-entailing", "contradictory C" = "entailing")) %>%
  select(-CILow,-CIHigh)
einf_means2 

e_inf_contr_nb = einf_means %>%
  left_join(einf_means2, by = "verb") %>%
  mutate(VeridicalityGroup = factor(case_when(
    verb %in% c("know", "discover", "reveal", "see", "be_annoyed") ~ "F", 
    verb %in% c("pretend", "think", "suggest", "say") ~ "NF", 
    verb %in% c("be_right","demonstrate") ~ "VNF",
    verb %in% c("entailing","non-entailing") ~ "control",
    TRUE ~ "V")))
e_inf_contr_nb
levels(e_inf_contr_nb$VeridicalityGroup)

ggplot(e_inf_contr_nb, aes(x=Mean, y=Mean2, fill=VeridicalityGroup)) +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  geom_errorbar(aes(ymin=YMinM,ymax=YMaxM),width=0) +
  geom_errorbarh(aes(xmin=YMinM2,xmax=YMaxM2),width=0) +
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
ggsave("../graphs/comparison-of-entailment-diagnostics.pdf",height=3,width=3)

# Spearman rank correlation on targets and controls: gradient entailment diagnostics
corr_inf_contr_nb = e_inf_contr_nb %>%
  #filter(VeridicalityGroup != "control") %>%
  summarize(Cor=cor(Mean,Mean2,method="spearman"))
corr_inf_contr #.953

# compare the two categorical (binary = b) entailment diagnostics ----
table(d_inf_b$verb)
table(d_contr_b$verb)

einf_means = d_inf_b %>%
  group_by(verb) %>%
  summarize(Prop = mean(nResponse), CILow = ci.low(nResponse), CIHigh = ci.high(nResponse)) %>%
  mutate(YMinM = Prop - CILow, YMaxM = Prop + CIHigh) %>%
  mutate(verb=recode(verb, "non-ent. C" = "non-entailing", "entailing C" = "entailing")) %>%
  select(-CILow,-CIHigh)
einf_means 

einf_means2 = d_contr_b %>%
  group_by(verb) %>%
  summarize(Prop2 = mean(nResponse), CILow = ci.low(nResponse), CIHigh = ci.high(nResponse)) %>%
  mutate(YMinM2 = Prop2 - CILow, YMaxM2 = Prop2 + CIHigh) %>%
  mutate(verb=recode(verb, "non-contrd. C" = "non-entailing", "contradictory C" = "entailing")) %>%
  select(-CILow,-CIHigh)
einf_means2 

e_inf_contr_b = einf_means %>%
  left_join(einf_means2, by = "verb") %>%
  mutate(VeridicalityGroup = factor(case_when(
    verb %in% c("know", "discover", "reveal", "see", "be_annoyed") ~ "F", 
    verb %in% c("pretend", "think", "suggest", "say") ~ "NF", 
    verb %in% c("be_right","demonstrate") ~ "VNF",
    verb %in% c("entailing","non-entailing") ~ "control",
    TRUE ~ "V")))
e_inf_contr_b
levels(e_inf_contr_b$VeridicalityGroup)

ggplot(e_inf_contr_b, aes(x=Prop, y=Prop2, fill=VeridicalityGroup)) +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  geom_errorbar(aes(ymin=YMinM,ymax=YMaxM),width=0) +
  geom_errorbarh(aes(xmin=YMinM2,xmax=YMaxM2),width=0) +
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
ggsave("../graphs/comparison-of-entailment-diagnostics.pdf",height=3,width=3)

# Spearman rank correlation on targets and controls: binary entailment diagnostics
corr_inf_contr_b = e_inf_contr_b %>%
  #filter(VeridicalityGroup != "control") %>%
  summarize(Cor=cor(Prop,Prop2,method="spearman"))
corr_inf_contr_b #.934

