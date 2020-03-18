# Prior probability work
# projection experiment with sensitivity to fact
# graphs.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(dichromat)
library(forcats)
library(RColorBrewer)
theme_set(theme_bw())

# load clean data for analysis ----
cd = read.csv("../data/cd.csv")
nrow(cd) #6916

# load prior means
pmeans = read.csv("../../1-prior/data/prior_means.csv")
pmeans$fact = gsub(".","",as.character(pmeans$fact),fixed=T)
pmeans
head(pmeans)

# change predicate names
cd = cd %>%
  mutate(verb=recode(verb, annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform"))

# merge contradictoriness means into cd
# cd = left_join(cd,vmeans,by=c("verb"))
cd = left_join(cd,pmeans,by=c("fact"))

head(cd)


# mean of non-projecting controls
table(cd$verb)
mean(cd[cd$verb == "control",]$response) #.19
ci.low(cd[cd$verb == "control",]$response) #.19
ci.high(cd[cd$verb == "control",]$response) #.01

# target data (20 items per Turker)
names(cd)
table(cd$verb)
t <- subset(cd, cd$verb != "control")
t <- droplevels(t)
nrow(t) #5320 / 20 items = 266 Turkers
table(t$verb,t$content)

names(t)
table(t$trigger_class)

head(t)

names(t)
table(t$itemType)
# mean rating across predicates by fact type
mean(t[t$itemType == "factL",]$response) #.32
mean(t[t$itemType == "factH",]$response) #.5

# plot mean projectivity by complement prior ----
means = t %>%
  group_by(verb, fact_type) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, Verb = fct_reorder(verb,Mean))
means
levels(means$Verb)

cols = data.frame(V=levels(means$Verb))
cols

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate"),"VNF",
                       ifelse(cols$V %in% c("MC"),"MC","PNF")))))

levels(cols$V)
cols

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "MC","black","tomato1"))))

cols
cols$V <- factor(cols$V, levels = cols[order(as.character(means$Verb)),]$V, ordered = TRUE)
levels(cols$V)

#cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "#009E73", 
#                      ifelse(cols$VeridicalityGroup == "NF", "black", 
#                             ifelse(cols$VeridicalityGroup == "VNF","black","black")))

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # c("#999999",

# xprag abstract
ggplot(means, aes(x=Verb, y=Mean, color=fact_type)) + 
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_y_continuous(limits = c(-0.05,1.05),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_color_manual(name="Prior probability of content", breaks=c("factH","factL"),labels=c("high", "low"), 
                     values=cbPalette) +
  scale_alpha(range = c(.3,1)) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position = c(0.2, 0.8)) +
  ylab("Mean certainty rating \n (higher is more projective)") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))
ggsave("../graphs/means-projectivity-by-predicate-and-facttype.pdf",height=3.2,width=6)

# xprag talk 
ggplot(means, aes(x=Verb, y=Mean, color=fact_type)) + 
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_y_continuous(limits = c(-0.05,1.05),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_color_manual(name="Prior probability of content", breaks=c("factH","factL"),labels=c("high", "low"), 
                     values=cbPalette) +
  scale_alpha(range = c(.3,1)) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position = c(0.2, 0.75)) +
  ylab("Mean certainty rating") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))
ggsave("../graphs/means-projectivity-by-predicate-and-facttype.pdf",height=3.2,width=6)

# xprag talk with main clause controls
table(cd$verb)

means = cd %>%
  group_by(verb, fact_type) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, Verb = fct_reorder(verb,Mean))
means
levels(means$Verb)

means[means$Verb == "control",]

# means_subj = cd %>%
#   group_by(verb,fact_type,workerid) %>%
#   summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
#   ungroup() %>%
#   mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, Verb = fct_reorder(verb,Mean))

cols = data.frame(V=levels(means$Verb))
cols

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate"),"VNF",
                       ifelse(cols$V %in% c("control"),"MC","PNF")))))

levels(cols$V)
cols

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "MC","black","tomato1"))))

cols
cols$V <- factor(cols$V, levels = cols[order(as.character(means$Verb)),]$V, ordered = TRUE)
levels(cols$V)

#cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "#009E73", 
#                      ifelse(cols$VeridicalityGroup == "NF", "black", 
#                             ifelse(cols$VeridicalityGroup == "VNF","black","black")))

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # c("#999999",


ggplot(means, aes(x=Verb, y=Mean, fill=fact_type)) + 
  # geom_point(data=means_subj,aes(color=fact_type),alpha=.3) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0) +
  geom_point(pch = 21, colour = "black", size = 3) +
  scale_y_continuous(limits = c(-0.05,1.05),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_fill_manual(name="Prior probability of content", breaks=c("factH","factL"),labels=c("high", "low"), 
                     values=cbPalette) +
  scale_color_manual(name="Prior probability of content", breaks=c("factH","factL"),labels=c("high", "low"), 
                    values=cbPalette) +  
  scale_alpha(range = c(.3,1)) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position = "top") +
  geom_errorbar(aes(x=1,ymin=means[means$Verb == "control",]$YMin,ymax=means[means$Verb == "control",]$YMax,width=.25),color="black") +
  geom_point(aes(x=1,y=means[means$Verb == "control",]$Mean), color="black",show.legend = FALSE ) +
  ylab("Mean certainty rating") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))
ggsave("../graphs/means-projectivity-by-predicate-and-facttype.pdf",height=4,width=6)

# figure in paper ----
table(cd$verb)

means = cd %>%
  group_by(verb, fact_type) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, Verb = fct_reorder(verb,Mean))
means
levels(means$fact_type)

means[means$Verb == "control",]$fact_type = "factH"

mean_control = cd %>%
  filter(verb == "control") %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)
mean_control

# means_subj = cd %>%
#   group_by(verb,fact_type,workerid) %>%
#   summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
#   ungroup() %>%
#   mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, Verb = fct_reorder(verb,Mean))

cols = data.frame(V=levels(means$Verb))
cols

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate"),"VNF",
                       ifelse(cols$V %in% c("control"),"MC","PNF")))))

levels(cols$V)
cols

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "MC","black","tomato1"))))

cols
cols$V <- factor(cols$V, levels = cols[order(as.character(means$Verb)),]$V, ordered = TRUE)
levels(cols$V)

#cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "#009E73", 
#                      ifelse(cols$VeridicalityGroup == "NF", "black", 
#                             ifelse(cols$VeridicalityGroup == "VNF","black","black")))

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # c("#999999",

table(means$fact_type)

ggplot(means, aes(x=Verb, y=Mean, color=fact_type, fill=fact_type,shape=fact_type)) + 
  # geom_point(data=means_subj,aes(color=fact_type),alpha=.3) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_point(stroke=.5,size=3,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position = "top") +
  scale_shape_manual(values=rev(c(25, 24)),labels=rev(c("lower","higher")),name="Prior probability of content") +
  scale_fill_manual(values=rev(c("#56B4E9","#E69F00")),labels=rev(c("lower","higher")),name="Prior probability of content") +
  scale_color_manual(name="Fact", breaks=c("higher","lower"),labels=c("higher", "lower"),values=cbPalette) +
  #manually change controls:
  geom_point(aes(x=1,y=means[means$Verb == "control",]$Mean), shape = 16,fill="black", color="black",size = 2) +
  geom_errorbar(aes(x=1,ymin=means[means$Verb == "control",]$YMin,ymax=means[means$Verb == "control",]$YMax,width=.25),color="black") +
  ylab("Mean certainty rating") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))
ggsave("../graphs/means-projectivity-by-predicate-and-facttype.pdf",height=4,width=7)


