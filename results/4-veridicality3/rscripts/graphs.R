# Factives paper
# 4-veridicality3 (inference ratings, continuous task)
# graphs.R

# What is true: Dan knows that Sophia got a tattoo.
# Does it follow that Sophia got a tattoo?

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(tidybayes)
library(dichromat)
library(brms)
library(knitr)
theme_set(theme_bw())


# load clean data ----
cd <- read.csv(file="../data/cd.csv", header=TRUE, sep=",")
nrow(cd) #7252
summary(cd)

# write means and sd to separate file 
means = cd %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(verb,Mean,YMin,YMax)
means = as.data.frame(means)
names(means)

write.csv(means, file="../data/inference_means.csv",row.names=F,quote=F)

# target data (20 items per Turker)
names(cd)
table(cd$verb)

t <- subset(cd, cd$verb != "entailing C" & cd$verb != "non-ent. C")
t <- droplevels(t)
nrow(t) #5180 / 20 = 259 Turkers

# target data and entailing controls
te <- droplevels(subset(cd,cd$verb != "non-ent. C"))
nrow(te) #6216/24 = 259 Turkers

# how many ratings per predicate and per predicate-clause combination?
names(t)
tmp <- as.data.frame(table(t$verb))
min(tmp$Freq) #259 because 259 Turkers and each Turker saw each predicate once

table(t$content)
t$predicateClause <- interaction(t$verb,t$content)
tmp <- as.data.frame(table(t$predicateClause))
head(tmp)
min(tmp$Freq) #3
max(tmp$Freq) #23
mean(tmp$Freq) #13

# save target data
write.csv(t, "../data/t.csv")
nrow(t) #5180

# load target data for analysis
t <- read.csv(file="../data/t.csv", header=TRUE, sep=",")
head(t)
str(t$verb)

## plots ----

# plot of means with participant ratings (4-way distinction, factivity paper)
cd = cd %>%
  mutate(verb = fct_recode(verb,"entailing"="entailing C","non-entailing"="non-ent. C"))

means = cd %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(verb),Mean))
options(tibble.print_max = Inf)
means
levels(means$verb)

cd$verb <-factor(cd$verb, levels=levels(means$verb))

# define colors for the predicates
cols = data.frame(V=levels(cd$verb))
cols

levels(cols$V)
#cols$V <- factor(cols$V, levels = cols[order(as.character(means$verb)),]$V, ordered = TRUE)

cols$VeridicalityGroup = factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate"),"VNF",
                       ifelse(cols$V %in% c("non-entailing","entailing"),"MC","V")))),levels=c("F","V","VNF","NF","MC"))

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "MC","black","tomato1"))))

cols$Colors
levels(cols$VeridicalityGroup)

subjmeans = cd %>%
  group_by(verb,workerid) %>%
  summarize(Mean = mean(response)) 
subjmeans$verb <- factor(subjmeans$verb, levels = unique(levels(means$verb)))
levels(subjmeans$verb)

means$VeridicalityGroup = factor(
  ifelse(means$verb %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(means$verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(means$verb  %in% c("be_right","demonstrate"),"VNF",
                       ifelse(means$verb  %in% c("non-entailing","entailing"),"control","V")))),levels=rev(c("F","V","VNF","NF","control")))

levels(means$VeridicalityGroup)
levels(subjmeans$verb)
levels(means$verb)

ggplot(means, aes(x=verb, y=Mean, fill=VeridicalityGroup, shape=VeridicalityGroup)) +
  geom_point(shape=21,fill="gray70",data=subjmeans, alpha=.1, color="gray40") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  geom_point(stroke=.5,size=2.5,color="black") +
  scale_shape_manual(values=c( 21, 22, 25, 24, 23),labels=rev(c("factive","optionally\nfactive","veridical\nnon-factive","non-veridical\nnon-factive","control")),name="Predicate type") +
  scale_fill_manual(values=rev(c("darkorchid","tomato1","dodgerblue","gray60","black")),labels=rev(c("factive","optionally\nfactive","veridical\nnon-factive","non-veridical\nnon-factive","control")),name="Predicate type") +
  # guides(fill=FALSE, shape=F) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors),legend.position="bottom") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  ylab("Mean inference rating") +
  xlab("Predicate")
ggsave("../graphs/means-inference-by-predicate-variability.pdf",height=4.5,width=7)

# plot of means with participant ratings (3-way distinction, Tuebingen talk)
means = cd %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(verb),Mean))
options(tibble.print_max = Inf)
means
levels(means$verb)

cd$verb <-factor(cd$verb, levels=levels(means$verb))
levels(cd$verb)

# define colors for the predicates
cols = data.frame(V=levels(cd$verb))
cols

levels(cols$V)
cols$V <- factor(cols$V, levels = levels(means$verb))

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate"),"NF",
                       ifelse(cols$V %in% c("non-ent. C","entailing C"),"MC","V")))))

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "MC","black","tomato1"))))

cols$Colors
levels(cols$VeridicalityGroup)


subjmeans = cd %>%
  group_by(verb,workerid) %>%
  summarize(Mean = mean(response)) 
subjmeans$verb <- factor(subjmeans$verb, levels = unique(levels(means$verb)))
levels(subjmeans$verb)

means$VeridicalityGroup = as.factor(
  ifelse(means$verb %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(means$verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(means$verb  %in% c("be_right","demonstrate"),"NF",
                       ifelse(means$verb  %in% c("non-ent. C","entailing C"),"MC","V")))))

ggplot(means, aes(x=verb, y=Mean, fill=VeridicalityGroup)) +
  geom_point(shape=21,fill="gray60",data=subjmeans, alpha=.1, color="gray40") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  scale_fill_manual(values=c("darkorchid","black","gray60","tomato1")) +
  guides(fill=FALSE) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  ylab("Mean inference rating") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) 
ggsave("../graphs/means-inference-by-predicate-variability-3way.pdf",height=4,width=7)

# plot by-participant variability
cd$PresumedVerbType = as.factor(
  ifelse(cd$verb %in% c("know", "discover", "reveal", "see", "be_annoyed"), "factive", 
         ifelse(cd$verb %in% c("pretend", "think", "suggest", "say"), "plain non-factive", 
                ifelse(cd$verb %in% c("be_right","demonstrate"),"veridical non-factive",
                       ifelse(cd$verb %in% c("non-ent. C"),"non-entailing control",
                              ifelse(cd$verb %in% c("entailing C"),"entailing control",
                                     "projective non-factive"))))))
table(cd$PresumedVerbType,cd$verb)

means = cd %>%
  group_by(PresumedVerbType,workerid) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) 
nrow(means)
head(means)

factives = means %>%
  filter(PresumedVerbType == "factive") %>%
  mutate(Participant = fct_reorder(as.factor(workerid),Mean))

means$Participant = factor(x=as.character(means$workerid),levels=levels(factives$Participant))
cd$Participant = factor(x=as.character(cd$workerid),levels=levels(factives$Participant))

# get rid of error bars for all but the "factives"
means[means$PresumedVerbType != "factive",]$YMin = means[means$PresumedVerbType != "factive",]$Mean
means[means$PresumedVerbType != "factive",]$YMax = means[means$PresumedVerbType != "factive",]$Mean

ggplot(means, aes(x=Participant, y=Mean, fill=PresumedVerbType)) +
  geom_point(shape=21, data=cd, aes(y=response,fill=PresumedVerbType), alpha=.1) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  scale_fill_manual(values=c("black","darkorchid","green","gray60","tomato1","dodgerblue")) +
  ylab("Mean inference rating") +
  xlab("Participant") +
  theme(axis.text.x = element_text(size = 6, angle = 45, hjust=1,vjust=1 )) 
ggsave("../graphs/means-inference-by-participant.pdf",height=4,width=25)


# also used in MIT talk
# boxplot of inference strength by predicate, collapsing over complement clauses, ordered by mean
# including the two types of control stimuli
table(cd$verb)
means = cd %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(verb,Mean,YMin,YMax)
means = as.data.frame(means)
names(means)
View(means)

write.csv(means, file="../data/inference_means.csv",row.names=F,quote=F)

cd$verb <-factor(cd$verb, levels=means[order(means$Mean), "verb"])
table(cd$verb)

cols = data.frame(V=levels(cd$Verb))
cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate"),"VNF",
                       ifelse(cols$V %in% c("entailing C", "non-ent. C"),"control","V")))))

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "control","black","tomato1"))))

ggplot(cd, aes(x=verb, y=response)) + 
  geom_boxplot(width=0.2,position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Inference rating")+
  xlab("Predicate") +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors))
ggsave("../graphs/boxplot-inference.pdf",height=4,width=8)

# boxplot of inference strength by predicate, collapsing over complement clauses, ordered by median
means = cd %>%
  group_by(verb) %>%
  summarize(Median = median(response), Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(verb,Mean,Median,YMin,YMax)
means = as.data.frame(means)

cd$verb <-factor(cd$verb, levels=means[order(means$Median), "verb"])

cols = data.frame(V=levels(cd$Verb))
cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate"),"VNF",
                       ifelse(cols$V %in% c("entailing C", "non-ent. C"),"control","V")))))

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "control","black","tomato1"))))

ggplot(cd, aes(x=verb, y=response)) + 
  geom_boxplot(width=0.2,position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Inference rating")+
  xlab("Predicate (ordered by median)") +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors))
ggsave("../graphs/boxplot-inference-by-median.pdf",height=3.5,width=6.5)

# plot of inference rating by predicate and complement clauses
agr_verb = t %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)

agr_subj = t %>%
  group_by(content, verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)

ggplot(agr_verb, aes(x=verb, y=Mean)) + 
  geom_point(color="black", size=4) +
  geom_point(data=agr_subj, aes(color=content)) +
  theme(axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Inference rating")+
  xlab("Predicate")
ggsave("../graphs/inference-means-byitem.pdf",height=4,width=8)

# plot of inference rating by complement clause
agr_content = t %>%
  group_by(content) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)

ggplot(agr_content, aes(x=content, y=Mean)) + 
  geom_point(color="black", size=4) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax)) +
  theme(axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Inference rating")+
  xlab("Content")
ggsave("../graphs/inference-means-bycontent.pdf",height=8,width=6)

summary(t)

# plot inference rating by participant
variances = t %>%
  group_by(workerid) %>%
  summarise(VeriVar = var(response),
            VeriMean=mean(response),
            Veri.ci.low=ci.low(response),
            Veri.ci.high=ci.high(response))

variances = as.data.frame(variances)

ggplot(variances, aes(x=reorder(workerid,VeriMean),y=VeriMean)) +
  geom_point() +
  stat_summary(fun.y=mean, geom="point",color="gray70",  size=2,position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=VeriMean-Veri.ci.low,ymax=VeriMean+Veri.ci.high)) +
  theme(text = element_text(size=12),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  scale_y_continuous(expand = c(0, 0),limits = c(0,1.05),breaks = c(0.0,0.2,0.4,0.6,0.8,1.0)) +
  xlab("Participant") +
  ylab("Mean inference rating")
ggsave("../graphs/inference-subjmeans.pdf",height=3,width=6.5)

# plot inference rating by age (continous)
means = t %>%
  group_by(age) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) #%>%
#filter(!is.na(gender)) %>%
#droplevels()
dodge = position_dodge(.9)

ggplot(means, aes(y=Mean, x=age))+#, alpha=VeridicalityMean)) + 
  geom_point() +
  #geom_point(data=agr_subj, aes(color=content)) +
  # geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  geom_smooth(method='lm') +
  # scale_color_manual(name="Prior probability\nof eventuality", breaks=c("factH","factL"),labels=c("high", "low"), 
  # values=brewer.pal(2,"Dark2")) +
  scale_alpha(range = c(.3,1)) +
  ylab("Mean inference rating") +
  xlab("Participant age") +
  #facet_grid(fact_type~subjectGender) + 
  #ggtitle("Columns: attitude holder gender") +
  #ggtitle(title="Rows: speaker gender; Columns: participant gender") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors))#, legend.position = "top") 
ggsave("../graphs/age-continuuous-collapsed.pdf")

# veridicality rating by participant gender
means = t %>%
  group_by(gender) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  filter(!is.na(gender)) %>%
  droplevels()
dodge = position_dodge(.9)

means

ggplot(means, aes(y=Mean, x=gender))+#, alpha=VeridicalityMean)) + 
  geom_point() +
  #geom_point(data=agr_subj, aes(color=content)) +
  # geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  # scale_color_manual(name="Prior probability\nof eventuality", breaks=c("factH","factL"),labels=c("high", "low"), 
  # values=brewer.pal(2,"Dark2")) +
  scale_alpha(range = c(.3,1)) +
  ylab("Mean veridicality rating") +
  xlab("Participant gender") +
  #facet_grid(fact_type~subjectGender) + 
  #ggtitle("Columns: attitude holder gender") +
  #ggtitle(title="Rows: speaker gender; Columns: participant gender") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors))#, legend.position = "top") 
ggsave("../graphs/gender-collapsed.pdf")
