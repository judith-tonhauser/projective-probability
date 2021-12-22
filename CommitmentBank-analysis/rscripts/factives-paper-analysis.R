# Analysis file for the figure from the CommitmentBank
# included in the factives paper

# this file assumes that the following three directories occur at the same level:
# data (CommitmentBank-All.csv)
# graphs (figures created by this analysis script)
# rscripts (the location of this file as well as helpers.R)

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source("helpers.R")

# load packages
library(tidyverse)
library(dplyr)
library(xtable)

# b/w background in figures
theme_set(theme_bw())

#load the data
d = read.csv("../data/CommitmentBank-All.csv", header=T, comment.char="")
nrow(d) #11545
length(unique(d$uID)) #1200 unique discourses

#drop the nonepistemic modals
dataM = droplevels(subset(d, ModalType != "AB" & ModalType != "CI" & ModalType != "DE"))
length(unique(dataM$uID)) #982 unique discourses
table(dataM$ModalType)
nrow(dataM) #9599

# figure ----
names(dataM)

#aggregate data to create predicate + number of discourses information
aData = dataM %>%
  select(uID, Verb, Embedding, Target, Prompt, Answer) %>%
  group_by(uID, Verb, Embedding, Target, Prompt) %>%
  summarize(Mean = mean(Answer), low.ci = ci.low(Answer), high.ci = ci.high(Answer), SD = sd(Answer), C = n())
aData = as.data.frame(aData)
nrow(aData) #982 discourses

# paste predicate and number of discourses per predicate
verbNum = as.data.frame(xtabs(~ Verb, aData))
verbNum #number of discourses per verb
dataM = merge(dataM, verbNum, by="Verb")
head(dataM)
dataM$VerbNum = paste(dataM$Verb, paste0("(",dataM$Freq,")"), sep="  ")
table(dataM$VerbNum)

table(dataM$VerbNum)
dataM$VerbNum <- as.factor(dataM$VerbNum)

mean_proj = dataM %>%
  group_by(VerbNum) %>%
  summarize(Mean = mean(Answer), CILow = ci.low(Answer), CIHigh = ci.high(Answer)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, VerbNum = fct_reorder(as.factor(VerbNum),Mean))
mean_proj
nrow(mean_proj) #45 verbs

levels(mean_proj$VerbNum)

# code the predicate type of the 45 predicates
# abbreviations: factive (F), optionally factive (V), veridical nonfactive (VNF), nonveridical nonfactive (NF)

# Kiparsky & Kiparsky 1970 classification
# factive: significant, odd, tragic, exciting, relevant, matters, counts, makes sense, suffices, amuses, bothers, regret, aware, grasp,
# factive: comprehend, take into consideration/account, bear in mind, ignore, make clear, mind, forget, deplore, resent, care
# optionally factive: anticipate, acknowledge, suspect, report, emphasize, announce, admit, deduce and remember
# nonfactive: likely, sure, possible, true, false, seems, appears, happens, chances, turns out, suppose, assert, allege, assume, claim
# nonfactive: charge, maintain, believe, conclude, conjecture, intimate, deem, fancy, figure

# predicates of the 45 where a decision had to be made on how to classify (MW = Merriam Webster)
# expect (NF, MW lists anticipate as first synonym, though also hope)
# convince (NF, MW: argue, persuade)
# insist (NF, MW lists assert as a synonym)
# prove (V, MW lists demonstrate and establish as synonym)
# signal (NF, MW: argue, persuade)
# fear (F, MW: bother, worry)
# hear (V, as in our exps)
# decide (NF, MW: conclude, figure)
# foresee (V, MW lists anticipate as first synonym)
# show (V, parallel to prove)
# tell (V, MW: report, reveal)
# accept (V, closeness to admit)


#V: hear, prove, announce, show, admit, tell, show, announce, accept, convince, foresee
#VNF: none

cols = data.frame(V=levels(mean_proj$VerbNum))
cols

# code the VeridicalityGroup of the predicates
cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("forget  (13)", "learn  (6)", "notice  (32)", "understand  (7)", "recognize  (1)", "bother  (1)","realize  (29)",
                       "know  (122)","find  (17)","see  (12)","fear  (3)", "remember  (5)"), "F", 
         ifelse(cols$V %in% c("decide  (11)","signal  (2)","say  (67)","convince  (5)", "guess  (16)", "bet  (1)", "take  (1)","decide  (11)","hypothesize  (1)",
                              "figure  (1)","assume  (5)","imagine  (15)","mean  (50)","insist  (3)","demand  (2)","hope  (8)","feel  (29)",
                              "believe  (46)","think  (378)","suggest  (17)","pretend  (4)","seem  (2)","suppose  (5)","occur  (1)", "expect  (4)", "suspect  (18)"),"NF","V")))
cols
summary(cols)

#cols$V <- factor(cols$V, levels = cols[order(as.character(mean_proj$VerbNum)),]$V, ordered = TRUE)
#levels(cols$V)

cols = cols %>%
  mutate(V = fct_reorder(as.factor(V),mean_proj$Mean))
levels(cols$V)

# assign colors to the VeridicalityGroups
cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", "tomato1"))
head(cols$Colors)
cols
cols$Colors

mean_proj$VeridicalityGroup = as.factor(
  ifelse(mean_proj$VerbNum %in% c("forget  (13)", "learn  (6)", "notice  (32)", "understand  (7)", "recognize  (1)", "bother  (1)","realize  (29)",
                       "know  (122)","find  (17)","see  (12)","fear  (3)", "remember  (5)"), "F", 
         ifelse(mean_proj$VerbNum %in% c("decide  (11)","signal  (2)","say  (67)","convince  (5)", "guess  (16)", "bet  (1)", "take  (1)","decide  (11)","hypothesize  (1)",
                              "figure  (1)","assume  (5)","imagine  (15)","mean  (50)","insist  (3)","demand  (2)","hope  (8)","feel  (29)",
                              "believe  (46)","think  (378)","suggest  (17)","pretend  (4)","seem  (2)","suppose  (5)","occur  (1)", "expect  (4)", "suspect  (18)"),"NF","V")))
              

mean_proj = mean_proj %>%
  mutate(VeridicalityGroup = fct_relevel(VeridicalityGroup, "NF","V","F"))
levels(mean_proj$VeridicalityGroup)
#"NF" "V"  "F"

# factive: 23 (raute)
# optionally factive: 24 (triangle up)
# nonveridical nonfactive: 22 (square)

#means with confidence intervals -- used for SuB poster
mean_proj <- mutate(mean_proj,VerbNum = reorder(VerbNum, Mean, mean)) # put mean_proj in sorted order

# to bold-face the 10 predicates that occur in our experiments
bold <- c("pretend  (4)", "think  (378)", "see  (12)", "say  (67)", "prove  (2)", "hear  (9)","know  (122)","suggest  (17)", "admit  (5)", "announce  (3)")

levels(mean_proj$VerbNum)
levels(cols$V)
cols$Colors

# Figure 5 in color
ggplot(mean_proj, aes(x=VerbNum, y=Mean,fill=VeridicalityGroup,shape=VeridicalityGroup)) + 
  geom_point(stroke=.5,size=2.5,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),color="gray50",alpha=.5) +
  #geom_hline(yintercept=0, linetype="dashed", color = "red") +
  theme(panel.background = element_blank(), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(panel.grid.major.y = element_line(colour="grey90", size=0.5)) +
  theme(axis.text.x = element_text(face = ifelse(levels(mean_proj$VerbNum) %in% bold,"bold","plain"))) +
  scale_shape_manual(values=c(22,24,23),labels=c("nonveridical\nnonfactive","optionally\nfactive","factive"),name="Predicate type") +
  scale_fill_manual(values=c("gray60","tomato1","darkorchid"),labels=c("nonveridical\nnonfactive","optionally\nfactive","factive"),name="Predicate type") +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position="bottom") +
  ylab("Mean certainty rating") +
  xlab("Predicate (with number of discourses)")
ggsave("../graphs/means-projectivity-by-predicate-variability.pdf",height=5,width=9)
ggsave("../../papers/factives-paper/Language-figures/color/Figure5.pdf",height=5,width=9)


