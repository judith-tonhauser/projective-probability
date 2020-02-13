# Factives paper
# 6-veridicality2-binary (contradictoriness diagnostic, binary)
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
library(brms)
theme_set(theme_bw())


# load clean data for analysis ----
cd = read.csv("../data/cd.csv")
nrow(cd) #9884 (353 Turkers)
summary(cd)

# proportion of projective answers by predicate, including the main clause controls
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
                       ifelse(cols$V %in% c("contradictory C", "non-contrd. C"),"MC","V")))))

levels(cols$V)
cols$V <- factor(cols$V, levels = cols[order(as.character(prop$verb)),]$V, ordered = TRUE)

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "MC","black","tomato1"))))


# plot of proportions

prop$VeridicalityGroup = as.factor(
  ifelse(prop$verb %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(prop$verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(prop$verb  %in% c("be_right","demonstrate"),"VNF",
                       ifelse(prop$verb  %in% c("contradictory C", "non-contrd. C"),"MC","V")))))


ggplot(prop, aes(x=verb, y=Mean, fill=VeridicalityGroup, shape=VeridicalityGroup)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,color="black") +
  geom_point(stroke=.5,size=2.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  geom_jitter(data=cd,aes(y=nResponse),shape=15,color="gray40",alpha=.2,fill="black") +
  scale_shape_manual(values=rev(c(25, 24, 22, 21, 23)),labels=rev(c("veridical\nnon-factive","optionally\nfactive","non-veridical\nnon-factive","controls","factive")),name="Predicate type") +
  scale_fill_manual(values=rev(c("dodgerblue","tomato1","gray60","black","darkorchid")),labels=rev(c("veridical\nnon-factive","optionally\nfactive","non-veridical\nnon-factive","controls","factive")),name="Predicate type") +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
  ylab("Proportion of 'yes (contrd.)' ratings") +
  xlab("Predicate") 
ggsave("../graphs/proportion-by-predicate-variability-individual.pdf",height=4.5,width=7)

# plot Turker's 'no' responses for predicates up to "demonstrate" ----
table(cd$verb)

# subset by relevant 13 predicates
no <- droplevels(subset(cd,verb == "be_right" | verb == "discover" |verb == "prove" |
                          verb == "see" | verb == "know" |verb == "confirm" |
                          verb == "establish" | verb == "demonstrate" | verb == "admit" | 
                          verb == "confess" | verb == "acknowledge" | verb == "reveal" | verb == "be_annoyed"))

table(no$verb)
str(no$verb)

# calculate mean response, for ordering
prop = no %>%
  group_by(verb) %>%
  summarize(Mean = mean(nResponse)) %>%
  mutate(verb = fct_reorder(as.factor(verb),Mean))
prop
levels(prop$verb)


no <- merge(no,prop,by="verb")
head(no)
no$verb <- reorder(no$verb,no$Mean)
levels(no$verb)


length(unique(no[no$response == "No",]$workerid)) #196 different workers


# plot

no$VeridicalityGroup = as.factor(
  ifelse(no$verb %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(no$verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(no$verb  %in% c("be_right","demonstrate"),"VNF",
                       ifelse(no$verb  %in% c("entailing C", "non-ent. C"),"MC","V")))))

# define colors for the predicates
cols = data.frame(V=levels(no$verb))

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate"),"VNF",
                       ifelse(cols$V %in% c("entailing C", "non-ent. C"),"MC","V")))))

levels(cols$V)
cols$V <- factor(cols$V, levels = cols[order(as.character(prop$verb)),]$V, ordered = TRUE)

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "MC","black","tomato1"))))


# how many 'no' responses did the Turkers give
no.part = no %>%
  group_by(workerid) %>%
  summarise(Sum = sum(response == "No")) %>%
  print(n=100) %>%
  mutate(workerid = fct_reorder(as.factor(as.character(workerid)),Sum))

levels(no.part$workerid)

# reorder participants
no <- merge(no,no.part,by="workerid")
head(no)
no$workerid <- reorder(no$workerid,-no$Sum)

#no$workerid <- factor(no$workerid, levels = no[order(as.character(no.part$workerid)),]$workerid, ordered = TRUE)

# plot 'no' responses by predicate and Turker ID
levels(no$verb)

ggplot(no[no$response == "No",], aes(x=verb, y=workerid, fill=VeridicalityGroup)) +
  geom_point(shape=21,stroke=.5,size=2.5,color="black",show.legend = FALSE) +
  scale_fill_manual(values=c("darkorchid","tomato1","dodgerblue")) +
  ylab("Participant ID") +
  xlab("Predicate") +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors))
ggsave("../graphs/no-entailment-responses-by-pred-and-worker.pdf",height=3,width=4)

# JD CODE STARTS HERE
# TL;DR: all verbs are different from bad controls
cd <- read.csv(file="../data/cd.csv", header=TRUE, sep=",")

# Bayesian mixed effects regression to test whether ratings differ by predicate from good controls
cd$workerid = as.factor(as.character(cd$workerid))

# plotting slider ratings suggests we should use a zoib model
ggplot(cd, aes(x=response)) +
  geom_histogram(stat="count")

# exclude bad controls from analysis -- they're not relevant, right?
d = cd %>%
  # filter(verb != "control_bad") %>%
  droplevels() %>%
  # mutate(verb = fct_relevel(verb,"contradictory C")) %>%
  mutate(verb = fct_relevel(verb,"be_right")) %>%
  mutate(nResponse = ifelse(response == "Yes", 1, 0))

# JD removed the by-content intercepts and slopes for verb because it was taking TOO DAMN LONG and appeared to get stuck and not converge, and the reason for this is simply that the data are extreme and there is basically no item variability to speak of. barely any subject variability, too. had to 
m <- glmer(response ~ verb + (1|workerid), data=d, family="binomial")
# m <- glm(response ~ verb, data=d, family="binomial")
summary(m)

allm = allFit(m)

is.OK <- sapply(allm,is,"merMod")  ##  failed, others succeeded
allm.OK <- allm[is.OK]
allm.OK
lapply(allm.OK,function(x) x@optinfo$conv$lme4$messages)

summary(allm) # bobyqa exited ok, so we can interpret model above
summary(m)


m <- brm(nResponse~verb + (1|workerid), data=d, family=bernoulli())

# no neeed to run this multiple times
saveRDS(m,file="../data/bernoulli-model.rds")

summary(m) # see summary printed below
